{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module MplCliRunner.Runner where

-- Front end
import qualified MplPasses.Passes as Passes
import qualified MplPasses.Parser.BnfcParse as B
import qualified MplPasses.PassesErrors as PassesErrors
import qualified MplPasses.PassesErrorsPprint as PassesErrors
import qualified MplAST.MplCore as MplCore
import MplUtil.UniqueSupply

-- Assembler
import qualified MplAsmAST.MplAsmCore as Asm
import qualified MplAsmAST.MplAsmPrinter as Asm
import qualified MplAsmAST.MplAsmProg as Asm
import qualified MplAsmPasses.Compile.Compile as Asm
import qualified MplAsmPasses.Compile.CompileErrors as Asm
import qualified MplAsmPasses.FromLambdaLifted.FromLambdaLifted as Asm
import qualified MplAsmPasses.FromLambdaLifted.FromLambdaLiftedErrors as Asm
import qualified MplAsmPasses.PassesErrorsPprint as Asm


-- from cli
import MplCliRunner.Flags
import MplCliRunner.Stack

-- abstract machine
import qualified MplMach.MplMachTypes as MplMach
import qualified MplMach.MplMachRunner as MplMach
import qualified MplMach.MplMachStack as MplMach

import Optics

import Data.Proxy
import Data.Typeable
import Data.List
import Data.Maybe
import Data.Coerce

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import System.IO
import System.Environment
import Control.Exception
import Control.Monad

import qualified Data.Bifunctor as Bifunctor
import Data.Traversable
import Data.Foldable

-- for devugging
import qualified Text.Show.Pretty as PrettyShow 


cliRunner :: [String] -> IO ()
cliRunner args = getOpts args >>= execMplCli cliRunPipeline . review _MplCliEnv

-- | runs the front end passes
cliRunPipeline :: MplCli ()
cliRunPipeline = 
    join (gviews mplCliInpFile (liftIO . readFile))
        >>= cliRunPipelineInputProg 


-- | really runs the front end passes, but accepts the entire input program
-- as input (doesn't read the file) -- this is useful for the bencmakring.
cliRunPipelineInputProg :: 
    -- | input program
    String -> 
    -- | all done.
    MplCli ()
cliRunPipelineInputProg inp = do
    flags <- gview mplCliFlags 

    Passes.MplPassesEnv
        { Passes.mplPassesEnvUniqueSupply = supply
        , Passes.mplPassesTopLevel = toplvl } <- liftIO Passes.mplPassesEnv
    let ~(s0:s1:s2:s3:_) = uniqueSupplies supply

    -- parsed
    parsed <- liftEither 
        $ Bifunctor.first 
            ( ParsedException 
            . show 
            . (PassesErrors.pprintMplPassesErrors :: [PassesErrors.MplPassesErrors] -> PassesErrors.MplDoc) 
            )
        $ Passes.runParse' <=< B.runBnfc 
        $ inp

    for_ flags $ \case
        Dump opt@Parsed dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, "output"]
                , MplCore.pprint (Proxy :: Proxy MplCore.MplParsed) parsed
                ]
        _ -> return ()

    -- renamer
    renamed <- liftEither 
        $ Bifunctor.first 
            ( RenamedException 
            . show 
            . (PassesErrors.pprintMplPassesErrors :: [PassesErrors.MplPassesErrors] -> PassesErrors.MplDoc) 
            )
        $ Passes.runRename' (toplvl, s0)
        $ parsed

    for_ flags $ \case
        Dump opt@Renamed dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, "output"]
                , MplCore.pprint (Proxy :: Proxy MplCore.MplRenamed) renamed
                ]
        _ -> return ()

    -- type checker
    typechecked <- liftEither 
        $ Bifunctor.first 
            ( TypeCheckedException 
            . show 
            . (PassesErrors.pprintMplPassesErrors 
                :: [PassesErrors.MplPassesErrors] -> PassesErrors.MplDoc) 
            )
        $ Passes.runTypeCheck' (toplvl, s1)
        $ renamed

    for_ flags $ \case
        Dump opt@TypeChecked dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, "output"]
                , MplCore.pprint (Proxy :: Proxy MplCore.MplTypeChecked) typechecked
                ]
        _ -> return ()

    -- pattern compilation
    patterncompiled <- liftEither 
        $ Bifunctor.first 
            ( PatternCompiledException 
            . show 
            . (PassesErrors.pprintMplPassesErrors 
                :: [PassesErrors.MplPassesErrors] -> PassesErrors.MplDoc) 
            )
        $ Passes.runPatternCompile' (toplvl, s2)
        $ typechecked

    for_ flags $ \case
        Dump opt@PatternCompiled dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, "output"]
                , MplCore.pprint (Proxy :: Proxy MplCore.MplPatternCompiled) patterncompiled
                ]
        _ -> return ()

    -- lambda lifting
    let lambdalifted = Passes.runLambdaLiftProg patterncompiled
    for_ flags $ \case
        Dump opt@LambdaLifted dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, "output"]
                , MplCore.pprint (Proxy :: Proxy MplCore.MplLambdaLifted) lambdalifted
                ]
        _ -> return ()

    -- to the assembly format
    assembled <- liftEither
        $ Bifunctor.first 
            ( AssembledException 
            . show 
            . (Asm.pprintFromLambdaLiftedErrors :: [Asm.FromLambdaLiftedError] -> Asm.MplAsmDoc) 
            )
        $ Asm.mplAssembleProg s3 lambdalifted

    for_ flags $ \case
        Dump opt@Assembled dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, "output"]
                , Asm.pprint assembled
                ]
        _ -> return ()

    -- transform it to the initial machine state
    (supercombs, mainf) <- liftEither 
        $ Bifunctor.first 
            ( AssembledException 
            . show
            . Asm.pprintCompileErrors 
            )
        $ Asm.mplAsmProgToInitMachState assembled


    {-
    liftIO $ do
        pPrint supercombs
        pPrint mainf
    -}
    for_ flags $ \case
        Dump opt@AssembledAst dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, "output"]
                , intercalate " " ["-- super combinators"]
                , PrettyShow.ppShow supercombs
                , intercalate " " ["-- main function"]
                , PrettyShow.ppShow mainf
                ]
        _ -> return ()

    -- when the user actulllay wants to run the machien, actually run the machine
    when (has (folded % _RunMplMach) flags) $ liftIO $ do
        env <- MplMach.initMplMachEnv supercombs
        MplMach.mplMachRunnner env mainf

    return ()
