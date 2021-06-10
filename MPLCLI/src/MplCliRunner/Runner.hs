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

-- from cli
import MplCliRunner.Flags
import MplCliRunner.Stack
import MplCliRunner.LambdaLiftedToAsm

import qualified AMPL as AMPL

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
import Text.Show.Pretty


cliRunner :: IO ()
cliRunner = do
    getOpts >>= execMplCli cliRunPipeline . review _MplCliEnv

-- | runs the front end passes
cliRunPipeline :: MplCli ()
cliRunPipeline = do
    flags <- gview mplCliFlags 

    Passes.MplPassesEnv
        { Passes.mplPassesEnvUniqueSupply = supply
        , Passes.mplPassesTopLevel = toplvl } <- liftIO Passes.mplPassesEnv
    let ~(s0:s1:s2:s3:_) = uniqueSupplies supply
    inp <- join $ gviews mplCliInpFile (liftIO . readFile)

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
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, " output"]
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
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, " output"]
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
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, " output"]
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
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, " output"]
                , MplCore.pprint (Proxy :: Proxy MplCore.MplPatternCompiled) patterncompiled
                ]
        _ -> return ()

    -- lambda lifting
    let lambdalifted = Passes.runLambdaLiftProg patterncompiled
    for_ flags $ \case
        Dump opt@LambdaLifted dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, " output"]
                , MplCore.pprint (Proxy :: Proxy MplCore.MplLambdaLifted) lambdalifted
                ]
        _ -> return ()

    -- to the assembly format
    let assembled = mplAssembleProg s3 lambdalifted
    for_ flags $ \case
        Dump opt@Assembled dpoutput -> liftIO 
            $ dumpOutput dpoutput 
            $ intercalate "\n" 
                [ intercalate " " ["-- dumped", dumpOptShowOptions opt, " output"]
                , Asm.pprint assembled
                ]
        _ -> return ()

    -- transform it to the initial machine state
    initmachst <- liftEither 
        $ Bifunctor.first 
            ( AssembledException 
            . show
            . Asm.pprintCompileErrors 
            )
        $ Asm.mplAsmProgToInitMachState assembled

    liftIO $ pPrint initmachst
    -- actually run the machine
    -- liftIO $ AMPL.execAmplMachWithDefaultsFromInitAMPLMachState "5000" initmachst

    return ()


