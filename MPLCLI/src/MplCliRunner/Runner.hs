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

-- from cli
import MplCliRunner.Flags
import MplCliRunner.Stack
import MplCliRunner.LambdaLiftedToAsm


import Optics

import Data.Proxy
import Data.Typeable
import Data.List
import Data.Maybe

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


cliRunner :: IO ()
cliRunner = do
    getOpts >>= execMplCli cliFrontEndPasses . review _MplCliEnv

    {-
    lambalifted <- 
            cliRunFrontEndPass (Proxy :: Proxy MplCore.MplPatternCompiled) 
                [(LambdaLifted, dump) | Dump LambdaLifted dump <- flags]
                .  (Right . Passes.runLambdaLiftProg)
        <=< cliRunFrontEndPass (Proxy :: Proxy MplCore.MplPatternCompiled) 
            [(PatternCompiled, dump) | Dump PatternCompiled dump <- flags]
            . Passes.runPatternCompile' (toplvl, s2)
        <=< cliRunFrontEndPass (Proxy :: Proxy MplCore.MplTypeChecked) 
            [(TypeChecked, dump) | Dump TypeChecked dump <- flags]
            . Passes.runTypeCheck' (toplvl, s1)
        <=< cliRunFrontEndPass (Proxy :: Proxy MplCore.MplRenamed) 
            [(Renamed, dump) | Dump Renamed dump <- flags]
            . Passes.runRename' (toplvl, s0)
        <=< cliRunFrontEndPass (Proxy :: Proxy MplCore.MplParsed) 
            [(Parsed, dump) | Dump Parsed dump <- flags]
            . (Passes.runParse' <=< B.runBnfc) 
        $ inp
    -}
    undefined

    -- putStrLn $ MplCore.pprintParsed  lambalifted


    undefined

cliFrontEndPasses :: 
    MplCli [MplCore.MplDefn MplCore.MplLambdaLifted] 
cliFrontEndPasses = do
    flags <- gview mplCliFlags 

    Passes.MplPassesEnv
        { Passes.mplPassesEnvUniqueSupply = supply
        , Passes.mplPassesTopLevel = toplvl } <- liftIO Passes.mplPassesEnv
    let ~(s0:s1:s2:_) = uniqueSupplies supply
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
            ( ParsedException 
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
            ( ParsedException 
            . show 
            . (PassesErrors.pprintMplPassesErrors :: [PassesErrors.MplPassesErrors] -> PassesErrors.MplDoc) 
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
            ( ParsedException 
            . show 
            . (PassesErrors.pprintMplPassesErrors :: [PassesErrors.MplPassesErrors] -> PassesErrors.MplDoc) 
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
 
    undefined
    


{-
cliRunFrontEndPass ::
    forall x a.
    ( Show (FrontEndException x)
    , Typeable (FrontEndException x)
    , MplCore.PPrint a x
    ) => 
    Proxy x -> 
    [(DumpOpt, DumpOutput)] ->
    Either [PassesErrors.MplPassesErrors] a -> 
    IO a
cliRunFrontEndPass proxy ddumps res = case res of
    Right a -> do
        forM_ ddumps $ \dump -> do
            let str = intercalate "\n"
                    [ "-- dumped " ++ dumpOptShowOptions (fst dump) ++ " output"
                    , MplCore.pprint (Proxy :: Proxy x) a
                    ]
            case snd dump of
                DumpToFile fp -> writeFile fp str
                DumpToStdOut  -> putStrLn str
        return a
    Left errs -> throwIO 
        $ (FrontEndException :: String -> FrontEndException x)
        $ show
        $ PassesErrors.pprintMplPassesErrors errs
    -}

{-
data MplAsmCom x
    = CAssign (XCAssign x) (IdP x) (MplAsmCom x)
    | CLoad (XCLoad x) (IdP x)
    | CRet (XCRet x)
    | CCall (XCCall x) (IdP x) [IdP x]
    | CInt (XCInt x) Int
    | CChar (XCChar x) Char
    | CBool (XCBool x) Bool

    -- and, or other bool operators

    | CEqInt (XCEqInt x)
    | CLeqInt (XCLeqInt x)
    | CEqChar (XCEqChar x)
    | CLeqChar (XCLeqChar x)

    | CAdd (XCAdd x)
    | CSub (XCSub x)
    | CMul (XCMul x)
    -- | data type, handle, arguments
    | CConstructor (XCConstructor x) (TypeAndSpec x) [IdP x]
    -- | data type, handle, arguments, expression to destruct
    | CDestructor (XCDestructor x) (TypeAndSpec x) [IdP x] (IdP x)

    | CCase (XCCase x) (IdP x) [LabelledMplSeqComs x]
    | CRecord (XCRecord x) [LabelledMplSeqComs x]
    | CIf (XCIf x) (IdP x) (MplAsmComs x) (MplAsmComs x)

    | CTuple (XCTuple x) [IdP x]
    | CProj (XCProj x) Word (IdP x)


    -- | Concurrent command. @get a on channel@
    | CGet (XCGet x) (IdP x) (IdP x)
    -- | Concurrent command. @put a on channel@
    | CPut (XCPut x) (IdP x) (IdP x)
    | CHPut (XCHPut x) (TypeAndSpec x) (IdP x)
    | CHCase (XCHCase x) (IdP x) [LabelledMplConcComs x]
    | CSplit (XCSplit x) (IdP x) (IdP x, IdP x)
    | CFork (XCFork x) (IdP x) (ForkPhrase x, ForkPhrase x)
    | CPlug (XCPlug x) [IdP x] (PlugPhrase x, PlugPhrase x)
    | CRun (XCRun x) (IdP x) ([IdP x], [IdP x], [IdP x])
    | CId (XCId x) (IdP x, IdP x)
    | CRace (XCRace x) [RacePhrase x]
    | CClose (XCRace x) (IdP x)
    | CHalt (XCRace x) (IdP x)

type MplAsmComs x = [MplAsmCom x]
type LabelledMplSeqComs x = (TypeAndSpec x, [IdP x], MplAsmComs x) 
type LabelledMplConcComs x = (TypeAndSpec x, MplAsmComs x) 
-- | @a with a0,a1 : coms @
type ForkPhrase x = (IdP x, [IdP x], MplAsmComs x)
type PlugPhrase x = ([IdP x], MplAsmComs x)
type RacePhrase x = (IdP x, MplAsmComs x)

$(makePrisms ''MplAsmCom)
$(makeBaseFunctor ''MplAsmCom)
 -}

