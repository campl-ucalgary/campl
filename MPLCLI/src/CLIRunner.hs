{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module CLIRunner where

-- Front end
import qualified MplPasses.Passes as Passes
import qualified MplPasses.Parser.BnfcParse as B
import qualified MplPasses.PassesErrors as PassesErrors
import qualified MplPasses.PassesErrorsPprint as PassesErrors
import qualified MplAST.MplCore as MplCore
import MplUtil.UniqueSupply

import Optics

import Data.Proxy
import Data.Typeable
import Data.List
import Data.Maybe

import Control.Monad.Except

import System.Environment
import Control.Exception
import System.IO

import Config

cliRunner :: IO ()
cliRunner = do
    (flags, inpfile) <- getOpts
    Passes.MplPassesEnv
        { Passes.mplPassesEnvUniqueSupply = supply
        , Passes.mplPassesTopLevel = toplvl } <- Passes.mplPassesEnv
    let ~(s0:s1:s2:_) = uniqueSupplies supply
    inp <- readFile inpfile

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

    -- putStrLn $ MplCore.pprintParsed  lambalifted


    undefined

newtype FrontEndPassException x = 
    FrontEndPassException String
instance Show (FrontEndPassException MplCore.MplParsed) where
    show (FrontEndPassException str) = concat [ "parse error:" , "\n" , str ]

instance Show (FrontEndPassException MplCore.MplRenamed) where
    show (FrontEndPassException str) = concat [ "rename error:" , "\n" , str ]

instance Show (FrontEndPassException MplCore.MplTypeChecked) where
    show (FrontEndPassException str) = concat [ "type check / semantic error:" , "\n" , str ]

instance Show (FrontEndPassException MplCore.MplPatternCompiled) where
    show (FrontEndPassException str) = concat [ "pattern compilation error:" , "\n" , str ]

instance (Show (FrontEndPassException x), Typeable (FrontEndPassException x)) => 
    Exception (FrontEndPassException x) where

cliRunFrontEndPass ::
    forall x a.
    ( Show (FrontEndPassException x)
    , Typeable (FrontEndPassException x)
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
        $ (FrontEndPassException :: String -> FrontEndPassException x)
        $ show
        $ PassesErrors.pprintMplPassesErrors errs
    


