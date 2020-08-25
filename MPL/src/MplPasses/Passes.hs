{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Passes where

import Optics

import qualified MplPasses.Parser.BnfcParse as B
import MplPasses.Parser.Parse
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R

import MplPasses.TypeChecker.TypeCheck
import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.KindCheck 

import MplPasses.Env

import MplAST.MplCore

import MplUtil.UniqueSupply
import Control.Monad

import Data.Word
import Data.Void
import Data.List

import Debug.Trace

data MplPassesErrors =
    MplBnfcErrors B.BnfcErrors
    | MplParseErrors ParseErrors
    | MplRenameErrors RenameErrors
    | MplTypeCheckErrors TypeCheckErrors
  deriving Show

$(makeClassyPrisms ''MplPassesErrors)

instance B.AsBnfcErrors MplPassesErrors where
    _BnfcErrors = _MplBnfcErrors

instance AsParseErrors MplPassesErrors where
    _ParseErrors = _MplParseErrors

instance AsRenameErrors MplPassesErrors where 
    _RenameErrors = _MplRenameErrors

instance AsTypeCheckErrors MplPassesErrors where
    _TypeCheckErrors = _MplTypeCheckErrors 

instance AsKindCheckErrors MplPassesErrors where
    _KindCheckErrors = _MplTypeCheckErrors  % _TypeCheckKindErrors 


data MplPassesEnv = MplPassesEnv {
    mplPassesEnvUniqueSupply :: UniqueSupply
    , mplPassesTopLevel :: TopLevel
}

mplPassesEnv :: IO MplPassesEnv 
mplPassesEnv = do
    uniqsup <- initUniqueSupply 0
    return $ MplPassesEnv uniqsup TopLevel

runPasses :: 
    MplPassesEnv -> 
    String -> 
    Either [MplPassesErrors] _
runPasses MplPassesEnv{mplPassesEnvUniqueSupply = supply, mplPassesTopLevel = toplvl} = 
    runTypeCheck' (toplvl, rs) 
    <=< runRename' (toplvl, ls) 
    <=< runParse' 
    <=< B.runBnfc
    -- in runRename' (TopLevel, ls, rsymtab) <=< runParse' <=< B.runBnfc
  where
    (ls, rs) = split supply

runPassesTester ::
    String -> 
    IO ()
runPassesTester str = do
    env <- mplPassesEnv
    case runPasses env str of
        Right v -> putStrLn $ pprint v
        Left v -> putStrLn $ intercalate "\n" $ map show v
