{-# LANGUAGE TemplateHaskell #-}
module MplPasses.Passes where

import Optics

import qualified MplPasses.Parser.BnfcParse as B
import MplPasses.Parser.Parse
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R
import MplPasses.Env

import MplAST.MplCore

import MplUtil.UniqueSupply
import Control.Monad

import Data.Word
import Data.Void

data MplPassesErrors =
    MplBnfcErrors B.BnfcErrors
    | MplParseErrors ParseErrors
    | MplRenameErrors RenameErrors
  deriving Show

$(makeClassyPrisms ''MplPassesErrors)

instance B.AsBnfcErrors MplPassesErrors where
    _BnfcErrors = _MplBnfcErrors

instance AsParseErrors MplPassesErrors where
    _ParseErrors = _MplParseErrors

instance AsRenameErrors MplPassesErrors where 
    _RenameErrors = _MplRenameErrors


data MplPassesEnv = MplPassesEnv {
    mplPassesEnvUniqueSupply :: UniqueSupply
    , mplPassesContext :: R.SymTab
}

mplPassesEnv :: IO MplPassesEnv 
mplPassesEnv = do
    uniqsup <- initUniqueSupply 0
    return $ MplPassesEnv uniqsup []

runPasses :: 
    MplPassesEnv -> 
    String -> 
    Either [MplPassesErrors] (MplProg MplRenamed)
runPasses MplPassesEnv{mplPassesEnvUniqueSupply = supply, mplPassesContext = rsymtab} = 
    let (ls, rs) = split supply
    in runRename' (TopLevel, ls, rsymtab) <=< runParse' <=< B.runBnfc

runPassesTester ::
    String -> 
    IO ()
runPassesTester str = do
    env <- mplPassesEnv
    print $ runPasses env str
