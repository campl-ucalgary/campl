{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module MplCliRunner.Stack where

-- front end
import qualified MplPasses.Passes as Passes
import qualified MplPasses.Parser.BnfcParse as B
import qualified MplPasses.PassesErrors as PassesErrors
import qualified MplPasses.PassesErrorsPprint as PassesErrors
import qualified MplAST.MplCore as MplCore
import MplUtil.UniqueSupply
import System.FilePath

import qualified Data.Bifunctor as Bifunctor

import Optics

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Proxy

import Control.Exception

import Data.Typeable

import MplCliRunner.Flags

{- module for defining monad transformers stack
-}

data FrontEndException 
    = ParsedException String
    | ModuleException String
    | RenamedException String
    | TypeCheckedException String
    | PatternCompiledException String
    | AssembledException String

instance Show FrontEndException where
    show fee = case fee of
        ParsedException str -> concat [ "parse error:" , "\n" , str ]
        ModuleException str -> concat [ "module error:" , "\n" , str ]
        RenamedException str -> concat [ "rename error:" , "\n" , str ]
        TypeCheckedException str -> concat [ "type check / semantic error:" , "\n" , str ]
        PatternCompiledException str -> concat [ "pattern compilation error:" , "\n" , str ]
        AssembledException str -> concat [ "assembler error:" , "\n" , str ]


instance Exception FrontEndException where

newtype MplCli a = MplCli 
    { runMplCli :: ReaderT MplCliEnv (ExceptT FrontEndException IO) a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadError FrontEndException 
    , MonadIO
    , MonadReader MplCliEnv 
    )

execMplCli :: 
    MplCli a ->
    MplCliEnv ->
    IO a
execMplCli mplcli env = runExceptT (runReaderT (runMplCli mplcli) env) >>= \case
    Right res -> return res
    Left err -> throwIO err

data MplCliEnv = MplCliEnv  {
    _mplCliFlags :: [Flag]
    , _mplCliInpFile :: FilePath
}

$(makePrisms ''MplCliEnv)
$(makeLenses ''MplCliEnv)
