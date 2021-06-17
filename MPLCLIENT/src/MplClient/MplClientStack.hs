{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module MplClient.MplClientStack where

import Optics

import Control.Monad.Reader
import Data.Coerce

import MplMach.MplMachServices
import MplMach.MplMachTypes


newtype MplClient r a = MplClient (ReaderT r IO a)
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadIO 
    )

runMplClient ::
    MplClient r a ->
    r ->
    IO a
runMplClient ma r = runReaderT (coerce ma) r


data MplClientEnv = MplClientEnv 
    { _hostname :: String
    , _port :: String
    , _serviceCh :: ServiceCh
    }

defaultMplClientEnv :: MplClientEnv 
defaultMplClientEnv = MplClientEnv "" "" (coerce @Int @ServiceCh 0)

$(makeLenses ''MplClientEnv)



