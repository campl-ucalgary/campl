{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module MplMach.MplMachStack where

import Control.Monad.IO.Class


newtype MplMach a = MplMach { unwrapMplMach :: IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )
