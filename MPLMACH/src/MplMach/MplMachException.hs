module MplMach.MplMachException where

import Control.Exception

-- | Exception for an step
newtype IllegalStep = IllegalStep String
  deriving Show

instance Exception IllegalStep where

