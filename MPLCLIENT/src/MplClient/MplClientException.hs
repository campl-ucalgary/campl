module MplClient.MplClientException where

import Control.Exception

newtype IllegalServerCommand = IllegalServerCommand String
  deriving Show

instance Exception IllegalServerCommand  where
