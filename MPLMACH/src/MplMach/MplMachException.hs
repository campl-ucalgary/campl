module MplMach.MplMachException where

import Control.Exception
import qualified Text.Show.Pretty as PrettyShow

-- | Exception for an step
newtype IllegalStep = IllegalStep String
  deriving Show

{- | throws an illegal step if it is an instance of show using the PrettyShow -}
throwIllegalStep ::
    Show a =>
    a ->
    b
throwIllegalStep bad = throw $ IllegalStep $ PrettyShow.ppShow bad


ppShowIllegalStep ::
    Show a =>
    a ->
    IllegalStep
ppShowIllegalStep = IllegalStep . PrettyShow.ppShow

instance Exception IllegalStep where

