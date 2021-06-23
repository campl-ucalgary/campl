module MplMach.MplMachException where 
import Control.Exception
import qualified Text.Show.Pretty as PrettyShow

-- | Exception for an step
newtype IllegalStep = IllegalStep String

instance Show IllegalStep where
    show (IllegalStep str) = "IllegalStep: " ++ str

-- | Exception for an illegal service
newtype IllegalService = IllegalService String

instance Show IllegalService where
    show (IllegalService str) = "IllegalService: " ++ str

instance Exception IllegalStep where
instance Exception IllegalService where

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

ppShowIllegalService ::
    Show a =>
    a ->
    IllegalService
ppShowIllegalService = IllegalService . PrettyShow.ppShow


