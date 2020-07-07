module MPLUtil.Data.List.NonEmpty where

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

instance Out a => Out (NonEmpty a) where
    docPrec n = docPrec n . NE.toList
    doc = docPrec 0

