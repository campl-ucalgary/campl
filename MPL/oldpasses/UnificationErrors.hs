{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.UnificationErrors where

import Optics
import MPLAST.MPLASTCore

data UnificationError =
    MatchFailure TypeGTypeTag TypeGTypeTag 
    | OccursCheck TypeTag TypeGTypeTag 
    | ForallMatchFailure (TypeTag, TypeGTypeTag)
  deriving (Show, Eq)

$(makeClassyPrisms ''UnificationError)
