{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheckCallErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

data TypeCheckCallErrors = 
    CannotCallTerm (IdP MplRenamed)
    | CannotCallTypeCts (IdP MplRenamed)
  deriving Show

$(makeClassyPrisms ''TypeCheckCallErrors)
