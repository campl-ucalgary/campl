{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MplPasses.TypeChecker.TypeCheckErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckErrorPkg 
import MplPasses.TypeChecker.TypeCheckMplTypeSub

import Data.Foldable
import Data.Function
import Data.List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Bool
import Control.Arrow

data TypeCheckErrors =
    SemanticErrors TypeCheckSemanticErrors
    | CallErrors TypeCheckCallErrors
  deriving Show

$(makeClassyPrisms ''TypeCheckErrors)

instance AsTypeCheckSemanticErrors TypeCheckErrors where
    _TypeCheckSemanticErrors = _SemanticErrors

instance AsTypeCheckCallErrors TypeCheckErrors where
    _TypeCheckCallErrors = _CallErrors 

instance AsTypeUnificationError TypeCheckErrors MplTypeSub where
    _TypeUnificationError = _SemanticErrors % _TypeCheckUnificationErrors 

instance AsKindCheckErrors TypeCheckErrors where
    _KindCheckErrors = _SemanticErrors % _KindCheckErrors 

