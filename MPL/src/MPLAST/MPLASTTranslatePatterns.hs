{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module MPLAST.MPLASTTranslatePatterns where

import Optics

import Data.Function
import qualified Data.Bifunctor as Bifunctor
import Control.Monad

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 
import Data.Maybe

import MPLAST.MPLTypeAST
import MPLAST.MPLPatternAST
import MPLAST.MPLExprAST
import MPLAST.MPLProcessCommandsAST
import MPLAST.MPLProg
import MPLAST.MPLProgI

import MPLUtil.Data.Either

import qualified Language.AbsMPL as B

import Data.Data
import Data.Typeable
import Data.Either
import Data.Semigroup
import Control.Arrow

import MPLAST.MPLASTTranslateErrors

import Text.PrettyPrint.GenericPretty

translateBnfcPattern ::
    B.Pattern -> 
    PatternIBnfc
translateBnfcPattern (B.PATTERN p) = translateBnfcPattern p
translateBnfcPattern (B.LIST_COLON_PATTERN a colon b) = error "Colon pattern not implemented yet"
translateBnfcPattern (B.CONSTRUCTOR_PATTERN_ARGS ident _ patts _) = 
    review _PConstructor (ident ^. uIdentBnfcIdentGetter,  (), map translateBnfcPattern patts, ())
translateBnfcPattern (B.CONSTRUCTOR_PATTERN_NO_ARGS ident) = 
   review _PConstructor (ident ^. uIdentBnfcIdentGetter, (), [], ())

translateBnfcPattern (B.UNIT_PATTERN lbr rbr) = error "Unit pattern not implemented yet"

translateBnfcPattern (B.RECORD_PATTERN _ a as _) = do
    review _PRecord (f a :| map f as, (), ())
  where
    f (B.DESTRUCTOR_PATTERN_PHRASE ident patt) = (ident ^. uIdentBnfcIdentGetter, translateBnfcPattern patt)
        

translateBnfcPattern (B.LIST_PATTERN lbr patts rbr) = error "list pattern not implemented yet"

translateBnfcPattern (B.TUPLE_PATTERN lbr a as rbr) = 
    review _PTuple 
        ( (translateBnfcPattern a, NE.fromList (map f as))
        , () )
  where
    f (B.TUPLE_LIST_PATTERN patt) = translateBnfcPattern patt

translateBnfcPattern (B.VAR_PATTERN a) = 
   review _PVar (a ^. pIdentBnfcIdentGetter, ())

translateBnfcPattern (B.STR_PATTERN a) = error "string not implemented yet"
translateBnfcPattern (B.INT_PATTERN a) = review _PInt (a ^. pIntegerGetter, ())

translateBnfcPattern (B.NULL_PATTERN a) = 
    review _PNull (a ^. nullPatternBnfcIdentGetter, ())

translateBnfcPattern (B.BRACKETED_PATTERN _ pattern _) = translateBnfcPattern pattern
