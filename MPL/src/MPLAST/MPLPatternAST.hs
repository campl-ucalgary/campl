{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module MPLAST.MPLPatternAST where

import MPLUtil.Data.List.NonEmpty

import Optics
import MPLUtil.Optics.TH
import Data.Functor.Foldable.TH

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE 

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )
--------------------------
-- Expr definition
--------------------------

data Pattern calldef ident =
    PConstructor { _pConstructor :: ident, _pConstructorArgs :: [Pattern calldef ident]}
    | PUnit 
    | PRecord { _pRecordPhrase :: NonEmpty (ident, Pattern calldef ident) }
    | PList { _pList :: [Pattern calldef ident] }
    | PTuple { _pTuple :: (Pattern calldef ident, NonEmpty (Pattern calldef ident)) }
    | PVar { _pVar :: ident }
    | PString { _pString :: String }
    | PInt { _pInt :: (ident, Int) }
    | PNull  { _pNull :: ident }
  deriving ( Read, Show, Generic, Out, Data, Eq )


$(concat <$> traverse mplMakeFieldLabels
    [ ''Pattern
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''Pattern
    ]
 )

$(makeBaseFunctor ''Pattern)
    
