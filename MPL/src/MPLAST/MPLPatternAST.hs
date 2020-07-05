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

import MPLIdent

import Optics
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

data Pattern def var =
    PConstructor { _pConstructor :: def, _pConstructorArgs :: [Pattern def var]}
    | PUnit 
    | PRecord { _pRecordPhrase :: NonEmpty (def, Pattern def var) }
    | PList { _pList :: [Pattern def var] }
    | PTuple { _pTuple :: (Pattern def var, NonEmpty (Pattern def var)) }
    | PVar { _pVar :: var }
    | PString { _pString :: String }
    | PInt { _pInt :: (var, Int) }
    | PNull  { _pNull :: var }
  deriving ( Read, Show, Generic, Out, Data )


$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''Pattern
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''Pattern
    ]
 )

$(makeBaseFunctor ''Pattern)
    
