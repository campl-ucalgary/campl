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

data Pattern typedef calldef ident =
    PConstructor { _pConstructor :: ident
        , _pConstructorCallDef :: calldef
        , _pConstructorArgs :: [Pattern typedef calldef ident]
        , _pType :: typedef}
    | PUnit { _pUnit :: ident, _pType :: typedef }
    | PRecord { _pRecordPhrase :: NonEmpty (ident , Pattern typedef calldef ident)
                , _pRecordCallDef :: calldef 
                , _pType :: typedef }
    | PList { _pList :: [Pattern typedef calldef ident], _pType :: typedef }
    | PTuple { _pTuple :: (Pattern typedef calldef ident, NonEmpty (Pattern typedef calldef ident)), _pType :: typedef }
    | PVar { _pVar :: ident, _pType :: typedef }
    | PString { _pString :: String, _pType :: typedef }
    | PInt { _pInt :: (ident, Int), _pType :: typedef }
    | PNull  { _pNull :: ident, _pType :: typedef }
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
    
