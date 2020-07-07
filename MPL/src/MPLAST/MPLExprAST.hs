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
module MPLAST.MPLExprAST where

import MPLUtil.Data.List.NonEmpty

import Optics.TH
import Optics.Prism
import Optics.Operators
import Data.Functor.Foldable.TH

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE 

import MPLAST.MPLPatternAST

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )
--------------------------
-- Expr definition
--------------------------

data Expr pattern letdef def var =
    EIf { _eIf :: Expr pattern letdef def var
        , _eThen :: Expr pattern letdef def var
        , _eElse :: Expr pattern letdef def var }
    | ELet { _eLet ::  NonEmpty letdef, _eIn :: Expr pattern letdef def var}
    | EOp { _eLarg :: Expr pattern letdef def var
        , _eOp :: InternalOperators 
        , _eRarg :: Expr pattern letdef def var}
    | EList { _eList :: [Expr pattern letdef def var] }
    | EVar { _eVar :: var }
    | EInt { _eInt :: (var,Int) }
    | EString { _eString :: (var, String) }
    | EChar { _eChar :: (var, Char) }
    | EDouble { _eDouble :: (var, Double) }
    | EUnit { _eUnit :: var }
    | EFold { _eFold :: Expr pattern letdef def var, _eFoldPhrases :: [FoldPhraseF def pattern (Expr pattern letdef def var)] }
    | EUnfold { _eUnfold :: Expr pattern letdef def var
        , _eUnfoldPhrases :: [UnfoldPhraseF def pattern (Expr pattern letdef def var)] }
    | ECase { _eCaseOn :: Expr pattern letdef def var
        , _eCases :: NonEmpty ([pattern], Expr pattern letdef def var) }
    | ESwitch { _eSwitches :: NonEmpty (Expr pattern letdef def var, Expr pattern letdef def var) }
    | EDestructorConstructor { _eCalledDestructorConstructor :: def, _eArgs :: [Expr pattern letdef def var] }
    | ETuple (Expr pattern letdef def var, NonEmpty (Expr pattern letdef def var))
    | EFun  { _eCalledFun :: def, _eArgs :: [Expr pattern letdef def var] }
    | ERecord { _eRecord :: def, _eRecordPhrases :: NonEmpty (def, Expr pattern letdef def var) }
  deriving ( Read, Show, Generic, Out, Data, Eq )

data FoldPhraseF def pattern t = FoldPhraseF {
    _foldPhraseFieldF :: def
    , _foldPhraseArgsF :: [pattern]
    , _foldPhraseExprF :: t
} deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable, Eq )

data UnfoldPhraseF def pattern t = UnfoldPhraseF {
    _unfoldPhraseStateF :: pattern
        -- TODO this should be a pattern
    , _unfoldPhraseFolds :: [FoldPhraseF def pattern t]
} deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable, Eq )


_InternalOperatorParser :: Prism' String InternalOperators
_InternalOperatorParser = prism' embed match
  where
    tmp = 
        [ ("||", InternalOr)
        , ("&&", InternalAnd)
        , ("==", InternalEq)
        , ("==", InternalNeq)
        , ("<", InternalLt)
        , ("<=", InternalLeq)
        , (">", InternalGt)
        , (">=", InternalGeq)
        , ("++", InternalAppend)
        , ("+", InternalAdd)
        , ("-", InternalAdd)
        , ("*", InternalMul)
        , ("/", InternalDiv)
        , ("%", InternalMod)
        , ("^", InternalExp)
        , ("!!", InternalIndex)
        , (":", InternalCons)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp
    
data InternalOperators =
    InternalOr
    | InternalAnd
    | InternalEq
    | InternalNeq
    | InternalLt
    | InternalLeq
    | InternalGt
    | InternalGeq
    | InternalAppend

    | InternalAdd
    | InternalSub

    | InternalMul
    | InternalDiv
    | InternalMod
    | InternalExp

    | InternalIndex

    | InternalCons

  deriving MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE 

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''Expr
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''Expr
    , ''InternalOperators
    ]
 )

$(makeBaseFunctor ''Expr)
    
