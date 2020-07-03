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

import MPLIdent

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

data Expr letdef def var =
    EIf { _eIf :: Expr letdef def var, _eThen :: Expr letdef def var, _eElse :: Expr letdef def var }
    | ELet { _eLet ::  NonEmpty letdef, _eIn :: Expr letdef def var}
    | EOp { _eLarg :: Expr letdef def var, _eOp :: InternalOperators , _eRarg :: Expr letdef def var}
    | EList { _eList :: [Expr letdef def var] }
    | EVar { _eVar :: var }
    | EInt { _eInt :: Int }
    | EString { _eString :: String }
    | EChar { _eChar :: Char }
    | EDouble { _eDouble :: Double }
    | EUnit { _eUnit :: () }
    | EFold { _eFold :: Expr letdef def var, _eFoldPhrases :: [FoldPhraseF def var (Expr letdef def var)] }
    | EUnfold { _eUnfold :: Expr letdef def var, _eUnfoldPhrases :: [UnfoldPhraseF def var (Expr letdef def var)] }
    | ECase { _eCaseOn :: Expr letdef def var, _eCases :: [(Pattern def var, Expr letdef def var)] }
    | ESwitch { _eSwitchOn :: Expr letdef def var, _eSwitches :: [(Expr letdef def var, Expr letdef def var)] }
    | EDestructorConstructor { _eCalledDestructorConstructor :: def, _eArgs :: [Expr letdef def var] }
    | ETuple (Expr letdef def var, NonEmpty (Expr letdef def var))
    | EFun  { _eCalledFun :: def, _eArgs :: [Expr letdef def var] }
    | ERecord { _eRecordPhrases :: NonEmpty (def, Expr letdef def var) }
  deriving ( Read, Show, Generic, Out, Data )

data FoldPhraseF def var t = FoldPhraseF {
    _foldPhraseFieldF :: def
    , _foldPhraseArgsF :: [var]
    , _foldPhraseExprF :: t
} deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable )

data UnfoldPhraseF def var t = UnfoldPhraseF {
    _unfoldPhraseStateF :: def
    , _unfoldPhraseFolds :: [FoldPhraseF def var t]
} deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable )


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
    
