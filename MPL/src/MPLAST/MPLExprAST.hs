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

data Expr pattern letdef typedef calleddef ident =
    EIf { _eIf :: Expr pattern letdef typedef calleddef ident
        , _eThen :: Expr pattern letdef typedef calleddef ident
        , _eElse :: Expr pattern letdef typedef calleddef ident
        , _eType :: typedef }
    | ELet { 
        _eLet ::  NonEmpty letdef
        , _eIn :: Expr pattern letdef typedef calleddef ident
        , _eType :: typedef }
    | EOp { _eLarg :: Expr pattern letdef typedef calleddef ident
        , _eOp :: InternalOperators 
        , _eRarg :: Expr pattern letdef typedef calleddef ident
        , _eType :: typedef}
    | EVar { 
        _eVar :: ident 
        , _eType :: typedef }
    | EInt { 
        _eInt :: (ident,Int)
        , _eType :: typedef }
    | EChar { 
        _eChar :: (ident, Char) 
        , _eType :: typedef }
    | EDouble { 
        _eDouble :: (ident, Double) 
        , _eType :: typedef } 
    | EList { 
        _eList :: [Expr pattern letdef typedef calleddef ident] 
        , _eType :: typedef }
    | EString { 
        _eString :: (ident, String) 
        , _eType :: typedef }
    | EUnit { 
        _eUnit :: ident 
        , _eType :: typedef }
    | ETrue { 
        _eTrue :: ident 
        , _eType :: typedef }
    | EFalse { 
        _eTrue :: ident 
        , _eType :: typedef }
    | EFold { 
        _eFold :: Expr pattern letdef typedef calleddef ident
        , _eFoldPhrases :: [FoldPhraseF pattern calleddef ident (Expr pattern letdef typedef calleddef ident)]
        , _eType :: typedef }
    | EUnfold { 
        _eUnfold :: Expr pattern letdef typedef calleddef ident
        , _eUnfoldPhrases :: [UnfoldPhraseF pattern calleddef ident (Expr pattern letdef typedef calleddef ident)] 
        , _eType :: typedef }
    | ECase { 
         _eCaseOn :: Expr pattern letdef typedef calleddef ident
        , _eCases :: NonEmpty (pattern, Expr pattern letdef typedef calleddef ident) 
        , _eType :: typedef}
    | ESwitch { 
        _eSwitches :: NonEmpty (Expr pattern letdef typedef calleddef ident, Expr pattern letdef typedef calleddef ident) 
        , _eType :: typedef}
    | EConstructorDestructor { 
        _eCalledConstructorDestructor :: ident
        , _eCalledConstructorDestructorDef :: calleddef
        , _eArgs :: [Expr pattern letdef typedef calleddef ident]
        , _eType :: typedef }
    | ETuple { 
        _eTupleArgs :: (Expr pattern letdef typedef calleddef ident
        , NonEmpty (Expr pattern letdef typedef calleddef ident)), _eType :: typedef }
    | ECall  { 
        _eCalledIdent :: ident
        , _eCalledDef :: calleddef
        , _eArgs :: [Expr pattern letdef typedef calleddef ident] 
        , _eType :: typedef }
    | ERecord { 
        _eRecord :: ident
        , _eRecordDef :: calleddef
        , _eRecordPhrases :: NonEmpty (ident, Expr pattern letdef typedef calleddef ident) 
        , _eType :: typedef }
  deriving ( Read, Show, Generic, Out, Data, Eq )

data FoldPhraseF pattern calleddef ident t = FoldPhraseF {
    _foldPhraseFieldF :: ident
    , _foldPhraseFieldDefF :: calleddef
    , _foldPhraseArgsF :: [pattern]
    , _foldPhraseExprF :: t
} deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable, Eq )

data UnfoldPhraseF pattern calleddef ident t = UnfoldPhraseF {
    _unfoldPhraseStateF :: pattern
        -- TODO this should be a pattern
    , _unfoldPhraseFolds :: [FoldPhraseF pattern calleddef ident t]
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
    
