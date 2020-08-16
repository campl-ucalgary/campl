{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module MplAST.MplExpr where


import Optics.TH
import Optics.Prism
import Optics.Operators
import Data.Functor.Foldable.TH

import MplAST.MplIdent

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE 

import MplAST.MplPattern

import GHC.Generics
import Data.Data
import Data.Typeable
import Data.Kind

import Data.Void

import Text.PrettyPrint.GenericPretty

--------------------------
-- Expr definition
--------------------------
type family XMplExpr x

_PrimitiveOperatorParser :: Prism' String PrimitiveOperators
_PrimitiveOperatorParser = prism' embed match
  where
    tmp = 
        [ ("==", PrimitiveEq)
        , ("==", PrimitiveNeq)
        , ("<", PrimitiveLt)
        , ("<=", PrimitiveLeq)
        , (">", PrimitiveGt)
        , (">=", PrimitiveGeq)
        , ("+", PrimitiveAdd)
        , ("-", PrimitiveAdd)
        , ("*", PrimitiveMul)
        , ("/", PrimitiveDiv)
        , ("%", PrimitiveMod)
        , ("^", PrimitiveExp)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp

    
data PrimitiveOperators =
    PrimitiveAdd
    | PrimitiveSub
    | PrimitiveMul
    | PrimitiveDiv
    | PrimitiveMod
    | PrimitiveExp
    | PrimitiveEq
    | PrimitiveNeq
    | PrimitiveLt
    | PrimitiveLeq
    | PrimitiveGt
    | PrimitiveGeq
  deriving (Show, Read, Eq, Data, Generic)

_BuiltInOperatorParser :: Prism' String BuiltInOperators
_BuiltInOperatorParser = prism' embed match
  where
    tmp = 
        [ ("||", InternalOr)
        , ("&&", InternalAnd)
        , ("++", InternalAppend)
        , ("!!", InternalIndex)
        , (":", InternalListCons)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp

data BuiltInOperators = 
    InternalOr
    | InternalAnd
    | InternalAppend
    | InternalIndex
    | InternalListCons
  deriving (Show, Read, Eq, Data, Generic)



{-
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
        , _eOp :: PrimitiveOperators 
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
        , _eFoldPhrases :: NonEmpty (FoldPhraseF pattern calleddef ident (Expr pattern letdef typedef calleddef ident))
        , _eType :: typedef }
    | EUnfold { 
        _eUnfold :: Expr pattern letdef typedef calleddef ident
        , _eUnfoldPhrases :: NonEmpty 
            (UnfoldPhraseF pattern calleddef ident (Expr pattern letdef typedef calleddef ident))
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
        _eRecordPhrases :: NonEmpty 
            (ident, (calleddef, ([pattern], Expr pattern letdef typedef calleddef ident)))
        , _eType :: typedef }
  deriving ( Read, Show, Generic, Data, Eq )

data FoldPhraseF pattern calleddef ident t = FoldPhraseF {
    _foldPhraseIdentF :: ident
    , _foldPhraseFieldDefF :: calleddef
    , _foldPhraseArgsF :: [pattern]
    , _foldPhraseExprF :: t
} deriving ( Read, Show, Generic, Data, Functor, Typeable, Foldable, Traversable, Eq )

data UnfoldPhraseF pattern calleddef ident t = UnfoldPhraseF {
    _unfoldPhraseStateF :: pattern
        -- TODO this should be a pattern
    , _unfoldPhraseFolds :: NonEmpty (FoldPhraseF pattern calleddef ident t)
} deriving ( Read, Show, Generic, Data, Functor, Typeable, Foldable, Traversable, Eq )



$(concat <$> traverse makeLenses
    [ ''Expr
    , ''FoldPhraseF 
    , ''UnfoldPhraseF 
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''Expr
    , ''PrimitiveOperators
    , ''FoldPhraseF 
    ]
 )

$(makeBaseFunctor ''Expr)
    
    -}
