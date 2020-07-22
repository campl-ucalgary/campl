{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MPLAST.MPLProgGraph where

import MPLAST.MPLProg
import MPLAST.MPLProgI
import MPLAST.MPLTypeAST
import MPLAST.MPLPatternAST
import MPLAST.MPLExprAST 

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Void

import GHC.Generics 

import Control.Monad.State

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

data ClausesGraph ident = ClausesGraph {
    _clauseGraphObjectType :: ObjectType
    , _clauseGraphSpine :: ClauseGraphSpine ident 
}  deriving Show

type ClauseGraphSpine ident = NonEmpty ( TypeClauseG ident )

newtype ClausesKnot ident typevar = ClausesKnot {
    _clauseGraph :: ClausesGraph ident 
}  

instance Show (ClausesKnot ident typevar) where
    show (ClausesKnot _) = "ClausesKnot _ "

newtype ClausePhraseKnot neighbors ident typevar = ClausePhraseKnot { 
        _phraseParent :: TypeClause 
            neighbors 
            (ClausePhraseKnot neighbors ident typevar)
            (TypeClauseNode ident typevar)
            ident
            typevar
    }  
instance Show (ClausePhraseKnot neighbors ident typevar) where
    show (ClausePhraseKnot _) = "ClausePhraseKnot _ "

type TypeClauseKnot ident typevar =
    TypeClause 
        (ClausesKnot ident typevar) 
        (ClausePhraseKnot (ClausesKnot ident typevar) ident typevar)
        (TypeClauseNode ident typevar)
        ident
        typevar

type TypeClauseG ident = TypeClause 
    (ClausesKnot ident ident) 
    (ClausePhraseKnot (ClausesKnot ident ident) ident ident) 
    (TypeClauseNode ident ident) ident ident

type TypePhraseG ident = TypePhrase 
    (ClausePhraseKnot (ClausesKnot ident ident) ident ident) 
    (TypeClauseNode ident ident) ident ident


data TypeClauseNode ident typevar = 
    TypeClauseNode (TypeClauseKnot ident typevar)
    | TypeClauseLeaf 

-- do NOT recurse...
instance (Eq ident, Eq typevar) => Eq (TypeClauseNode ident typevar) where
    TypeClauseNode _ == TypeClauseNode _ = True
    TypeClauseLeaf == TypeClauseLeaf = True
    _ == _ = False

instance (Show ident, Show typevar) => Show (TypeClauseNode ident typevar) where
    show (TypeClauseNode _) = "TypeClauseNode"
    show (TypeClauseLeaf) = "TypeClauseLeaf"

type TypeG ident = TypeGTypeVar ident ident
type TypeGTypeVar ident typevar = Type (TypeClauseNode ident ident) ident typevar

type PatternG ident typevar = 
    Pattern 
        (TypeGTypeVar ident typevar)
        (TypePhraseG ident)
        ident

type ExprG ident typevar = 
    Expr 
        (PatternG ident typevar)
        (Stmt (DefnG ident typevar)) 
        (TypeGTypeVar ident typevar) 
        (FunctionCallValueKnot ident typevar) ident

data FunctionCallValueKnot ident typevar = 
    FunctionKnot (FunctionDefG ident typevar)
    | ConstructorDestructorKnot (TypePhraseG ident)
  deriving Show

type StmtG ident typevar = Stmt (DefnG ident typevar)

type FunctionDefSigG ident typevar = ([TypeGTypeVar ident typevar], TypeGTypeVar ident typevar)

type FunctionDefG ident typevar = 
    FunctionDefn
        (PatternG ident typevar)
        (StmtG ident typevar)
        (TypeGTypeVar ident typevar)
        (TypeGTypeVar ident typevar) -- (FunctionDefSigG ident typevar)
        (FunctionCallValueKnot ident typevar)
        ident


data DefnG ident typevar = 
    ObjectG (ClausesGraph ident)
    | FunctionDecDefG (FunctionDefG ident typevar)
    | ProcessDecDefG
  deriving Show

newtype TypeTag = TypeTag UniqueTag
  deriving (Eq, Ord)

type TypeGTypeTag = 
    TypeGTypeVar TaggedBnfcIdent TypeTag

instance Show TypeTag where
    show (TypeTag (UniqueTag n)) = show n

freshTypeTag ::
    ( MonadState c m
    , HasUniqueTag c ) => 
    m TypeTag
freshTypeTag = TypeTag <$> freshUniqueTag


$(concat <$> traverse makeLenses 
    [ ''ClausesKnot
    , ''ClausePhraseKnot 
    , ''ClausesGraph 
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ClausesGraph 
    , ''TypeClauseNode
    , ''DefnG
    ]
 )

phraseGObjType = typePhraseContext 
    % phraseParent 
    % clauseGObjType

phraseGClauseTypeArgs = 
    typePhraseContext 
    % phraseParent 
    % typeClauseArgs

clauseGObjType = 
    typeClauseNeighbors 
    % clauseGraph 
    % clauseGraphObjectType 
