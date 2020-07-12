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

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Void

import GHC.Generics 

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

data ClausesGraph ident = ClausesGraph {
    _clauseGraphObjectType :: ObjectType
    , _clauseGraphSpine :: ClauseGraphSpine ident 
}  deriving Show

type ClauseGraphSpine ident = NonEmpty ( TypeClauseG ident )

newtype ClausesKnot ident typevar = ClausesKnot {
    _clauseGraph :: ClausesGraph ident 
}  deriving Show

newtype ClausePhraseKnot neighbors ident typevar = ClausePhraseKnot { 
        _phraseParent :: TypeClause 
            neighbors 
            (ClausePhraseKnot neighbors ident typevar)
            (TypeClauseNode ident typevar)
            ident
            typevar
    }  deriving Show

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
  deriving Show

type TypeG ident = Type (TypeClauseNode ident ident) ident ident

type PatternG ident = 
    Pattern 
        (TypeG ident)
        (TypePhraseG ident)
        ident

data FunctionCallValueKnot ident = 
    FunctionKnot (FunctionDefG ident)
    | ConstructorDestructorKnot (TypePhraseG ident)

type FunctionDefSigG ident = ([TypeG ident], TypeG ident)

type FunctionDefG ident = 
    FunctionDefn
        (PatternG ident)
        (Stmt (DefnG ident))
        (TypeG ident)
        (FunctionDefSigG ident)
        (FunctionCallValueKnot ident)
        ident


data DefnG ident = 
    ObjectG (ClausesGraph ident )
    | FunctionDecDefG (FunctionDefG ident)
    | ProcessDecDefG

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
