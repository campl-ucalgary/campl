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

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Void

import GHC.Generics 

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

data ClausesGraph ident = ClausesGraph {
    _clauseGraphObjectType :: ObjectType
    , _clauseGraphSpine :: ClauseGraphSpine  ident
}  deriving Show

type ClauseGraphSpine ident = NonEmpty (
    TypeClause 
        (ClausesKnot ident) 
        (ClausePhraseKnot (ClausesKnot ident) ident) 
        (TypeClauseNode ident) 
        ident
        )

newtype ClausesKnot ident = ClausesKnot {
    _clauseGraph :: ClausesGraph ident
}  deriving Show

newtype ClausePhraseKnot neighbors ident = ClausePhraseKnot { 
        _phraseParent :: TypeClause 
            neighbors 
            (ClausePhraseKnot neighbors ident)
            (TypeClauseNode ident)
            ident
    }  deriving Show

type TypeClauseKnot ident =
    TypeClause 
        (ClausesKnot ident) 
        (ClausePhraseKnot (ClausesKnot ident) ident)
        (TypeClauseNode ident)
        ident

-- type TypeClauseG ident = TypeClause (ClauseGraphSpine  ident) phrasecontext calldef ident
-- type TypePhraseG calldef ident = ()

data TypeClauseNode ident = 
    TypeClauseNode (TypeClauseKnot ident)
    | TypeClauseLeaf 
  deriving Show

data DefnG ident = 
    ObjectG (ClausesGraph ident)
    | FunctionDefG
    | ProcessDefG

{-
{
        unDefnG :: Defn  
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            Void
            Void
    } 
    -}

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
