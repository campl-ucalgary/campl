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

data ClausesGraph calldef ident = ClausesGraph {
    _clauseGraphObjectType :: ObjectType
    , _clauseGraphSpine ::ClauseGraphSpine calldef ident
}  deriving Show

type ClauseGraphSpine calldef ident = NonEmpty (TypeClause  (ClausesKnot calldef ident) (ClausePhraseKnot (ClausesKnot calldef ident) calldef ident) calldef ident)

newtype ClausesKnot calldef ident = ClausesKnot {
    _clauseGraph :: ClausesGraph calldef ident
}  deriving Show

newtype ClausePhraseKnot neighbors calldef ident = ClausePhraseKnot { 
        _phraseParent :: TypeClause 
            neighbors 
            (ClausePhraseKnot neighbors calldef ident)
            calldef
            ident
    }   deriving Show

type TypeClauseKnot calldef ident =
    TypeClause 
        (ClausesKnot calldef ident) 
        (ClausePhraseKnot (ClausesKnot calldef ident) calldef ident)
        calldef 
        ident

data TypeClauseNode = 
    TypeClauseNode (TypeClauseKnot TypeClauseNode  TaggedBnfcIdent)
    | TypeClauseLeaf 
  deriving Show



newtype DefnG ident = DefnG {
        unDefnG :: Defn  
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            (ClausesGraph TypeClauseNode TaggedBnfcIdent)
            Void
            Void
    }


$(concat <$> traverse makeLenses 
    [ ''ClausesKnot
    , ''ClausePhraseKnot 
    , ''ClausesGraph 
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ClausesGraph 
    , ''TypeClauseNode
    ]
 )
