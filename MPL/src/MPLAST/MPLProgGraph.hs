{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
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
import MPLUtil.UniqueSupply
import MPLUtil.Data.Stream

import Optics
import Optics.State.Operators

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Void
import Data.Coerce
import Data.Maybe

import GHC.Generics 

import Control.Monad.State
import Control.Arrow

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Tuple


data TaggedBnfcIdent = TaggedBnfcIdent {
    _taggedBnfcIdentBnfcIdent :: BnfcIdent
    , _taggedBnfcIdentTag :: UniqueTag
} deriving (Show, Read)

-- equality of tagged bnfcidents should depend only 
-- on equality of the unique tag
instance Eq TaggedBnfcIdent where
    TaggedBnfcIdent _ a == TaggedBnfcIdent _ b =  a == b

newtype UniqueTag = UniqueTag { _unUniqueTag :: Unique }
  deriving (Show, Eq, Ord, Read, Enum)

$(makeClassy ''UniqueTag)


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
            (TypeClauseCallDefKnot ident typevar)
            ident
            typevar
    }  
instance Show (ClausePhraseKnot neighbors ident typevar) where
    show (ClausePhraseKnot _) = "ClausePhraseKnot _ "

type TypeClauseKnot ident typevar =
    TypeClause 
        (ClausesKnot ident typevar) 
        (ClausePhraseKnot (ClausesKnot ident typevar) ident typevar)
        (TypeClauseCallDefKnot ident typevar)
        ident
        typevar

type TypeClauseG ident = TypeClause 
    (ClausesKnot ident ident) 
    (ClausePhraseKnot (ClausesKnot ident ident) ident ident) 
    (TypeClauseCallDefKnot ident ident) ident ident

type TypePhraseG ident = TypePhrase 
    (ClausePhraseKnot (ClausesKnot ident ident) ident ident) 
    (TypeClauseCallDefKnot ident ident) ident ident

data TypeClauseCallDefKnot ident typevar = 
    TypeClauseCallDefKnot (TypeClauseKnot ident typevar)

instance (Eq ident, Eq typevar) => Eq (TypeClauseCallDefKnot ident typevar) where
    TypeClauseCallDefKnot _ == TypeClauseCallDefKnot _ = True
-- We'll just assume that this is true...

instance (Show ident, Show typevar) => Show (TypeClauseCallDefKnot ident typevar) where
    show (TypeClauseCallDefKnot _) = "TypeClauseCallDefKnot"

type TypeG ident = TypeGTypeVar ident ident
type TypeGTypeVar ident typevar = Type (TypeClauseCallDefKnot ident ident) ident typevar

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


$(concat <$> traverse makeLenses 
    [ ''ClausesKnot
    , ''ClausePhraseKnot 
    , ''ClausesGraph 
    , ''TaggedBnfcIdent
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ClausesGraph 
    , ''TypeClauseCallDefKnot
    , ''DefnG
    , ''TaggedBnfcIdent
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

freshUniqueTag ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m UniqueTag
freshUniqueTag = 
    uniqueSupply %%= (first (UniqueTag . uniqueFromSupply) <<< split)

freshUniqueTags :: 
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m (Stream UniqueTag)
freshUniqueTags = do
    supply <- freshUniqueSupply
    return $ coerce $ uniquesFromSupply supply

freshTypeTags :: 
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m (Stream TypeTag)
freshTypeTags = coerce <$> freshUniqueTags

freshUniqueSupply ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m UniqueSupply
freshUniqueSupply =
    uniqueSupply %%= split

tagBnfcIdent ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    BnfcIdent ->
    m TaggedBnfcIdent
tagBnfcIdent ident = do
    review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag

freshTypeTag ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m TypeTag
freshTypeTag = TypeTag <$> freshUniqueTag

taggedBnfcIdentName :: Lens' TaggedBnfcIdent String
taggedBnfcIdentName = lens get set
  where
    get n = n ^. taggedBnfcIdentBnfcIdent % bnfcIdentName
    set n v = n & taggedBnfcIdentBnfcIdent % bnfcIdentName .~ v

taggedBnfcIdentPos :: Lens' TaggedBnfcIdent (Int, Int)
taggedBnfcIdentPos =  lens get set
  where
    get n = n ^. taggedBnfcIdentBnfcIdent % bnfcIdentPos
    set n v = n & taggedBnfcIdentBnfcIdent % bnfcIdentPos .~ v

bnfcIdentName :: Lens' BnfcIdent String
bnfcIdentName = lens get set
  where
    get n = n ^. stringPos % _1
    set n v = n & stringPos % _1 .~ v

bnfcIdentPos :: Lens' BnfcIdent (Int, Int)
bnfcIdentPos = lens get set
  where
    get n = n ^. stringPos % _2
    set n v = n & stringPos % _2 .~ v

instance HasUniqueTag TaggedBnfcIdent where
    uniqueTag = taggedBnfcIdentTag 

progGQueryFunctions :: 
    (Prog (DefnG TaggedBnfcIdent TypeTag)) -> 
    [FunctionDefG TaggedBnfcIdent TypeTag]
progGQueryFunctions (Prog defsg) = concatMap f defsg
  where
    f (Stmt defns wdefs) = mapMaybe g (NE.toList defns) ++ progGQueryFunctions (Prog wdefs)
    g (FunctionDecDefG defn) = Just defn
    g _ = Nothing

progGQueryTypeClausesGraphs :: 
    (Prog (DefnG TaggedBnfcIdent TypeTag)) -> 
    [ClausesGraph TaggedBnfcIdent]
progGQueryTypeClausesGraphs (Prog defsg) = concatMap f defsg
  where
    f (Stmt defns wdefs) = mapMaybe g (NE.toList defns) ++ progGQueryTypeClausesGraphs (Prog wdefs)
    g (ObjectG defn) = Just defn
    g _ = Nothing

