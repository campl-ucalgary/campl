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

data TaggedChIdent = TaggedChIdent {
    _taggedChIdentTaggedBnfcIdent :: TaggedBnfcIdent
    , _taggedChIdentPolarity :: Polarity
} deriving Show

-- equality of tagged bnfcidents should depend only 
-- on equality of the unique tag
instance Eq TaggedBnfcIdent where
    TaggedBnfcIdent _ a == TaggedBnfcIdent _ b =  a == b

instance Ord TaggedBnfcIdent where
    TaggedBnfcIdent _ a <= TaggedBnfcIdent _ b =  a <= b

newtype UniqueTag = UniqueTag { _unUniqueTag :: Unique }
  deriving (Show, Eq, Ord, Read, Enum)

$(makeClassy ''UniqueTag)

data ClausesGraph ident = ClausesGraph {
    _clauseGraphObjectType :: ObjectType
    , _clauseGraphSpine :: ClauseGraphSpine ident 
    , _clauseGraphUniqueTag :: UniqueTag
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

type ExprG ident typevar chident = 
    Expr 
        (PatternG ident typevar)
        (Stmt (DefnG ident typevar chident)) 
        (TypeGTypeVar ident typevar) 
        (FunctionCallValueKnot ident typevar chident) ident

data FunctionCallValueKnot ident typevar chident = 
    FunctionKnot (FunctionDefG ident typevar chident)
    | ConstructorDestructorKnot (TypePhraseG ident)
    | LocalVar 
  deriving Show

data ProcessCallValueKnot ident typevar chident =
    ProcessKnot (ProcessDefG ident typevar chident)
    | ProtocolCoprotocolKnot (TypePhraseG ident)
    | LocalChannel
  deriving Show

type StmtG ident typevar chident = Stmt (DefnG ident typevar chident)

type FunctionDefSigG ident typevar = ([TypeGTypeVar ident typevar], TypeGTypeVar ident typevar)

type FunctionDefG ident typevar chident= 
    FunctionDefn
        (PatternG ident typevar)
        (StmtG ident typevar chident)
        (TypeGTypeVar ident typevar)
        (TypeGTypeVar ident typevar) -- (FunctionDefSigG ident typevar)
        (FunctionCallValueKnot ident typevar chident)
        ident

type ProcessDefG ident typevar chident = 
    ProcessDefn 
        (PatternG ident typevar)
        (StmtG ident typevar chident)
        (TypeGTypeVar ident typevar)
        (TypeGTypeVar ident typevar) 
        (FunctionCallValueKnot ident typevar chident)
        (ProcessCallValueKnot ident typevar chident)
        ident 
        chident
    

data DefnG ident typevar chident = 
    ObjectG (ClausesGraph ident)
    | FunctionDecDefG (FunctionDefG ident typevar chident)
    | ProcessDecDefG (ProcessDefG ident typevar chident)
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
    , ''TaggedChIdent 
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ClausesGraph 
    , ''TypeClauseCallDefKnot
    , ''DefnG
    , ''TaggedBnfcIdent
    , ''TaggedChIdent 
    ]
 )

phraseGClausesGraph = typePhraseContext 
    % phraseParent 
    % typeClauseNeighbors
    % clauseGraph

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


instance HasUniqueTag (ClausesGraph ident) where
    uniqueTag = clauseGraphUniqueTag 

instance Eq (ClausesGraph ident) where
    a == b = a ^. uniqueTag == b ^. uniqueTag

instance HasUniqueTag TaggedBnfcIdent where
    uniqueTag = taggedBnfcIdentTag 

instance HasUniqueTag TaggedChIdent where
    uniqueTag = taggedChIdentTaggedBnfcIdent % taggedBnfcIdentTag 

instance HasBnfcIdent TaggedBnfcIdent where
    bnfcIdent = taggedBnfcIdentBnfcIdent 

instance HasBnfcIdent TaggedChIdent where
    bnfcIdent = taggedChIdentTaggedBnfcIdent % taggedBnfcIdentBnfcIdent


progGQueryFunctions :: 
    (Prog (DefnG TaggedBnfcIdent TypeTag TaggedChIdent)) -> 
    [FunctionDefG TaggedBnfcIdent TypeTag TaggedChIdent]
progGQueryFunctions (Prog defsg) = concatMap f defsg
  where
    f (Stmt defns wdefs) = mapMaybe g (NE.toList defns) ++ progGQueryFunctions (Prog wdefs)
    g (FunctionDecDefG defn) = Just defn
    g _ = Nothing

progGQueryTypeClausesGraphs :: 
    (Prog (DefnG TaggedBnfcIdent TypeTag TaggedChIdent)) -> 
    [ClausesGraph TaggedBnfcIdent]
progGQueryTypeClausesGraphs (Prog defsg) = concatMap f defsg
  where
    f (Stmt defns wdefs) = mapMaybe g (NE.toList defns) ++ progGQueryTypeClausesGraphs (Prog wdefs)
    g (ObjectG defn) = Just defn
    g _ = Nothing

-- | Gets the statevars mapping to a type clause..
clauseGraphStateVarsToClause :: 
    ClausesGraph ident ->
    NonEmpty (ident, TypeClauseG ident)
clauseGraphStateVarsToClause = 
    fmap (view typeClauseStateVar &&& id) . view clauseGraphSpine

clauseGraphStateVars :: 
    ClausesGraph ident ->
    NonEmpty ident
clauseGraphStateVars = fmap fst . clauseGraphStateVarsToClause

clauseGraphTypeArgs :: 
    ClausesGraph ident ->
    [ident]
clauseGraphTypeArgs clausegraph = focusedclauseg ^. typeClauseArgs
  where
    -- recall that each type clause in a clause graph
    -- should have the same type variables
    (focusedclauseg :| _) = clausegraph ^. clauseGraphSpine

-- | get all the clause graph phrases
clauseGraphPhrases ::
    ClausesGraph ident ->
    NonEmpty (TypePhraseG ident)
clauseGraphPhrases clausegraph = 
    NE.fromList 
    $ concatMap f
    $ NE.toList 
    $ clausegraph ^. clauseGraphSpine 
  where
    f clauseg = clauseg ^. typeClausePhrases
