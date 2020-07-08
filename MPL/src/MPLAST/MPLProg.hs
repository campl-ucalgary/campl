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
module MPLAST.MPLProg where

import MPLAST.MPLExprAST
import MPLAST.MPLTypeAST hiding (TypePhrase (..))
import MPLAST.MPLPatternAST
import MPLAST.MPLProcessCommandsAST

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import GHC.Generics 

import Data.List.NonEmpty

newtype Prog defn = Prog [Stmt defn]
  deriving (Show, Eq, Read)

data Stmt defn = Stmt {
    _stmtDefns :: NonEmpty defn
    , _stmtWhereBindings :: [Stmt defn] 
} deriving (Show, Eq, Read, Functor, Foldable, Traversable)

data Defn datadefn codatadefn protdefn coprotdefn fundefn procdefn =
    DataDefn datadefn
    | CodataDefn codatadefn
    | ProtocolDefn protdefn
    | CoprotocolDefn coprotdefn
    | FunctionDecDefn fundefn
    | ProcessDecDefn procdefn
  deriving (Show, Eq, Read)

type TypeClausesPhrases neighbors phrasecontext phrase ident = 
    NonEmpty (TypeClausePhrase neighbors phrasecontext phrase ident)

type TypeClausePhrase neighbors phrasecontext phrase ident = 
        TypeClause neighbors
        (TypePhrase phrasecontext phrase ident) 
        ident

data TypeClause neighbors phrase ident = TypeClause {
    _typeClauseName :: ident 
    , _typeClauseArgs :: [ ident]
    , _typeClauseStateVar ::  ident
    , _typeClausePhrases :: [phrase]
    , _typeClauseNeighbors :: neighbors
}  deriving (Show, Eq, Read, Generic)

data TypePhrase phrasecontext phrase ident = TypePhrase {
    _typePhraseContext :: phrasecontext
    , _typePhraseName :: ident
    , _typePhraseType :: phrase
}  deriving (Show, Eq, Read, Generic)

data DataPhrase calldef ident = DataPhrase {
    _dataFrom :: [Type calldef ident]
    , _dataTo :: ident
}  deriving (Show, Eq, Read, Generic)

data CodataPhrase calldef ident = CodataPhrase {
    _codataFrom :: [Type calldef ident]
    , _codataTo :: Type calldef ident 
}  deriving (Show, Eq, Read, Generic)

data ProtocolPhrase calldef ident = ProtocolPhrase {
    _protocolFrom :: Type calldef ident 
    , _protocolTo :: ident
}  deriving (Show, Eq, Read, Generic)

data CoprotocolPhrase calldef ident = CoprotocolPhrase {
    _coprotocolFrom :: ident
    , _coprotocolTo :: Type calldef ident
}  deriving (Show, Eq, Read, Generic)

data FunctionDefn pattern letdef calldef ident = FunctionDefn { 
    _funName :: ident
    , _funTypesFromTo :: Maybe ([Type calldef ident], Type calldef ident)
    , _funDefn :: NonEmpty ([Pattern calldef ident], Expr pattern letdef calldef ident) 
} deriving (Show, Eq, Read)

data ProcessDefn patterns letdef calldef ident = ProcessDefn { 
    _procName :: ident
    , _procSeqInChsOutChsTypes :: Maybe ([Type calldef ident], [Type calldef ident], [Type calldef ident])
    , _procDefn :: NonEmpty 
            ( ([Pattern calldef ident], [ident], [ident])
            , ProcessCommands patterns letdef calldef ident) 
} deriving (Show, Eq, Read)

$(concat <$> traverse makeLenses 
    [ ''Stmt
    , ''Defn
    , ''TypeClause
    , ''TypePhrase
    , ''DataPhrase
    , ''CodataPhrase
    , ''ProtocolPhrase
    , ''CoprotocolPhrase
    , ''FunctionDefn
    , ''ProcessDefn
    ]
 )

$(concat <$> traverse makePrisms  
    [ ''Stmt
    , ''Defn
    , ''FunctionDefn
    , ''ProcessDefn
    , ''TypeClause
    , ''TypePhrase
    , ''DataPhrase
    , ''CodataPhrase
    , ''ProtocolPhrase
    , ''CoprotocolPhrase
    ]
 )

$(makeBaseFunctor ''Stmt)
