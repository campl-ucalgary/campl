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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Data

newtype Prog defn = Prog { _prog :: [Stmt defn] }
  deriving (Show, Eq, Read, Semigroup, Monoid, Data)


data Stmt defn = Stmt {
    _stmtDefns :: NonEmpty defn
    , _stmtWhereBindings :: [Stmt defn] 
} deriving (Show, Eq, Read, Functor, Data, Foldable, Traversable)

data Defn datadefn codatadefn protdefn coprotdefn fundefn procdefn =
    DataDefn datadefn
    | CodataDefn codatadefn
    | ProtocolDefn protdefn
    | CoprotocolDefn coprotdefn
    | FunctionDecDefn fundefn
    | ProcessDecDefn procdefn
  deriving (Show, Eq, Read, Data)

type TypeClausesPhrases neighbors phrasecontext calldef ident typevar = 
    NonEmpty (TypeClause neighbors phrasecontext calldef ident typevar)

data ObjectType =
    DataObj
    | CodataObj
    | ProtocolObj
    | CoprotocolObj
  deriving (Show, Eq, Data)

data TypeClause neighbors phrasecontext calldef ident typevar = TypeClause {
    _typeClauseName :: ident 
    , _typeClauseArgs :: [ident]
    , _typeClauseStateVar ::  ident
    , _typeClausePhrases :: [TypePhrase phrasecontext calldef ident typevar]
    , _typeClauseNeighbors :: neighbors
}  deriving (Show, Eq, Read, Data, Generic)

data TypePhrase phrasecontext calldef ident typevar = TypePhrase {
    _typePhraseContext :: phrasecontext
    , _typePhraseName :: ident
    , _typePhraseFrom :: [Type calldef ident typevar]
    , _typePhraseTo :: Type calldef ident typevar
} deriving (Show, Eq, Read, Data, Generic)

{-
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
-}


data FunctionDefn pattern letdef typedef typesig calldef ident = FunctionDefn { 
    _funName :: ident
    -- , _funTypesFromTo :: Maybe ([Type typecalldef ident], Type typecalldef ident)
    , _funTypesFromTo :: typesig
    , _funDefn :: NonEmpty ([pattern], Expr pattern letdef typedef calldef ident) 
} deriving (Show, Eq, Read, Data)

data ProcessDefn patterns letdef typedef typesig seqcalleddef conccalleddef ident chident = ProcessDefn { 
    _procName :: ident
    , _procSeqInChsOutChsTypes :: typesig
    , _procDefn :: NonEmpty 
            ( ([patterns], [chident], [chident])
            , ProcessCommands patterns letdef typedef seqcalleddef conccalleddef ident chident) 
} deriving (Show, Eq, Read, Data)

data Polarity = 
    Input
    | Output
  deriving Show


$(concat <$> traverse makeLenses 
    [ ''Prog
    , ''Stmt
    , ''Defn
    , ''TypeClause
    , ''TypePhrase
    -- , ''TypePhraseFromTo
    -- , ''DataPhrase
    -- , ''CodataPhrase
    -- , ''ProtocolPhrase
    -- , ''CoprotocolPhrase
    , ''FunctionDefn
    , ''ProcessDefn
    ]
 )

$(concat <$> traverse makePrisms  
    [ ''Prog
    , ''Stmt
    , ''Defn
    , ''FunctionDefn
    , ''ProcessDefn
    , ''TypeClause
    , ''TypePhrase
    , ''ObjectType
    , ''Polarity
    ]
 )

$(makeBaseFunctor ''Stmt)
