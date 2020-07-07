{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
module MPLAST.MPLProg where

import MPLAST.MPLExprAST
import MPLAST.MPLTypeAST hiding (TypePhrase (..))
import MPLAST.MPLPatternAST
import MPLAST.MPLProcessCommandsAST

import Optics

import GHC.Generics 

import Data.List.NonEmpty

newtype Prog defn = Prog [Stmt defn]
  deriving (Show, Eq, Read)

data Stmt defn = Stmt {
    _stmtDefns :: NonEmpty defn
    , _stmtWhereBindings :: [Stmt defn] 
} deriving (Show, Eq, Read)

-- Explanation of the type variables:
-- pattern: the type of a pattern (swapped to () after compilation of pattern matching)
-- letdef: the type for a let definition (swapped to () after lambda lifting)
-- calldef: identifier for CALLING an already declared constructor, funciton, etc
-- var: identifier for a variable
-- concvar: identifier for a concurrent term 
data Defn pattern letdef calldef decdef metavar var concvar =
    DataDefn (NonEmpty (TypeClausePhrase (DataPhrase calldef metavar) decdef metavar))
    | CodataDefn (NonEmpty (TypeClausePhrase (CodataPhrase calldef metavar) decdef metavar))
    | ProtocolDefn (NonEmpty (TypeClausePhrase (ProtocolPhrase calldef metavar) decdef metavar))
    | CoprotocolDefn (NonEmpty (TypeClausePhrase (CoprotocolPhrase calldef metavar) decdef metavar))
    | FunctionDecDefn (FunctionDefn pattern letdef calldef decdef metavar var)
    | ProcessDecDefn (ProcessDefn pattern letdef calldef decdef metavar var concvar)
  deriving (Show, Eq, Read)

type TypeClausePhrase phrase decdef var = TypeClause (TypePhrase phrase decdef var) decdef var

data TypeClause phrase decdef var = TypeClause {
    _typeClauseName :: decdef
    , _typeClauseArgs :: [var]
    , _typeClauseStateVar :: var
    , _typeClausePhrases :: [phrase]
}  deriving (Show, Eq, Read, Generic)

data TypePhrase phrase decdef var = TypePhrase {
    _typePhraseName :: decdef
    , _typePhraseType :: phrase
}  deriving (Show, Eq, Read, Generic)

data DataPhrase calldef var = DataPhrase {
    _dataFrom :: [Type calldef var]
    , _dataTo :: var
}  deriving (Show, Eq, Read, Generic)

data CodataPhrase calldef var = CodataPhrase {
    _codataFrom :: [Type calldef var]
    , _codataTo :: Type calldef var
}  deriving (Show, Eq, Read, Generic)

data ProtocolPhrase calldef var = ProtocolPhrase {
    _protocolFrom :: Type calldef var
    , _protocolTo :: var
}  deriving (Show, Eq, Read, Generic)

data CoprotocolPhrase calldef var = CoprotocolPhrase {
    _coprotocolFrom :: var
    , _coprotocolTo :: Type calldef var
}  deriving (Show, Eq, Read, Generic)


data FunctionDefn pattern letdef calldef decdef metavar var = FunctionDefn { 
    _funName :: decdef
    , _funTypesFromTo :: Maybe ([Type calldef metavar], Type calldef metavar)
    , _funDefn :: NonEmpty ([Pattern calldef var], Expr pattern letdef calldef var) 
} deriving (Show, Eq, Read)

data ProcessDefn patterns letdef calldef decdef metavar var concvar = ProcessDefn { 
    _procName :: decdef
    , _procSeqInChsOutChsTypes :: Maybe ([Type calldef metavar], [Type calldef metavar], [Type calldef metavar])
    , _procDefn :: NonEmpty 
            ( ([Pattern calldef var], [concvar], [concvar])
            , ProcessCommands patterns letdef calldef var concvar) 
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
