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

data Defn pattern letdef calldef decdef var concvar =
    DataDefn (NonEmpty (TypeClause (TypePhrase (DataPhrase calldef var) decdef var) decdef var))
    | CodataDefn (NonEmpty (TypeClause 
        (TypePhrase (CodataPhrase calldef var) decdef var) decdef var))

    | ProtocolDefn (NonEmpty 
        (TypeClause (TypePhrase (ProtocolPhrase calldef var) decdef var) decdef var))
    | CoprotocolDefn (NonEmpty 
        (TypeClause (TypePhrase (CoprotocolPhrase calldef var) decdef var) decdef var))
    | FunctionDecDefn (FunctionDefn pattern letdef decdef var)

    | ProcessDecDefn (ProcessDefn pattern letdef decdef var concvar)
  deriving (Show, Eq, Read)

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


data FunctionDefn pattern letdef def var = FunctionDefn { 
    _funName :: def
    , _funTypesFromTo :: Maybe ([Type def var], Type def var)
    , _funDefn :: NonEmpty ([Pattern def var], Expr pattern letdef def var) 
} deriving (Show, Eq, Read)

data ProcessDefn patterns letdef def var concvar = ProcessDefn { 
    _procName :: def
    , _procSeqInChsOutChsTypes :: Maybe ([Type def var], [Type def var], [Type def var])
    , _procDefn :: NonEmpty 
            ( ([Pattern def var], [concvar], [concvar])
            , ProcessCommands patterns letdef def var concvar) 
} deriving (Show, Eq, Read)

$(concat <$> traverse makeLenses 
    [ ''Stmt
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
