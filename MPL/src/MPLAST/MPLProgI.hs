{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module MPLAST.MPLProgI where

import MPLIdent

import Optics.TH
import Optics.Prism
import Optics.Operators

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MPLAST.MPLTypeAST
import MPLAST.MPLPatternAST
import MPLAST.MPLExprAST
import MPLAST.MPLProcessCommandsAST
import MPLAST.MPLProg

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

-- TODO: for obvious reasons, this is not ideal. Perhaps look into
-- autmoatically genrating the required parts with Template haskell.
-- #define MplAstDerivingClause ( Read, Show, Generic, Out, Functor, Foldable, Traversable, Data, Typeable )
-- #define MplDerivingClause ( Read, Show, Generic, Out, Data, Typeable )

#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )

type BnfcIdent = (String, (Int, Int))
type ProgI = Prog StmtI
type StmtI = Stmt (Defn BnfcIdent BnfcIdent)
type PatternI = Pattern BnfcIdent BnfcIdent 
type TypeI = Type BnfcIdent BnfcIdent 
type ExprI = Expr 
    (Pattern BnfcIdent BnfcIdent) 
    (Stmt (Defn BnfcIdent BnfcIdent))
    BnfcIdent 
    BnfcIdent
type ProcessCommandsI = ProcessCommands PatternI StmtI BnfcIdent BnfcIdent

data Defn def var =
    DataDefn  { _seqTypeClause :: NonEmpty (SeqTypeClause def var) }
    | CodataDefn  { _seqTypeClause :: NonEmpty (SeqTypeClause def var) }
    | ProtocolDefn  { _concTypeClause :: NonEmpty (ConcTypeClause def var) }
    | CoprotocolDefn  { _concTypeClause :: NonEmpty (ConcTypeClause def var) }
    | FunctionDefn  { _funName :: def
                    , _funTypesFromTo :: Maybe ([Type def var], Type def var)
                    , _funDefn :: NonEmpty ([Pattern def var], Expr (Pattern def var) (Stmt (Defn def var)) def var) }
    | ProcessDefn  { _procName :: def
                    , _procSeqInChsOutChsTypes :: Maybe ([Type def var], [Type def var], [Type def var])
                    , _procDefn :: NonEmpty 
                        ( ([Pattern def var], [var], [var])
                        , ProcessCommands (Pattern def var) (Stmt (Defn def var)) def var) }

data SeqTypeClause def var = SeqTypeClause {
    _seqTypeClauseName :: def
    , _seqTypeClauseArgs :: [var]
    , _seqTypeClauseStateVar :: var
    , _seqTypePhrases :: [SeqTypePhrase def var]
}

data SeqTypePhrase def var = SeqTypePhrase {
    _seqTypePhraseName :: def
    , _seqTypePhraseFrom :: [Type def var]
    , _seqTypePhraseTo :: var
}

data ConcTypeClause def var = ConcTypeClause {
    _concTypeClauseName :: def
    , _concTypeClauseArgs :: [var]
    , _concTypeClauseStateVar :: var
    , _concTypePhrases :: [ConcTypePhrase def var]
}

data ConcTypePhrase def var = ConcTypePhrase {
    _concTypePhraseName :: def
    , _concTypePhraseFrom :: var
    , _concTypePhraseTo :: var
}

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''Defn ]
 )


$(concat <$> traverse makePrisms 
    [ ''Defn ]
 )
