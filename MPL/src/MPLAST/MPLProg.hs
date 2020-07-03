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
module MPLAST.MPLProg where

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

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

-- TODO: for obvious reasons, this is not ideal. Perhaps look into
-- autmoatically genrating the required parts with Template haskell.
-- #define MplAstDerivingClause ( Read, Show, Generic, Out, Functor, Foldable, Traversable, Data, Typeable )
-- #define MplDerivingClause ( Read, Show, Generic, Out, Data, Typeable )

#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )


newtype Prog a = Prog [Stmt a]

data Stmt a = Stmt {
    _stmtDefn :: Defn a
    , _stmtDefns :: [Defn a]
    , _stmtWhereBindings :: [Stmt a] 
}  

_MutuallyRecursiveStmts :: Prism' (Stmt a) ((Defn a, Defn a), [Defn a])
_MutuallyRecursiveStmts = prism' embed match
  where
    embed ((a, b), rst) = Stmt a (b:rst) []
    match (Stmt a (b:bs) whs) = Just ((a,b), bs)
    match _ = Nothing
    

data Defn a =
    DataDefn  { _seqTypeClause :: NonEmpty (SeqTypeClause a) }
    | CodataDefn  { _seqTypeClause :: NonEmpty (SeqTypeClause a) }
    | ProtocolDefn  { _concTypeClause :: NonEmpty (SeqTypeClause a) }
    | CoprotocolDefn  { _concTypeClause :: NonEmpty (SeqTypeClause a) }
    | FunctionDefn  { _funName :: a
                    , _funTypesFromTo :: Maybe ([Type a a], Type a a)
                    , _funDefn :: NonEmpty (Pattern a a, Expr (Defn a) a a) }
    | ProcessDefn  { _procName :: a
                    , _procSeqInChsOutChsTypes :: Maybe ([Type a a], [Type a a], [Type a a])
                    , _procDefn :: NonEmpty (([Pattern a a], [a], [a]), ProcessCommands (Defn a) a a) }

data SeqTypeClause a = SeqTypeClause {
    _seqTypeClauseKindFrom :: Type a a
    , _seqTypeClauseKindTo :: Type a a
    , _seqTypePhrases :: [SeqTypePhrase a]
}

data SeqTypePhrase a = SeqTypePhrase {
    _seqTypePhraseName :: a
    , _seqTypePhraseFrom :: [Type a a]
    , _seqTypePhraseTo :: Type a a
}

data ConTypeClause a = ConcTypeClause {
    _concTypeClauseKindFrom :: Type a a
    , _concTypeClauseKindTo :: Type a a
    , _concTypePhrases :: [ConcTypePhrase a]
}

data ConcTypePhrase a = ConcTypePhrase {
    _concTypePhraseName :: a
    , _concTypePhraseFrom :: Type a a
    , _concTypePhraseTo :: Type a a
}

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''Defn ]
 )

$(concat <$> traverse makeLenses 
    [ ''Stmt ]
 )

$(concat <$> traverse makePrisms 
    [ ''Defn 
    ]
 )
