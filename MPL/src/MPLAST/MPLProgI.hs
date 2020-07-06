{-# LANGUAGE TemplateHaskell #-}
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

import Optics

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MPLAST.MPLTypeAST hiding (TypePhrase (..))
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

type ProgI = Prog DefnI
type StmtI = Stmt DefnI
type PatternI = Pattern BnfcIdent BnfcIdent 
type TypeI = Type BnfcIdent BnfcIdent 
type DataTypePhraseI = TypePhrase (DataPhrase BnfcIdent BnfcIdent) BnfcIdent BnfcIdent
type SeqTypeClauseI = SeqTypeClause BnfcIdent BnfcIdent BnfcIdent
type SeqTypePhraseI = SeqTypePhrase BnfcIdent BnfcIdent BnfcIdent
type ConcTypeClauseI =  ConcTypeClause BnfcIdent BnfcIdent BnfcIdent
type ConcTypePhraseI = ConcTypePhrase BnfcIdent BnfcIdent BnfcIdent
type ExprI = Expr 
    (Pattern BnfcIdent BnfcIdent) 
    StmtI
    BnfcIdent 
    BnfcIdent

type ProcessCommandsI = ProcessCommands PatternI StmtI BnfcIdent BnfcIdent BnfcIdent
type ProcessCommandI = ProcessCommand PatternI StmtI BnfcIdent BnfcIdent BnfcIdent

newtype BnfcIdent = BnfcIdent { stringPos :: (String, (Int, Int)) }
  deriving (Show, Eq, Read, Ord)

newtype DefnI = DefnI { 
    _unDefnI :: Defn (Pattern BnfcIdent BnfcIdent) (Stmt DefnI) BnfcIdent BnfcIdent BnfcIdent BnfcIdent
    }
  deriving (Show, Eq, Read)
$(makeLenses ''DefnI)

{-
data DefnI =
    DataDefnI  { _seqTypeClauseI :: NonEmpty (SeqTypeClause BnfcIdent BnfcIdent) }
    | CodataDefnI  { _seqTypeClauseI :: NonEmpty (SeqTypeClause BnfcIdent BnfcIdent) }
    | ProtocolDefnI  { _concTypeClauseI :: NonEmpty (ConcTypeClause BnfcIdent BnfcIdent) }
    | CoprotocolDefnI  { _concTypeClauseI :: NonEmpty (ConcTypeClause BnfcIdent BnfcIdent) }

    | FunctionDefnI (FunctionDefn PatternI StmtI BnfcIdent BnfcIdent)

    | ProcessDefnI (ProcessDefn 
        PatternI 
        StmtI 
        BnfcIdent 
        BnfcIdent
        BnfcIdent)
        -}

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''DefnI ]
 )

$(concat <$> traverse makePrisms 
    [ ''DefnI ]
 )

$(makeClassy ''BnfcIdent)
