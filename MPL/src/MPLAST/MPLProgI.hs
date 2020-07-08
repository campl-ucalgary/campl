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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Data.Functor.Foldable

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

-- TODO: for obvious reasons, this is not ideal. Perhaps look into
-- autmoatically genrating the required parts with Template haskell.
-- #define MplAstDerivingClause ( Read, Show, Generic, Out, Functor, Foldable, Traversable, Data, Typeable )
-- #define MplDerivingClause ( Read, Show, Generic, Out, Data, Typeable )

#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )

type ProgI ident = Prog (DefnI ident)
type StmtI ident = Stmt (DefnI ident) 
type PatternI ident = Pattern () ident 
type TypeI ident = Type () ident 
type DataTypePhraseI ident = TypePhrase () (DataPhrase () ident) ident 
type CodataTypePhraseI ident = TypePhrase () (CodataPhrase () ident) ident 
type ProtocolTypePhraseI ident = TypePhrase () (ProtocolPhrase () ident) ident 
type CoprotocolTypePhraseI ident = TypePhrase () (CoprotocolPhrase () ident) ident 
type ProcessCommandsI ident = ProcessCommands (PatternI ident) (StmtI ident) () ident 
type ProcessCommandI ident = ProcessCommand (PatternI ident) (StmtI ident) () ident 
type ExprI ident = Expr 
    (PatternI ident)
    (StmtI ident)
    ()
    BnfcIdent


newtype BnfcIdent = BnfcIdent { stringPos :: (String, (Int, Int)) }
  deriving (Show, Eq, Read, Ord)

type ObjectDefnI phrase ident = TypeClausesPhrases () () (phrase () ident) ident
type DataDefnI ident = ObjectDefnI DataPhrase ident
type CodataDefnI ident = ObjectDefnI CodataPhrase ident
type ProtocolDefnI ident = ObjectDefnI ProtocolPhrase ident
type CoprotocolDefnI ident = ObjectDefnI CoprotocolPhrase ident
type ProcessDefnI ident = ProcessDefn (PatternI ident) (Stmt (DefnI ident)) () ident
type FunctionDefnI ident = FunctionDefn (PatternI ident) (Stmt (DefnI ident)) () ident

newtype DefnI ident = DefnI { 
    _unDefnI :: Defn (DataDefnI ident) (CodataDefnI ident) (ProtocolDefnI ident) (CoprotocolDefnI ident) (FunctionDefnI ident) (ProcessDefnI ident)
    }
  deriving (Show, Eq, Read)
$(makeLenses ''DefnI)


$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''DefnI ]
 )

$(concat <$> traverse makePrisms 
    [ ''DefnI ]
 )

$(makeClassy ''BnfcIdent)

type DefnIBnfc = DefnI BnfcIdent
type ProgIBnfc = ProgI BnfcIdent
type StmtIBnfc = StmtI BnfcIdent
type PatternIBnfc = PatternI BnfcIdent
type TypeIBnfc = TypeI BnfcIdent
type DataTypePhraseIBnfc = DataTypePhraseI BnfcIdent
type CodataTypePhraseIBnfc = CodataTypePhraseI BnfcIdent
type ProtocolTypePhraseIBnfc = ProtocolTypePhraseI BnfcIdent
type CoprotocolTypePhraseIBnfc = CoprotocolTypePhraseI BnfcIdent
type ProcessCommandsIBnfc = ProcessCommandsI BnfcIdent
type ProcessCommandIBnfc = ProcessCommandI BnfcIdent
type ExprIBnfc = ExprI BnfcIdent 

data TaggedBnfcIdent = TaggedBnfcIdent {
    _taggedBnfcIdentTag :: UniqueTag
    , _taggedBnfcIdentBnfcIdent :: BnfcIdent
} deriving (Show, Eq, Read, Ord)

newtype UniqueTag = UniqueTag Int
  deriving (Show, Eq, Ord, Read, Enum)


$(makeClassy ''UniqueTag)

$(concat <$> traverse makeLenses
    [ ''TaggedBnfcIdent
    ]
 )
$(concat <$> traverse makePrisms
    [ ''TaggedBnfcIdent
    ]
 )

