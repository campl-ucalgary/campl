{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
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
import Optics.State.Operators

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MPLAST.MPLTypeAST hiding (TypePhrase (..))
import MPLAST.MPLPatternAST
import MPLAST.MPLExprAST
import MPLAST.MPLProcessCommandsAST
import MPLAST.MPLProg
import MPLAST.MPLASTIdent

import Data.Functor.Foldable

import GHC.Generics hiding (to)
import Data.Data
import Data.Typeable

import Control.Monad.State

import Text.PrettyPrint.GenericPretty

-- TODO: for obvious reasons, this is not ideal. Perhaps look into
-- autmoatically genrating the required parts with Template haskell.
-- #define MplAstDerivingClause ( Read, Show, Generic, Out, Functor, Foldable, Traversable, Data, Typeable )
-- #define MplDerivingClause ( Read, Show, Generic, Out, Data, Typeable )

#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )

type ProgI ident = Prog (DefnI ident)
type StmtI ident = Stmt (DefnI ident) 
type PatternI ident = Pattern () () ident 
type TypeI ident = Type () ident ident
type DataTypePhraseI ident = TypePhrase () () ident ident
type CodataTypePhraseI ident = TypePhrase () () ident ident
type ProtocolTypePhraseI ident = TypePhrase () () ident ident
type CoprotocolTypePhraseI ident = TypePhrase () () ident ident
type ProcessCommandsI ident = ProcessCommands (PatternI ident) (StmtI ident) () () () ident ident 
type ProcessCommandI ident = ProcessCommand (PatternI ident) (StmtI ident) () () () ident ident 
type ExprI ident = Expr 
    (PatternI ident)
    (StmtI ident)
    ()
    ()
    BnfcIdent


type ObjectDefnI ident = NonEmpty (TypeClause () () () ident ident)
type DataDefnI ident = ObjectDefnI ident
type CodataDefnI ident = ObjectDefnI ident
type ProtocolDefnI ident = ObjectDefnI ident
type CoprotocolDefnI ident = ObjectDefnI ident
type ProcessDefnI ident = ProcessDefn 
    (PatternI ident) 
    (Stmt (DefnI ident)) 
    () 
    (Maybe ([Type () ident ident], [Type () ident ident], [Type () ident ident]))
    () 
    () 
    ident 
    ident

type FunctionDefnI ident = FunctionDefn 
    (PatternI ident) 
    (Stmt (DefnI ident)) 
    () 
    (FunctionDefSigI ident) () ident

type FunctionDefSigI ident = Maybe ([Type () ident ident], Type () ident ident)

newtype DefnI ident = DefnI { 
    _unDefnI :: Defn (DataDefnI ident) (CodataDefnI ident) (ProtocolDefnI ident) (CoprotocolDefnI ident) (FunctionDefnI ident) (ProcessDefnI ident)
    }
  deriving (Show, Eq, Read, Data)

$(makeLenses ''DefnI) 
$(makePrisms ''DefnI)



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


