{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.SymbolTable where

import MPLAST.MPLProgI
import MPLAST.MPLASTCore

import Optics
import Optics.State.Operators

import Control.Monad.State
import Control.Monad.Except


import Data.Map ( Map (..) )
import qualified Data.Map as Map

{-
data SymEntry =
    SymTypeArgVar
    | SymTypeStateVar (SeqClauseG TaggedBnfcIdent)
    | SymSeqClause (SeqClauseG TaggedBnfcIdent)

type Scope = [(String, (UniqueTag, SymEntry))]
type SymbolTable = [Scope]
{-
withScope  :: 
    ( MonadState c m
    , HasSymbolTableState c ) => 
    SymEntry ->
    m ()

insertSymbolTable :: 
    ( MonadState c m
    , HasSymbolTableState c ) => 
    SymEntry ->
    m ()
insertSymbolTable entry = do
    tag <- freshUniqueTag
    symbolTable %= Map.insert tag entry
    -}
    -}
