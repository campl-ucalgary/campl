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

type SymbolTable = [(String, SymEntry SymInfo)]


data SymEntry info = SymEntry {
    _symEntryUniqueTag :: UniqueTag
    , _symEntryInfo :: info
}  deriving Show

newtype TypeTag = TypeTag UniqueTag
  deriving (Eq, Ord)

data SymInfo = 
    SymSeqClause (TypeClauseG TaggedBnfcIdent)
    | SymSeqPhrase (TypePhraseG TaggedBnfcIdent)
    | SymConcClause (TypeClauseG TaggedBnfcIdent)
    | SymConcPhrase (TypePhraseG TaggedBnfcIdent)

    | SymFunDefn (FunctionDefG TaggedBnfcIdent)
    | SymCallSeqPhrase (TypePhraseG TaggedBnfcIdent)
    | SymValType (TypeG TaggedBnfcIdent)
    -- used for look up types
    | SymLocalVar TypeTag

$(concat <$> traverse makePrisms 
    [ ''SymEntry 
    , ''SymInfo ]
 )
$(concat <$> traverse makeLenses 
    [ ''SymEntry 
    , ''SymInfo ]
 )


{-
data SymEntry =
    SymTypeArgVar
    | SymTypeStateVar (SeqClauseG TaggedBnfcIdent)
    | SymSeqClause (SeqClauseG TaggedBnfcIdent)

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
