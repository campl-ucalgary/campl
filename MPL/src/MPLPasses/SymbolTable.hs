{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.SymbolTable where

import MPLAST.MPLProgI
import MPLAST.MPLASTCore
import MPLPasses.Unification

import Optics
import Optics.State.Operators

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.RWS

import Data.Map ( Map (..) )
import qualified Data.Map as Map


data SymEntry info = SymEntry {
    _symEntryUniqueTag :: UniqueTag
    , _symEntryInfo :: info
}  deriving Show

data SymInfo = 
    SymSeqClause (TypeClauseG TaggedBnfcIdent)
    | SymSeqPhrase (TypePhraseG TaggedBnfcIdent)
    | SymConcClause (TypeClauseG TaggedBnfcIdent)
    | SymConcPhrase (TypePhraseG TaggedBnfcIdent)

    | SymFunDefn (FunctionDefG TaggedBnfcIdent TypeTag)
    | SymCallSeqPhrase (TypePhraseG TaggedBnfcIdent)
    -- used for look up types
    | SymLocalSeqVar TypeTag
  deriving Show

$(concat <$> traverse makePrisms 
    [ ''SymEntry 
    , ''SymInfo ]
 )
$(concat <$> traverse makeLenses 
    [ ''SymEntry 
    , ''SymInfo ]
 )

type SymbolTable = [(String, SymEntry SymInfo)] 
{-
newtype SymbolTable = SymbolTable { 
    _unSymbolTable :: [(String, SymEntry SymInfo)] }

$(makeLenses ''SymbolTable)
-- flip the append so that when working 
-- with the writer monad, we get the
-- correct look up properties i.e.,
-- things put later in the symbol table
-- are looked up first.
instance Semigroup SymbolTable where
    SymbolTable a <> SymbolTable b = SymbolTable (b <> a)

instance Monoid SymbolTable where
    mempty = SymbolTable []
-}
