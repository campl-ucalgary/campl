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

data SymEntry =
    SeqDec (SeqGraphPhrase TaggedBnfcIdent)

data SymbolTableState = SymbolTableState {
    _uniqueTagGenerator :: UniqueTag
    , _scopeLookup :: [[(String, (UniqueTag, SymEntry))]]
    , _symbolTable :: Map UniqueTag SymEntry
}

$(concat <$> traverse makeClassy 
    [ ''SymbolTableState]
 )
$(concat <$> traverse makePrisms 
    [ ''SymEntry]
 )

{-
freshUniqueTag ::
    ( MonadState c m
    , HasSymbolTableState c ) => 
    m UniqueTag
freshUniqueTag = 
    uniqueTagGenerator <<%= succ

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
