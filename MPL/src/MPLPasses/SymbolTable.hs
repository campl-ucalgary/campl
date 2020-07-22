{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module MPLPasses.SymbolTable where

import MPLAST.MPLProgI
import MPLAST.MPLASTCore
import MPLPasses.Unification
import MPLPasses.GraphGenCore
import MPLPasses.TieDefnsErrors

import Optics
import Optics.State.Operators

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.RWS

import Data.Map ( Map (..) )
import qualified Data.Map as Map

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Control.Arrow 
import Data.Functor.Contravariant
import Data.Bool
import Data.Maybe
import Control.Applicative


data SymEntry info = SymEntry {
    _symEntryUniqueTag :: UniqueTag
    , _symEntryInfo :: info
}  deriving Show

data SymInfo = 
    -- | type clauses lookups
    SymClause (TypeClauseG TaggedBnfcIdent)
    | SymPhrase (TypePhraseG TaggedBnfcIdent)
    -- | used for keeping track of which type is which
    | SymTypeVar 

    -- | Function lookups
    | SymFunDefn (FunctionDefG TaggedBnfcIdent TypeTag)
    | SymCallSeqPhrase (TypePhraseG TaggedBnfcIdent)
    | SymCallMatchedDestructor (TypeG TaggedBnfcIdent) (TypePhraseG TaggedBnfcIdent)
        -- | useful for lookup up the types as a local vairable
    | SymLocalSeqVar TypeTag
        
    -- process lookups
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

_SymEntryTypeClauseObjType = 
    symEntryInfo 
    % _SymClause 
    % typeClauseNeighbors 
    % clauseGraph 
    % clauseGraphObjectType

_SymEntryTypePhraseObjType = 
    symEntryInfo 
    % _SymPhrase 
    % typePhraseContext
    % phraseParent 
    % typeClauseNeighbors 
    % clauseGraph 
    % clauseGraphObjectType

class CollectSymEntries a where
    collectSymEntries :: a -> SymbolTable

instance CollectSymEntries def => CollectSymEntries (Stmt def) where
    collectSymEntries (Stmt defs _) = concatMap collectSymEntries defs

instance CollectSymEntries (DefnG TaggedBnfcIdent TypeTag) where
    collectSymEntries (ObjectG graph) = collectClauseGraphSymbolTable graph
      where
        collectClauseGraphSymbolTable graph = f graph
          where
            f graph =
                map (second (review _SymEntry <<< view (typeClauseName % uniqueTag) 
                        &&& SymClause)) (collectClauseGraphClauses graph)
                ++ map (second (review _SymEntry <<< view (typePhraseName % uniqueTag) 
                        &&& SymPhrase)) (collectClauseGraphPhrases graph)

        collectClauseGraphClauses graph = map f $ NE.toList $ graph ^. clauseGraphSpine
          where
            f = view (typeClauseName % taggedBnfcIdentName ) &&& id

        collectClauseGraphPhrases graph = concatMap f $ NE.toList $ graph ^. clauseGraphSpine
          where
            f graph = map g (graph ^. typeClausePhrases)
            g = view (typePhraseName % taggedBnfcIdentName) &&& id 

    collectSymEntries (FunctionDecDefG graph) = [
        ( graph ^. funName % taggedBnfcIdentName
        , SymEntry (graph ^. funName % uniqueTag) $ SymFunDefn graph)
        ]

    collectSymEntries _ = error "proceses todo"

querySymbolTableBnfcIdentName :: 
    BnfcIdent -> 
    SymbolTable -> 
    GraphGenCore SymbolTable
querySymbolTableBnfcIdentName ident symtab = do
    let entries = filter f symtab
    tell $ bool [] 
        [_NotInScope # ident] 
        (null entries)
    return entries
  where
    f ~(str, entry) = str == ident ^. bnfcIdentName

querySymbolTableSequentialPhrases :: 
    SymbolTable -> 
    SymbolTable 
querySymbolTableSequentialPhrases = 
    filter
        ( liftA2 (||) 
            (has ( 
                _2
                % _SymEntryTypePhraseObjType
                % _DataObj ))
            (has (
                _2
                % _SymEntryTypePhraseObjType
                % _CodataObj
                ))
        )
    

ambiguousLookupCheck :: 
    SymbolTable -> 
    GraphGenCore (Maybe (String, SymEntry SymInfo))
ambiguousLookupCheck symtable = do
    tell $ bool [] [AmbiguousLookup] twoormoreelems
    return $ bool (listToMaybe symtable) Nothing (twoormoreelems || zeroelems)
  where
    twoormoreelems = length symtable >= 2
    zeroelems = null symtable


querySymbolTableSequentialClauses ::
    SymbolTable -> 
    SymbolTable 
querySymbolTableSequentialClauses = 
    filter
        ( liftA2 (||) 
            (has ( 
                _2
                % _SymEntryTypeClauseObjType
                % _DataObj ))
            (has (
                _2
                % _SymEntryTypeClauseObjType
                % _CodataObj
                ))
        )

querySymbolTableConcurrentClauses ::
    SymbolTable -> 
    SymbolTable 
querySymbolTableConcurrentClauses = 
    filter
        ( liftA2 (||) 
            (has ( 
                _2
                % _SymEntryTypeClauseObjType
                % _ProtocolObj ))
            (has (
                _2
                % _SymEntryTypeClauseObjType
                % _CoprotocolObj 
                ))
        )
