{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.List

import Data.Functor.Foldable

data SymEntry info = SymEntry {
    _symEntryUniqueTag :: UniqueTag
    , _symEntryPosition :: (Int,Int)
    , _symEntryInfo :: info
}  deriving Show

data SymSeqConcTypeInfo = 
    SymSeqClause 
        (TypeClauseG TaggedBnfcIdent)
    | SymConcClause 
        (TypeClauseG TaggedBnfcIdent)
    | SymTypeVar 
  deriving Show 

data SymCallInfo = 
    SymPhrase (TypePhraseG TaggedBnfcIdent)
    | SymCall 
        -- (type of the expression, free variables)
        SymCallTypeVar 
        (FunctionCallValueKnot 
            TaggedBnfcIdent 
            TypeTag)
  deriving Show

data SymInfo = 
    SymSeqCall SymCallInfo
    | SymConcCall SymCallInfo
    | SymConcLocalCh TypeTag

    | SymSeqConcType SymSeqConcTypeInfo
  deriving Show

data SymCallTypeVar =
    SymCallDummyTypeVar TypeTag
    | SymCallExplicitType (TypeG TaggedBnfcIdent)
  deriving Show

$(makePrisms ''SymEntry)
$(concat <$> traverse makeClassyPrisms 
    [ ''SymInfo 
    , ''SymCallInfo 
    , ''SymSeqConcTypeInfo 
    , ''SymCallTypeVar 
    ]
 )
$(concat <$> traverse makeLenses 
    [ ''SymEntry 
    , ''SymInfo ]
 )

instance AsSymSeqConcTypeInfo SymInfo where
    _SymSeqConcTypeInfo = _SymSeqConcType % _SymSeqConcTypeInfo 


type SymbolTable = [(String, SymEntry SymInfo)]

querySequentialPhrases = undefined
querySeqCallFuns = undefined
sequentialClausesPredicate = undefined
queryLocalSeqVar = undefined
queryChecks = undefined
symbolTableQuery = undefined

{-
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
    -} 

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
                map (second 
                        (\n -> _SymEntry #
                            ( n ^. typeClauseName % uniqueTag
                            , n ^. typeClauseName % taggedBnfcIdentPos
                            , bool (_SymConcClause # n) (_SymSeqClause # n) isseq 
                            )
                        )
                    ) 
                        (collectClauseGraphClauses graph)
                ++ map (second (\n -> _SymEntry #
                            ( n ^. typePhraseName % uniqueTag
                            , n ^. typePhraseName % taggedBnfcIdentPos
                            , bool (_SymConcCall # _SymPhrase # n) (_SymSeqCall # _SymPhrase # n) isseq )))
                         (collectClauseGraphPhrases graph)
              where
                isseq = liftA2 (||) (DataObj==) (CodataObj==) graphtype 
                graphtype = graph ^. clauseGraphObjectType 


        collectClauseGraphClauses graph = map f $ NE.toList $ graph ^. clauseGraphSpine
          where
            f = view (typeClauseName % taggedBnfcIdentName ) &&& id

        collectClauseGraphPhrases graph = concatMap f $ NE.toList $ graph ^. clauseGraphSpine
          where
            f graph = map g (graph ^. typeClausePhrases)
            g = view (typePhraseName % taggedBnfcIdentName) &&& id 

    collectSymEntries (FunctionDecDefG graph) = [
        ( graph ^. funName % taggedBnfcIdentName
        , _SymEntry # 
            ( graph ^. funName % uniqueTag
            , graph ^. funName % taggedBnfcIdentPos 
            , _SymSeqCall # 
                _SymCall # 
                    ( SymCallExplicitType 
                    $ typeGTypeTagToTypeG 
                    $ graph ^. funTypesFromTo
                    , FunctionKnot graph
                    ) 
            ) 
        )
        ]

    collectSymEntries _ = error "proceses todo"

data SymbolTableQueryState =  SymbolTableQueryState {
    _symbolTableQueryStateBnfcIdent :: Maybe BnfcIdent
    , _symbolTableQueryStateFailure :: Bool
}

$(makeLenses ''SymbolTableQueryState)

newtype SymbolTableQueryStateT m a =
    SymbolTableQueryStateT { unSymbolTableQueryT :: StateT SymbolTableQueryState m a }
  deriving 
    ( Functor 
    , Applicative
    , Monad
    , MonadState SymbolTableQueryState
    , MonadTrans )

type SymbolTableQuery symtab a = symtab -> SymbolTableQueryStateT GraphGenCore a

runSymbolTableQuery ::  
    SymbolTableQuery SymbolTable a -> 
    SymbolTable -> 
    GraphGenCore a
runSymbolTableQuery query symtab = 
    evalStateT 
    (unSymbolTableQueryT (query symtab)) 
    (SymbolTableQueryState Nothing False)

{-
symbolTableQuery ::
    SymbolTableQuery [(String, SymEntry a)] (Maybe (SymEntry a))
symbolTableQuery symtab = do
    queryfail <- guse symbolTableQueryStateFailure 
    return $ bool (snd <$> listToMaybe symtab) Nothing queryfail
-}

queryNotInScopeCheck ::
    SymbolTableQuery [a] [a]
queryNotInScopeCheck symtab = do
    ident <- guse symbolTableQueryStateBnfcIdent 
    flip (maybe (return symtab)) ident $ \ident -> do
        let isnull = null symtab
        symbolTableQueryStateFailure %= (||isnull)
        lift $ tell $ bool [] [NotInScope ident] isnull
        return symtab

{-
queryAmbiguousLookupCheck :: 
    SymbolTableQuery [(String, SymEntry a)] [(String, SymEntry a)]
queryAmbiguousLookupCheck symtab = do
    ident <- guse symbolTableQueryStateBnfcIdent 
    flip (maybe (return symtab)) ident $ \ident -> do
        let multiplelookups = length symtab >= 2
        symbolTableQueryStateFailure %= (||multiplelookups)
        lift 
            $ tell 
            $ bool [] 
                [ AmbiguousLookup ident  
                $ map (\(str, SymEntry _ pos _) -> BnfcIdent (str,pos)) symtab 
                ] multiplelookups
        return symtab
 
 -}
queryBnfcIdentName ::
    BnfcIdent -> 
    [(String, SymEntry info)] -> 
    GraphGenCore (Maybe (SymEntry info))
queryBnfcIdentName ident symtab = do
    tell $ bool [] [NotInScope ident] notinscope
    {-
    tell $ bool [] 
        [ AmbiguousLookup ident 
        $ map (\(str, SymEntry _ pos _) -> BnfcIdent (str,pos)) symtab
        ] 
        ambig
        -}
    return $ bool (Just $ snd $ head namequeries) Nothing notinscope

  where
    namequeries = filter ( (ident ^. bnfcIdentName==) . fst ) symtab
    notinscope = null namequeries


lookupBnfcIdent ::
    BnfcIdent ->
    [(String, a)] ->
    Maybe a
lookupBnfcIdent ident = fmap snd . find ((ident ^. bnfcIdentName  ==) . fst) 

{-
queryChecks ::
    SymbolTableQuery [(String, SymEntry a)] [(String, SymEntry a)]
queryChecks = queryNotInScopeCheck >=> queryAmbiguousLookupCheck
-}
    {-
    tell $ bool [] [AmbiguousLookup $ map (\(str, SymEntry _ pos _) -> BnfcIdent (str,pos)) symtable ] twoormoreelems
    return $ bool (listToMaybe symtable) Nothing (twoormoreelems || zeroelems)
  where
    twoormoreelems = length symtable >= 2
    zeroelems = null symtable
    -}

{-
queryBnfcIdentName :: 
    BnfcIdent ->
    SymbolTableQuery [(String, b)] [(String, b)]
queryBnfcIdentName ident symtab = do
    symbolTableQueryStateBnfcIdent .= Just ident
    return $ filter 
        ( (ident ^. bnfcIdentName==) . fst ) 
        symtab
        -}

{-
querySequentialPhrases ::
    SymbolTableQuery ()
querySequentialPhrases = 
    liftPredicateQuery
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

querySeqCallFuns ::
    SymbolTableQuery ()
querySeqCallFuns = 
    liftPredicateQuery
        ( has ( 
                _2
                % symEntryInfo 
                % _SymCall
                ))

queryLocalSeqVar ::
    SymbolTableQuery ()
queryLocalSeqVar = liftPredicateQuery ( has (_2 % symEntryInfo % _SymLocalSeqVar) )

liftPredicateQuery ::
    ((String, SymEntry SymInfo) -> Bool) ->
    SymbolTableQuery () 
liftPredicateQuery p = 
    symbolTableQueryStateSymbolTable %= filter p


sequentialClausesPredicate :: 
    (String, SymEntry SymInfo) -> 
    Bool 
sequentialClausesPredicate = 
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
                -}

{-
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
    undefined
    -- tell $ bool [] [AmbiguousLookup $ map (\(str, SymEntry _ pos _) -> BnfcIdent (str,pos)) symtable ] twoormoreelems
    return $ bool (listToMaybe symtable) Nothing (twoormoreelems || zeroelems)
  where
    twoormoreelems = length symtable >= 2
    zeroelems = null symtable

querySymbolTableSeqCallFuns ::
    SymbolTable -> 
    SymbolTable 
querySymbolTableSeqCallFuns = 
    filter (has ( 
                _2
                % symEntryInfo 
                % _SymCall
                ))

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
        -}
