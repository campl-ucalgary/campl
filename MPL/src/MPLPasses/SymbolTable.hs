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

data SymCallInfo knot = 
    SymPhrase (TypePhraseG TaggedBnfcIdent)
    | SymCall 
        -- (type of the expression, free variables)
        SymCallTypeVar 
        knot 
  deriving Show

data SymInfo = 
    SymSeqCall (SymCallInfo (FunctionCallValueKnot TaggedBnfcIdent TypeTag TaggedChIdent))
    | SymConcCall (SymCallInfo (ProcessCallValueKnot TaggedBnfcIdent TypeTag TaggedChIdent))
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

class CollectSymEntries a where
    collectSymEntries :: a -> SymbolTable

instance CollectSymEntries def => CollectSymEntries (Stmt def) where
    collectSymEntries (Stmt defs _) = concatMap collectSymEntries defs

instance CollectSymEntries (DefnG TaggedBnfcIdent TypeTag TaggedChIdent) where
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

lookupsSeqPhrases ::    
    Functor f => 
    f BnfcIdent ->
    SymbolTable ->
    f (Maybe (TypePhraseG TaggedBnfcIdent))
lookupsSeqPhrases idents symtab = fmap f idents
  where
    f ident = fmap (view symEntryInfo)
                $ lookupBnfcIdent ident 
                $ mapMaybe ( traverseOf (_2 % symEntryInfo)
                                    ( preview (_SymSeqCall % _SymPhrase) )) symtab

querySeqClauses ::
    AsSymSeqConcTypeInfo a =>
    SymbolTable -> 
    [(String, SymEntry a)]
querySeqClauses = mapMaybe 
    ( traverseOf (_2 % symEntryInfo ) ( fmap (review _SymSeqClause) . preview ( _SymSeqClause)))

queryClauses ::
    AsSymSeqConcTypeInfo a =>
    SymbolTable -> 
    [(String, SymEntry a)]
queryClauses = mapMaybe 
    ( traverseOf (_2 % symEntryInfo ) 
        (\n -> case n ^? _SymSeqClause of 
            Just n -> Just $ _SymSeqClause # n
            Nothing -> case n ^? _SymConcClause of
                Just n -> Just $ _SymConcClause # n
                Nothing -> Nothing
            )
        )
