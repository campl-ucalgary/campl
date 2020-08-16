{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
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

    | SymSeqConcType SymSeqConcTypeInfo
  deriving Show

data SymCallTypeVar =
    SymCallDummyTypeVar TypeTag
    | SymCallExplicitType (TypeG TaggedBnfcIdent)
  deriving Show

data SymChInfo = SymChInfo {
    _symChTypeTag :: TypeTag
    , _symChPolarity :: Polarity
}  deriving Show

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
    , ''SymInfo 
    , ''SymChInfo ]
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

lookupsConcPhrases ::    
    Functor f => 
    f BnfcIdent ->
    SymbolTable ->
    f (Maybe (TypePhraseG TaggedBnfcIdent))
lookupsConcPhrases idents symtab = fmap f idents
  where
    f ident = fmap (view symEntryInfo)
                $ lookupBnfcIdent ident 
                $ mapMaybe ( traverseOf (_2 % symEntryInfo)
                                    ( preview (_SymConcCall % _SymPhrase) )) symtab



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

instance HasBnfcIdent (String, SymEntry info) where
    bnfcIdent = lens getter setter
      where
        getter (str, SymEntry _ pos _) = BnfcIdent (str, pos)
        setter (str, SymEntry tag _ info) ident = 
            ( ident ^. bnfcIdentName
            , SymEntry tag (ident ^. bnfcIdentPos) info )

instance HasTaggedBnfcIdent (String, SymEntry info) where
    taggedBnfcIdent = lens getter setter
      where
        getter n@(str, SymEntry tag _ _) = _TaggedBnfcIdent # (n ^. bnfcIdent, tag)
        setter n@(_, SymEntry _ _ info) (TaggedBnfcIdent bnfc tag') = 
            n & bnfcIdent .~ bnfc
              & _2 % symEntryUniqueTag .~ tag'

instance HasTaggedChIdent (String, SymEntry SymChInfo) where
    taggedChIdent = lens getter setter
      where
        getter n@(str, SymEntry tag _ (SymChInfo ttype pol)) = _TaggedChIdent # (n ^. taggedBnfcIdent, pol)
        setter n (TaggedChIdent bnfc pol) = 
            n & taggedBnfcIdent .~ bnfc
              & _2 % symEntryInfo % symChPolarity .~ pol


deleteSymTab :: 
    String ->
    [(String, a)] -> 
    [(String, a)]
deleteSymTab str ((str', a) : rst)
    | str == str' = rst
    | otherwise = (str', a) : deleteSymTab str rst
deleteSymTab _ [] = []


