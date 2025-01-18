{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Renamer.RenameSym where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplUtil.UniqueSupply

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Control.Monad
import Data.Maybe

import Debug.Trace

import Control.Arrow

data SymEntry info = SymEntry { 
    _symEntryUniqueTag :: UniqueTag
    , _symEntryInfo :: info
}  deriving Show

-- Extra info about the symbol if the namespace information (
-- recall the namespaces are terms, types, and channels)
-- present in each identifier is not enough... If this information is
-- needed, then you must add it yourself after calling ``collectSymTab"
data SymInfo = 
    SymTypeInfo SymTypeVarInfo
    | SymSeqPhraseInfo SeqObjDefnTag
    | SymConcPhraseInfo ConcObjDefnTag
    | SymFunInfo 
    | SymProcInfo 
    | SymChInfo Polarity
  deriving Show

-- needed to resolve type variables and type clauses
data SymTypeVarInfo = 
    SymTypeVar
    | SymTypeClause ObjectDefnTag
  deriving Show

$(makeLenses ''SymEntry)
$(makePrisms ''SymEntry)
$(concat <$> traverse makeClassyPrisms 
    [ ''SymInfo 
    , ''SymTypeVarInfo ]
 )

chSymPrism :: 
    AsSymInfo sym =>
    Prism' (Maybe sym) Polarity
chSymPrism = _Just % _SymChInfo

lookupCh ident = 
    lookupSym ident chSymPrism

deleteCh ident = 
    deleteSym ident chSymPrism

-- | Restricts the channels to the provided idents in the symbol table
restrictChs :: 
    [IdentP] ->
    SymTab -> 
    SymTab 
restrictChs chs syms = go chs syms
  where
    go chs (sym:syms) 
        | has (_2 % symEntryInfo % chSymPrism) sym =
            if sym ^. _1 `elem` chs 
                then sym : go chs syms
                else go chs syms
        | otherwise = sym : go chs syms
    go chs []  = []
    
-- | finds all the channels currently in scope.
channelsInScope :: SymTab -> [(IdentP, SymEntry Polarity)]
channelsInScope = mapMaybe (traverseOf (_2 % symEntryInfo) (preview chSymPrism))

lookupProc ident = 
    lookupSym ident (_Just % _SymProcInfo)

lookupConcPhrase ident = 
    lookupSym ident (_Just % _SymConcPhraseInfo)

lookupTypeVar ident = 
    lookupSym ident (_Just % _SymTypeInfo % _SymTypeVar)

lookupTypeClause ident = 
    lookupSym ident (_Just % _SymTypeInfo % _SymTypeClause)

lookupSymTypeInfo ident = 
    lookupSym ident (_Just % _SymTypeInfo)

lookupSymSeqPhrase ident = 
    lookupSym ident (_Just % _SymSeqPhraseInfo)

{-
lookupSym ident prism = join 
    . fmap ( 
        traverseOf symEntryInfo 
            ( preview prism ) . view _2 )
    . find ((ident ==) . view _1)
-}

lookupSym :: (Eq t, Show t, Show a)=> t -> Prism _ _ _ _ -> [(t, SymEntry a)] -> Maybe (SymEntry b)
lookupSym ident prism [] = Nothing
lookupSym ident prism ((ident', entry):rst) 
    -- ident == ident' && has (symEntryInfo % prism) entry = Just entry
    | ident == ident', Just entry' <- entry ^? symEntryInfo % prism
        =  Just $ entry & symEntryInfo .~ entry'
    | otherwise = lookupSym ident prism rst

lookupSymAny ident = fmap snd . find ((ident ==) . view _1)

deleteSym :: Eq t => t -> Prism _ _ _ _ -> [(t, SymEntry a)] -> [(t, SymEntry a)]
deleteSym ident prism [] = []
deleteSym ident prism (sym:syms) 
    | ident == sym ^. _1 
        && has (_2 % symEntryInfo % prism) sym = syms
    | otherwise = sym : deleteSym ident prism syms


instance AsSymTypeVarInfo SymInfo where
    _SymTypeVarInfo = _SymTypeInfo 

instance HasUniqueTag (SymEntry info) where
    uniqueTag = symEntryUniqueTag 

type SymTab = [(IdentP, SymEntry (Maybe SymInfo))]

class CollectSymTab a where
    collectSymTab :: a -> SymTab

instance CollectSymTab a => CollectSymTab [a] where
    collectSymTab = concatMap collectSymTab

instance CollectSymTab a => CollectSymTab (NonEmpty a) where
    collectSymTab = concatMap collectSymTab

instance CollectSymTab (MplStmt MplRenamed) where
    collectSymTab stmt = foldOf (stmtDefns % folded % to collectSymTab) stmt

instance CollectSymTab (MplDefn MplRenamed) where
    collectSymTab (ObjectDefn n) = case n of
        SeqObjDefn n -> case n of
            DataDefn n -> querySpines n (_DataDefnTag # ()) 
                (_SymSeqPhraseInfo % _DataDefnTag # ())
            CodataDefn n -> querySpines n (_CodataDefnTag # ()) 
                (_SymSeqPhraseInfo % _CodataDefnTag # ())
        ConcObjDefn n -> case n of 
            ProtocolDefn n -> querySpines n (_ProtocolDefnTag # ()) 
                (_SymConcPhraseInfo % _ProtocolDefnTag # ())
            CoprotocolDefn n -> querySpines n (_CoprotocolDefnTag # ()) 
                (_SymConcPhraseInfo % _CoprotocolDefnTag # ())
      where
        querySpines spine tp tpphrase = 
            foldMapOf (typeClauseSpineClauses % folded)
                (flip queryTypeClause tp) spine
            ++ foldMapOf (typeClauseSpineClauses % folded)
                (flip queryTypePhrase tpphrase) spine

        queryTypeClause clause tp = clause ^. typeClauseName 
            % to (set (mapped % _2 % symEntryInfo) 
                    (_Just % _SymTypeClause # tp) 
                . collectSymTab)
        queryTypePhrase clause tpphrase = 
            foldMapOf (typeClausePhrases % traversed % typePhraseName)
                ( set (mapped % _2 % symEntryInfo) 
                    (_Just # tpphrase)
                . collectSymTab) 
                clause

    collectSymTab (FunctionDefn (MplFunction name _ _)) = 
        [(name ^. identRIdentP, SymEntry (name ^. uniqueTag) (_Just % _SymFunInfo # ()))]
    collectSymTab (ProcessDefn (MplProcess name _ _)) = 
        [(name ^. identRIdentP, SymEntry (name ^. uniqueTag) (_Just % _SymProcInfo # ()))]

instance CollectSymTab IdentR where
    collectSymTab = pure <<< view identRIdentP &&& flip SymEntry Nothing . view uniqueTag

instance CollectSymTab ChIdentR where
    collectSymTab ch = (ch ^. chIdentRIdentR % to collectSymTab) 
        & mapped % _2 % symEntryInfo .~ _Just % _SymChInfo # (ch ^. polarity)


tagIdentPWithSymEntry ::
    IdentP ->
    SymEntry a ->
    IdentR
tagIdentPWithSymEntry identp entry = _IdentR # (identp, entry ^. symEntryUniqueTag)

tagIdentPToChIdentRWithSymEntry :: 
    IdentP ->
    SymEntry Polarity ->
    ChIdentR
tagIdentPToChIdentRWithSymEntry ch entry = 
    _ChIdentR # (_IdentR # (ch, entry ^. uniqueTag ), entry ^. symEntryInfo)
