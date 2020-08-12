{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

import Control.Arrow

data SymEntry info = SymEntry { 
    _symEntryUniqueTag :: UniqueTag
    , _symEntryInfo :: info
}

-- Extra info about the symbol if the namespace information (
-- recall the namespaces are terms, types, and channels)
-- present in each identifier is not enough... If this information is
-- needed, then you must add it yourself after calling ``collectSymTab"
data SymInfo = 
    SymTypeInfo SymTypeVarInfo
    | SymPhraseInfo SMplObjectDefn
    | SymFunInfo 
    | SymProcInfo 
    | SymChInfo Polarity

-- needed to resolve type variables and type clauses
data SymTypeVarInfo = 
    SymTypeVar
    | SymTypeClause SMplObjectDefn

$(makeLenses ''SymEntry)
$(makePrisms ''SymEntry)
$(concat <$> traverse makeClassyPrisms 
    [ ''SymInfo 
    , ''SymTypeVarInfo ]
 )

lookupCh ident = 
    lookupSym ident (_Just % _SymChInfo)

lookupProc ident = 
    lookupSym ident (_Just % _SymProcInfo)

lookupTypeVar ident = 
    lookupSym ident (_Just % _SymTypeInfo % _SymTypeVar)

lookupTypeClause ident = 
    lookupSym ident (_Just % _SymTypeInfo % _SymTypeClause)

lookupSym ident prism = join 
    . fmap ( 
        traverseOf symEntryInfo 
            ( preview prism) . view _2 )
    . find ((ident ^. name==) . view _1)

instance AsSymTypeVarInfo SymInfo where
    _SymTypeVarInfo = _SymTypeInfo 


instance HasUniqueTag (SymEntry info) where
    uniqueTag = symEntryUniqueTag 

type SymTab = [(Name, SymEntry (Maybe SymInfo))]

class CollectSymTab a where
    collectSymTab :: a -> SymTab

instance CollectSymTab a => CollectSymTab [a] where
    collectSymTab = concatMap collectSymTab

instance CollectSymTab a => CollectSymTab (NonEmpty a) where
    collectSymTab = concatMap collectSymTab

instance CollectSymTab (MplStmt MplRenamed) where
    collectSymTab = undefined

instance CollectSymTab (MplDefn MplRenamed) where
    collectSymTab (ObjectDefn n) = case n of
        DataDefn n -> querySpines n SDataDefn
        CodataDefn n -> querySpines n SCodataDefn
        ProtocolDefn n -> querySpines n SProtocolDefn 
        CoprotocolDefn n -> querySpines n SCoprotocolDefn 
      where
        querySpines spine tp = 
            foldMapOf (typeClauseSpineClauses % folded)
                (flip queryTypeClause tp) spine
            ++ foldMapOf (typeClauseSpineClauses % folded)
                (flip queryTypePhrase tp) spine

        queryTypeClause clause tp = clause ^. typeClauseName 
            % to (set (mapped % _2 % symEntryInfo) 
                    (_Just % _SymTypeClause # tp) 
                . collectSymTab)
        queryTypePhrase clause tp = 
            foldMapOf (typeClausePhrases % traversed % typePhraseName)
                ( set (mapped % _2 % symEntryInfo) 
                    (_Just % _SymPhraseInfo # tp)
                . collectSymTab) 
                clause

    collectSymTab (FunctionDefn (MplFunction name _ _)) = undefined
    collectSymTab (ProcessDefn (MplProcess name _ _)) = undefined
        

instance CollectSymTab IdentR where
    collectSymTab = pure <<< view name &&& flip SymEntry Nothing . view uniqueTag

