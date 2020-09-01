{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheckSym where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplTypeChecked
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeCheckMplTypeSub

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Data.Maybe


type SymTabType = Map UniqueTag (MplObjectDefn MplTypeCheckedClause)
type SymTabTerm = Map UniqueTag (SymEntry SymType SymTermInfo)
type SymTabCh = Map UniqueTag (SymEntry (MplType MplTypeSub) ChIdentR)

type TypeTagMap = Map TypeTag SymTypeEntry

data SymTypeEntry = 
    SymTypeConc ([TypeP MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked])
    | SymTypeSeq ([TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked)

_SymTypeCh :: Prism' SymTypeEntry ([TypeP MplTypeChecked], MplType MplTypeChecked)
_SymTypeCh = prism' cts prj
  where
    cts (vs, tp) = SymTypeSeq (vs, [], tp)
    prj (SymTypeSeq (vs, [], tp)) = Just $ (vs, tp)
    prj _ = Nothing



data SymTab = SymTab {
    _symTabTerm :: SymTabTerm
    , _symTabType :: SymTabType
    , _symTabCh :: SymTabCh
}  

instance Semigroup SymTab where
    SymTab a0 b0 c0 <> SymTab a1 b1 c1 = SymTab (a0 <> a1) (b0 <> b1) (c0 <> c1)

instance Monoid SymTab where
    mempty = SymTab mempty mempty mempty

data SymTermInfo = 
    SymRunInfo (MplProcess MplTypeChecked)
    | SymSeqCall ExprCallDef
    | SymSeqPhraseCall (MplSeqObjDefn MplTypeCheckedPhrase)
    | SymConcPhraseCall (MplConcObjDefn MplTypeCheckedPhrase)
    | SymChInfo ChIdentR


data SymEntry a b = SymEntry {
    _symEntryType :: a
    , _symEntryInfo :: b
}
-- SymType

data SymPhraseType a = SymPhraseType {
    _noStateVarsType :: a
    , _originalType :: a
}

data SymType =
    SymSub (MplType MplTypeSub)
    | SymProc ([TypeP MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked])
    | SymFun ([TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked)

    | SymDataPhrase (SymPhraseType ([TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked))
     
    | SymCodataPhrase 
        (SymPhraseType ([TypeP MplTypeChecked], ([MplType MplTypeChecked], MplType MplTypeChecked), MplType MplTypeChecked))
    -- | SymInst ([IdentT], SymTypeEntry)



$(concat <$> traverse makePrisms 
    [ ''SymTermInfo 
    , ''SymType 
    , ''SymEntry 
    , ''SymTypeEntry
    , ''SymPhraseType
    ]
 )
$(concat <$> traverse makeLenses 
    [ ''SymEntry
    , ''SymTab 
    , ''SymPhraseType  
    ]
 )

