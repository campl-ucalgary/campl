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
type SymTabTerm = Map UniqueTag (SymEntry SymTermInfo)

type TypeTagMap = Map TypeTag SymTypeEntry

data SymTypeEntry = 
    SymTypeProc ([TypeP MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked])
    | SymTypeFun ([TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked)
    | SymType (MplType MplTypeChecked)



data SymTab = SymTab {
    _symTabTerm :: SymTabTerm
    , _symTabType :: SymTabType
}  

instance Semigroup SymTab where
    SymTab a0 b0 <> SymTab a1 b1 = SymTab (a0 <> a1) (b0 <> b1) 

instance Monoid SymTab where
    mempty = SymTab mempty mempty 

data SymTermInfo = 
    SymRunInfo (MplProcess MplTypeChecked)
    | SymSeqCall ExprCallDef
    | SymSeqPhraseCall (MplSeqObjDefn MplTypeCheckedPhrase)
    | SymChInfo ChIdentR


data SymEntry a = SymEntry {
    _symEntryType :: SymType
    , _symEntryInfo :: a
}

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

