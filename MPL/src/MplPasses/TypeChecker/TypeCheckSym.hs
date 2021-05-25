{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
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

import Data.Kind

{- Module for defining the symbol table
 -}


type SymTabType = Map UniqueTag (MplObjectDefn MplTypeCheckedClause)
type SymTabExpr = Map UniqueTag (SymEntry SymSeqType SymExprInfo)
type SymTabCh = Map UniqueTag (SymEntry TypeTag ChIdentR)
type SymTabConc = Map UniqueTag (SymEntry SymConcType SymConcInfo)

type TypeTagMap = Map TypeTag SymTypeEntry

type SymZooms m0 m1 n = 
    ( Zoom m0 n (Maybe (SymEntry SymSeqType SymExprInfo)) SymTab 
    , Zoom m1 m0 (MplSeqObjDefn MplTypeCheckedPhrase) (Maybe (SymEntry SymSeqType SymExprInfo))
    ) 

data SymTypeEntry 
    -- | this is used exclusively for concurrent arrows
    = SymTypeConc 
        ( [TypeP MplTypeChecked]
        , [MplType MplTypeChecked]
        , [MplType MplTypeChecked]
        , [MplType MplTypeChecked]
        )
    -- | somewhat deceptive that this is used for everything BUT the concurrent arrows.
    -- This includes channels which are concurrent
    -- TODO probably would be a bit better to seperate this out a bit, 
    -- but it actually doesn't really matter at all...
    | SymTypeSeq 
        ( [TypeP MplTypeChecked]
        , [MplType MplTypeChecked]
        , MplType MplTypeChecked
        )

_SymTypeCh :: Prism' SymTypeEntry 
    ([TypeP MplTypeChecked], MplType MplTypeChecked)
_SymTypeCh = prism' cts prj
  where
    cts (vs, tp) = SymTypeSeq (vs, [], tp)
    prj (SymTypeSeq (vs, [], tp)) = Just $ (vs, tp)
    prj _ = Nothing

data SymTab = SymTab {
    _symTabExpr :: SymTabExpr
    , _symTabType :: SymTabType
    , _symTabConc :: SymTabConc
    , _symTabCh :: SymTabCh
}  

instance Semigroup SymTab where
    SymTab a0 b0 c0 d0 <> SymTab a1 b1 c1 d1 = SymTab (a0 <> a1) (b0 <> b1) (c0 <> c1) (d0 <> d1)

instance Monoid SymTab where
    mempty = SymTab mempty mempty mempty mempty

data SymExprInfo = 
    SymSeqCall ExprCallDef
    | SymSeqPhraseCall (MplSeqObjDefn MplTypeCheckedPhrase)
    {-
        | SymConcPhraseCall (MplConcObjDefn MplTypeCheckedPhrase)
        | SymChInfo ChIdentR
    -}

data SymConcInfo = 
    SymConcPhraseCall 
        (MplConcObjDefn MplTypeCheckedPhrase)
    | SymRunInfo (MplProcess MplTypeChecked)


data SymEntry a b = SymEntry {
    _symEntryType :: a
    , _symEntryInfo :: b
}

data SymSeqPhraseType a = SymSeqPhraseType {
    _noStateVarsType :: a
    , _originalType :: a
}

data SymCallType a = 
    SymImplicit (MplType MplTypeSub)
    | SymExplicit a

data SymSeqType =
    SymSeqCallType (SymCallType ([TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked))

    | SymDataPhrase (SymSeqPhraseType 
        ( [TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked))
     
    | SymCodataPhrase 
        ( SymSeqPhraseType 
            ([TypeP MplTypeChecked], ([MplType MplTypeChecked], MplType MplTypeChecked), MplType MplTypeChecked)
        )

data SymConcType 
    = SymConcCallType (SymCallType 
        ( [TypeP MplTypeChecked]
        , [MplType MplTypeChecked]
        , [MplType MplTypeChecked]
        , [MplType MplTypeChecked])
        )
    | SymConcPhrase ( [TypeP MplTypeChecked], MplType MplTypeChecked )
        -- (free vars, unwrapped type, clause type)

$(concat <$> traverse makePrisms 
    [ ''SymExprInfo 
    , ''SymSeqType 
    , ''SymEntry 
    , ''SymTypeEntry
    , ''SymSeqPhraseType
    , ''SymConcType 
    , ''SymConcInfo 
    ]
 )

$(makeClassyPrisms ''SymCallType)

$(concat <$> traverse makeLenses 
    [ ''SymEntry
    , ''SymTab 
    , ''SymSeqPhraseType  
    ]
 )


instance (tp ~ TypeP MplTypeChecked, mpltp ~ MplType MplTypeChecked) => 
    AsSymCallType SymSeqType ([tp], [mpltp], mpltp) where
    _SymCallType = _SymSeqCallType

instance (tp ~ TypeP MplTypeChecked, mpltp ~ MplType MplTypeChecked) =>
    AsSymCallType SymConcType ([tp], [mpltp], [mpltp], [mpltp]) where
    _SymCallType = _SymConcCallType 
