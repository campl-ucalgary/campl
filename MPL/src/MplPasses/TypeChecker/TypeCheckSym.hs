{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheckSym where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

import MplPasses.TypeChecker.TypeCheckMplTypeSub

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

type SymTab = Map UniqueTag SymInfo

data SymInfo = 
    SymRunInfo (Maybe (MplType MplTypeSub)) (MplProcess MplTypeChecked)
    | SymSeqPattVar (Maybe (MplType MplTypeSub)) 

$(makePrisms ''SymInfo)

lookupSym ::
    ( HasUniqueTag t ) =>
    t -> SymTab -> SymInfo
lookupSym v map = fromJust $ Map.lookup (v ^. uniqueTag) map
    
    
class CollectSymTab a where
    collectSymTab :: a -> SymTab

instance CollectSymTab (MplDefn MplTypeChecked) where
    collectSymTab = undefined
