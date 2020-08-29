{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module MplPasses.TypeChecker.TypeCheckSymLookup where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplTypeChecked

import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckSym 

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Data.Bool

{-
lookupSymTab :: 
    Monad m => 
    -- | err
    m () -> 
    -- | key
    UniqueTag -> 
    -- | map
    SymTabMap a -> 
    m a
lookupSymTab err key symtab = do
    entry <- fmap fromJust $ symtab ^? at key
    bool err (return ()) $ has 
        (symTabMapEntryExists % _SymEntryExists) 
        entry 
    return $ entry ^. 
    -}
