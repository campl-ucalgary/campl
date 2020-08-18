{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Renamer.RenamePatt where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.Renamer.RenameUtils
import MplPasses.Renamer.RenameSym
import MplPasses.Renamer.RenameErrors
import MplPasses.Renamer.RenameType
import MplPasses.Env

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import MplUtil.UniqueSupply

import Data.Bool
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable
import Data.Functor.Foldable (cata, Base)

renamePattern :: 
    Rename (MplPattern MplParsed)
        (MplPattern MplRenamed)
renamePattern = cata f
  where
    f :: Base (MplPattern MplParsed) (_ (MplPattern MplRenamed)) ->
        (_ (MplPattern MplRenamed))
    f (PConstructorF () ident args) = do
        symtab <- guse envLcl
        args' <- sequenceA args
        let ident' = fromJust $ 
                lookupSymSeqPhrase ident symtab
        tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 

        return $ _PConstructor # 
            ( ()
            , _IdentR # (ident, ident' ^. uniqueTag)
            , args' )
    f (PRecordF loc phrases) = do
        phrases' <- traverse g phrases

        return $ _PRecord # ( loc, phrases' )
      where
        g ((), ident, patt) = do
            symtab <- guse envLcl
            patt' <- patt
            let ident' = fromJust $ lookupSymSeqPhrase ident symtab
            tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 
            return ((), _IdentR # (ident, ident' ^. uniqueTag), patt')
    -- extends the variable context
    f (PVarF () ident) = do
        ident' <- tagIdentP ident
        envLcl %= ((collectSymTab ident')<>)
        return $ _PVar # ((), ident')
     
    f (PNullF loc) = do
        return $ _PNull # loc
