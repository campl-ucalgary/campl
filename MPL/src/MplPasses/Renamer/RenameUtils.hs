{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MplPasses.Renamer.RenameUtils where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplUtil.UniqueSupply
import MplPasses.Env

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import MplPasses.Renamer.RenameSym
import MplPasses.Renamer.RenameErrors

type RenameEnv = Env SymTab SymTab

tagIdentP :: 
    ( HasUniqueSupply s
    , MonadState s m ) =>
    IdentP -> 
    m IdentR 
tagIdentP identp = do
    sup <- freshUniqueSupply
    let uniq = uniqueFromSupply sup
    return $ IdentR identp (UniqueTag uniq)

type Rename parsed renamed = 
    forall e m.
    ( AsRenameErrors e
    , MonadState RenameEnv m
    , MonadWriter [e] m
    , MonadFix m ) =>
    parsed -> m renamed

