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

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import MplPasses.Renamer.RenameSym
import MplPasses.Renamer.RenameErrors

data RenameEnv = RenameEnv {
    _renameEnvUniqueSupply :: UniqueSupply
    , _symTab :: SymTab
}

$(makeLenses ''RenameEnv)
$(makePrisms ''RenameEnv)

instance HasUniqueSupply RenameEnv where
    uniqueSupply = renameEnvUniqueSupply 

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
    ( AsRenameError e
    , MonadState RenameEnv m
    , MonadWriter [e] m
    , MonadFix m ) =>
    parsed -> m renamed

type RenameConst parsed renamed = 
    forall e m.
    ( AsRenameError e
    , MonadReader RenameEnv m
    , MonadWriter [e] m ) =>
    parsed -> m renamed

newtype RenameM a = RenameM {
        unRenameM :: StateT RenameEnv (Writer [RenameError]) a
    }
