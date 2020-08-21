{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MplPasses.TypeChecker.TypeCheckUtils where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplUtil.UniqueSupply

import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckErrors
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.Env

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

type TypeCheckEnv = Env SymTab TypeInfoEnv

data TypeInfoEnv = TypeInfoEnv {
    -- | the symbol table of course.
    _typeInfoSymTab :: SymTab
    -- | this should only change as you traverse down
    -- the expression
    , _typeInfoEnvTypeTag :: TypeTag
    -- | this should not change...
    , _typeInfoEnvMap :: Map TypeTag (MplType MplTypeChecked)
}

$(makeLenses ''TypeInfoEnv)

freshTypeInfoEnv :: 
    ( HasUniqueSupply s
    , MonadState s m ) =>
    m TypeInfoEnv
freshTypeInfoEnv = do
    tag <- freshTypeTag
    return $ TypeInfoEnv mempty tag mempty

withFreshTypeTag ::
    ( AsTypeCheckErrors e 
    , MonadWriter [e] m 
    , MonadState TypeCheckEnv m
    , MonadFix m ) =>
    m a -> m (TypeTag, a)
withFreshTypeTag act = do
    tag <- guse (envLcl % typeInfoEnvTypeTag)
    tag' <- freshTypeTag
    envLcl % typeInfoEnvTypeTag .= tag'
    res <- act
    envLcl % typeInfoEnvTypeTag .= tag
    return (tag', res)

type TypeCheck renamed typechecked =
    forall e m. 
    ( AsTypeCheckErrors e 
    , MonadWriter [e] m 
    , MonadState TypeCheckEnv m
    , MonadFix m ) =>
    renamed -> m typechecked
