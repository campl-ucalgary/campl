{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheckObj where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.Env

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import Debug.Trace

typeCheckClauseSpine ::
    ( AsTypeCheckErrors e 
    , TypeClauseSpineSameVarError t 
    , MonadWriter [e] m 
    , MonadState TypeCheckEnv m ) =>
    MplTypeClauseSpine MplRenamed t -> m (MplTypeClauseSpine MplTypeChecked t)
typeCheckClauseSpine spine = do
    -- first, check if all the args are the same 
    -- (mutually recursive types must have the same type variables)
    tell $ typeClauseSpineSameVarError spine
    undefined
