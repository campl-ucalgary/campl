{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module MPLPasses.GraphGenCore where

import Optics
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLAST.MPLProgGraph
import MPLPasses.TieDefnsErrors 

import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Bifunctor as Bifunctor
import Control.Arrow

import Data.Maybe
import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

newtype GraphGenCore a = GraphGenCore { 
    unGraphGenCore :: RWS GraphGenCoreEnv [TieDefnsError] GraphGenCoreState a
}  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState GraphGenCoreState
    , MonadReader GraphGenCoreEnv
    , MonadWriter [TieDefnsError]
    , MonadRWS GraphGenCoreEnv [TieDefnsError] GraphGenCoreState  )

data GraphGenCoreEnv = GraphGenCoreEnv {}
data GraphGenCoreState = GraphGenCoreState {
    _graphGenCoreStateUniqueTag :: UniqueTag
} 

defaultGraphGenCoreEnv = GraphGenCoreEnv
defaultGraphGenCoreState = 
    GraphGenCoreState $ UniqueTag 0

$(concat <$> traverse makeClassy 
    [ ''GraphGenCoreState
    , ''GraphGenCoreEnv ]
 )

instance HasUniqueTag GraphGenCoreState where
    uniqueTag = graphGenCoreStateUniqueTag
