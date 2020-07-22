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
import MPLUtil.UniqueSupply

import Data.IORef

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
    _graphGenCoreStateUniqueSupply :: UniqueSupply
} 

splitGraphGenCore :: 
    GraphGenCore a -> 
    GraphGenCore (a, [TieDefnsError])
splitGraphGenCore m = do
    env <- ask 
    sup <- freshUniqueSupply
    let ~(a, st, w) = runRWS (unGraphGenCore m) env (GraphGenCoreState sup)
    return (a, w)

defaultGraphGenCoreEnv :: GraphGenCoreEnv
defaultGraphGenCoreEnv = GraphGenCoreEnv

defaultGraphGenCoreState :: IO GraphGenCoreState
defaultGraphGenCoreState = do
    ref <- newIORef (0 :: Int)
    supply <- initUniqueSupply ref
    return $ GraphGenCoreState supply

$(concat <$> traverse makeClassy 
    [ ''GraphGenCoreState
    , ''GraphGenCoreEnv ]
 )

instance HasUniqueSupply GraphGenCoreState where
    uniqueSupply = graphGenCoreStateUniqueSupply
