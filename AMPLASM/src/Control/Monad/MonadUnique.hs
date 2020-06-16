{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.MonadUnique where

import Control.Monad.State
import Data.Functor.Identity

{-
    Monad for generating fresh names
-}


class Monad m => MonadUnique s m | m -> s where
    fresh :: m s

newtype UniqueT s m a = UniqueT { unUniqueT :: StateT s m a }
    deriving ( Functor, Applicative, Monad, MonadState s, MonadIO)

type Unique s a = UniqueT s Identity a

runUniqueT :: UniqueT s m a -> s -> m (a, s)
runUniqueT uniquet = runStateT (unUniqueT uniquet)

runUnique :: Unique s a -> s -> (a, s)
runUnique unique = runState (unUniqueT unique)
