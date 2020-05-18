module Control.MonadMVar 
    ( M.MVar 
    , MonadMVar
    , newEmptyMVar 
    , newMVar 
    , takeMVar
    , putMVar 
    , readMVar 
    , swapMVar 
    , tryTakeMVar 
    , tryPutMVar 
    , isEmptyMVar 
    , withMVar 
    , withMVarMasked 
    , modifyMVar_ 
    , modifyMVar 
    , modifyMVarMasked_ 
    , modifyMVarMasked 
    , tryReadMVar 
    , mkWeakMVar 
    )
    where

import Control.Monad.IO.Class

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as M
import System.Mem.Weak

{-
    Wrapper around the MVar operations (for type safety)
-}

class Monad m => MonadMVar m where
    newEmptyMVar :: m (MVar a)
    newMVar :: a -> m (MVar a)
    takeMVar :: MVar a -> m a
    putMVar :: MVar a -> a -> m ()
    readMVar :: MVar a -> m a
    swapMVar :: MVar a -> a -> m a
    tryTakeMVar :: MVar a -> m (Maybe a)
    tryPutMVar :: MVar a -> a -> m Bool
    isEmptyMVar :: MVar a -> m Bool
    withMVar :: MVar a -> (a -> m b) -> m b
    withMVarMasked :: MVar a -> (a -> m b) -> m b
    modifyMVar_ :: MVar a -> (a -> m a) -> m ()
    modifyMVar :: MVar a -> (a -> m (a, b)) -> m b
    modifyMVarMasked_ :: MVar a -> (a -> m a) -> m ()
    modifyMVarMasked :: MVar a -> (a -> m (a, b)) -> m b
    tryReadMVar :: MVar a -> m (Maybe a)
    mkWeakMVar :: MVar a -> m () -> m (Weak (MVar a))

instance MonadMVar IO where
    newEmptyMVar = M.newEmptyMVar
    newMVar = M.newMVar
    takeMVar = M.takeMVar
    putMVar = M.putMVar 
    readMVar = M.readMVar
    swapMVar = M.swapMVar
    tryTakeMVar = M.tryTakeMVar
    tryPutMVar = M.tryPutMVar
    isEmptyMVar = M.isEmptyMVar
    withMVar = M.withMVar
    withMVarMasked = M.withMVarMasked
    modifyMVar_ = M.modifyMVar_
    modifyMVar = M.modifyMVar
    modifyMVarMasked_ = M.modifyMVarMasked_
    modifyMVarMasked = M.modifyMVarMasked
    tryReadMVar = M.tryReadMVar
    mkWeakMVar = M.mkWeakMVar
