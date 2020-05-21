{-# LANGUAGE FlexibleInstances #-}
module Control.MonadIORef 
    ( IORef
    , MonadIORef 
    , MonadAtomicIORef 
    , newIORef 
    , readIORef 
    , writeIORef
    , modifyIORef
    , modifyIORef'
    , atomicModifyIORef 
    , atomicModifyIORef'
    )
    where

import qualified Data.IORef as IORef
import Data.IORef (IORef)
import Control.Monad.IO.Class

import Control.Monad.Reader

{-
    Wrapper for Data.IORef for type safety in monads..
-}

class Monad m => MonadAtomicIORef m where
    newIORef :: a -> m (IORef a)
    readIORef :: IORef a -> m a
    atomicModifyIORef :: IORef a -> (a -> (a, b)) -> m b
    atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> m b

class MonadAtomicIORef m => MonadIORef m where
    writeIORef :: IORef a -> a -> m ()
    modifyIORef :: IORef a -> (a -> a) -> m ()
    modifyIORef' :: IORef a -> (a -> a) -> m ()
    
instance MonadAtomicIORef IO where
    newIORef = IORef.newIORef
    readIORef = IORef.readIORef
    atomicModifyIORef = IORef.atomicModifyIORef 
    atomicModifyIORef' = IORef.atomicModifyIORef'

instance MonadAtomicIORef m => MonadAtomicIORef (ReaderT r m) where
    newIORef = lift . newIORef
    readIORef = lift . readIORef 
    atomicModifyIORef a = lift . atomicModifyIORef a
    atomicModifyIORef' a = lift . atomicModifyIORef' a

instance MonadIORef IO where
    writeIORef = IORef.writeIORef 
    modifyIORef = IORef.modifyIORef 
    modifyIORef' = IORef.modifyIORef' 

instance MonadIORef m => MonadIORef (ReaderT r m) where
    writeIORef a = lift . writeIORef a
    modifyIORef a = lift . modifyIORef a
    modifyIORef' a = lift . modifyIORef' a
