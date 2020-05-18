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
    newIORef = liftIO . IORef.newIORef
    readIORef = liftIO . IORef.readIORef
    atomicModifyIORef r f = liftIO (IORef.atomicModifyIORef r f)
    atomicModifyIORef' r f = liftIO (IORef.atomicModifyIORef' r f)

instance MonadIORef IO where
    writeIORef r f = liftIO (IORef.writeIORef r f)
    modifyIORef r f = liftIO (IORef.modifyIORef r f)
    modifyIORef' r f = liftIO (IORef.modifyIORef' r f)

