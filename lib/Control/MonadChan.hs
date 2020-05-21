module Control.MonadChan 
    ( MonadChan
    , Control.Concurrent.Chan.Chan
    , Control.MonadChan.newChan 
    , Control.MonadChan.writeChan 
    , Control.MonadChan.readChan 
    , Control.MonadChan.dupChan 
    )
    where

import Control.Monad.IO.Class
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan
import Control.Monad.Reader

{-
    Wrapper for the Channel operations for 
    type safety...
-}

class MonadIO m => MonadChan m where
    newChan :: m (Chan a)
    writeChan :: Chan a -> a -> m ()
    readChan :: Chan a -> m a
    dupChan :: Chan a -> m (Chan a)

-- usual IO instance...
instance MonadChan IO where
    newChan = Control.Concurrent.Chan.newChan
    writeChan = Control.Concurrent.Chan.writeChan 
    readChan = Control.Concurrent.Chan.readChan
    dupChan = Control.Concurrent.Chan.dupChan

instance MonadChan m => MonadChan (ReaderT r m) where
    newChan = lift newChan
    writeChan a = lift . writeChan a
    readChan = lift . readChan
    dupChan = lift . dupChan 

