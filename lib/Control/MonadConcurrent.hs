module Control.MonadConcurrent
    ( MonadConcurrent
    , myThreadId
    , forkIO
    , forkFinally
    , ThreadId
    ) where

import Control.Concurrent (ThreadId)
import qualified Control.Concurrent as C
import Control.Exception

{-
    Wrapper for some of the operations in Control.Concurrent...
-}

class MonadConcurrent m where
    myThreadId :: m ThreadId
    forkIO :: m () -> m ThreadId
    forkFinally :: m a -> (Either SomeException a -> m ()) -> m ThreadId

instance MonadConcurrent IO where
    myThreadId = C.myThreadId
    forkIO = C.forkIO
    forkFinally = C.forkFinally


