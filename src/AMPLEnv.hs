module AMPLEnv where

import Data.Array
import Data.Coerce
import Data.List
import Data.Map (Map (..))
import Control.Arrow
import Control.Monad.IO.Class

import qualified Data.Map as Map
import Control.Concurrent (MVar, newMVar)


import Control.MonadIORef
import Control.MonadChan
import Data.Queue (Queue (..))
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import qualified Data.Queue as Queue

import AMPLTypes

-- | Type class for looking up supercombinators
class HasSuperCombinators a where
    superCombInstrLookup :: a -> FunID -> [Instr]
    superCombNameLookup :: a -> FunID -> String

-- | type class for getting a logger.
-- e.g. putStrLn is a valid implementation of getLog.
class HasLog a where
    getLog :: a -> (String -> IO ())

-- | Type class for functions getting the channel manager
class HasChannelManager a where
    getChannelManager :: a -> MVar (Map GlobalChanID (Queue QInstr, Queue QInstr))

-- | Type class for counting the number of alive processes (threads).
class HasProcessCounter a where
    -- | tells us if no process is running (useful for termination
    -- checking)
    noProcessesRunning :: MonadAtomicIORef m => a -> m Bool
    -- | Increases the number of processes running
    succNumProcesses :: MonadAtomicIORef m => a -> m ()
    -- | Decreases the number of processes running
    predNumProcesses :: MonadAtomicIORef m => a -> m ()

-- | Functions relating to manipulating the broadcast channel.
-- NOte that an exception CANNOT be thrown during any function or the
-- size invariant may be violated..
class HasBroadcastChan a where
    -- | Gets the broadcast channel. The bradcast channel is the 
    -- almighty channel (FIFO queue) that communicates between the 
    -- processes, and the channel manager. This should NOT be used,
    -- since it will nto change the counter for the numbr of elements
    -- in the channel...
    -- getBroadcastChan :: a -> Chan BInstr

    -- | Wrapper around writeChan that keeps track of the number
    -- of elements in the Chan.
    writeBroadcastChan :: (MonadChan m, MonadAtomicIORef m) => a -> BInstr -> m ()

    -- | Wrapper around readChan that keeps track of the number
    -- of elements in the Chan.
    readBroadcastChan :: (MonadChan m, MonadAtomicIORef m) => a -> m BInstr

    -- | Gets the size of the broadcast channel (helpful as a termination 
    -- condition..
    getSizeOfBroadcastChan :: MonadAtomicIORef m => a -> m Word

-- | Type class for getting a channel name (note that multiple threads
-- may want to create a channel name at the same time, hence the dependency
-- on MonadAtomicIORef). 
class HasChannelNameGenerator a where
    -- | Gets a new channel name. It /should/ be fresh provided that
    -- each name returned in 'returnChannelName' is fresh
    getNewChannelName :: MonadAtomicIORef m => a -> m GlobalChanID
    -- | Returns a channel name to be used again.
    returnChannelName :: MonadAtomicIORef m => a -> GlobalChanID -> m ()

-- Environment that the machine runs in.
-- Includes: supercombinator defintions, data for locks and queues
data AmplEnv = AmplEnv
    {
        -- | function definitions (supercombinators is the terminolgy Simon Peyton Jones uses)
        supercombinators :: Array FunID (String, [Instr])
        -- | Used to log strings (all to IO)..
        , amplLog :: String -> IO ()
        -- | channel manager -- map from GlobalChanID to input and output queues..
        , channelManager :: MVar (Map GlobalChanID (Queue QInstr, Queue QInstr))
        -- | seed for the channel name
        , channelNameGenerator :: IORef (Stream Word)
        -- | a channel to broadcast commands to the channel manager. We call this the broadcast channel...
        , broadcastChan :: Chan BInstr
        -- | Corresponding to the size of the broadcase channel..
        , broadcastChanSize :: IORef Word
        -- | number of running processes (used for testing temrination...)
        , numRunningProcesses :: IORef Word
    }

-- | Smart constructor for the environment
amplEnv :: 
    [(FunID, (String, [Instr]))] ->             -- ^ association list of funciton ids and its name / instruction
    (String -> IO ()) ->                        -- ^ logger.
    Map GlobalChanID 
        (Queue QInstr, Queue QInstr) ->         -- ^ Channel manager (services should be preinitialized)...
    Stream Word ->                              -- ^ the head should be a fresh name..
    ([Instr], [Translation], [Val], [Val]) ->   -- (main function...)
    IO AmplEnv
amplEnv defs lg chm nmg pr = do
    chan <- newChan
    mchm <- newMVar chm
    nmg' <- newIORef nmg
    -- every program starts with running just the main process..
    numrunpr <- newIORef 1  
    -- every program starts with nothing in the broadcast channel
    broadcastchansize <- newIORef 0  
    return AmplEnv
            {
                supercombinators = if null defs
                                    -- create empty array..
                                    then array (FunID 1, FunID 0) []
                                    -- otherwise, fill up the array with the definitions..
                                    else array (FunID 0, FunID (genericLength defs - 1)) defs
                , amplLog = lg
                , broadcastChan = chan
                , broadcastChanSize = broadcastchansize
                , channelManager = mchm
                , channelNameGenerator = nmg'
                , numRunningProcesses = numrunpr
            }

instance HasSuperCombinators AmplEnv where
    superCombInstrLookup env ix = snd (supercombinators env ! ix)
    superCombNameLookup env ix = fst (supercombinators env ! ix)

instance HasLog AmplEnv where
    getLog = amplLog

instance HasChannelManager AmplEnv where
    getChannelManager = channelManager

instance HasProcessCounter AmplEnv where
    noProcessesRunning env = (==0) <$> readIORef (numRunningProcesses env)
    succNumProcesses env = atomicModifyIORef' (numRunningProcesses env) (succ &&& const ())
    predNumProcesses env = atomicModifyIORef' (numRunningProcesses env) (pred &&& const ())

instance HasBroadcastChan AmplEnv where
    -- getBroadcastChan = broadcastChan

        --  Note the ordering for writeBroadcastChan and readBroadcastChan
    writeBroadcastChan AmplEnv{ broadcastChan = bch, broadcastChanSize = bchsz } n =
        writeChan bch n >> atomicModifyIORef' bchsz (succ &&& const ()) 

    readBroadcastChan AmplEnv{ broadcastChan = bch, broadcastChanSize = bchsz } = do
        n <- readChan bch 
        atomicModifyIORef' bchsz (pred &&& const ()) 
        return n

    getSizeOfBroadcastChan AmplEnv{ broadcastChanSize = bchsz } = 
        readIORef bchsz

instance HasChannelNameGenerator AmplEnv where
    getNewChannelName AmplEnv{ channelNameGenerator = chg } = 
        GlobalChanID <$> atomicModifyIORef' chg (Stream.tail &&& Stream.head) 

    returnChannelName AmplEnv{ channelNameGenerator = chg } chid = 
        atomicModifyIORef' chg  (Stream.Cons (coerce chid) &&& const ()) 
