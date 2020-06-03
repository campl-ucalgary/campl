module AMPLEnv where

import AMPLLogger
import AMPLTypes
import AMPLServices

import Control.Arrow
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad.Reader
import Data.Array
import Data.Coerce
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function
import System.IO
import Network.Socket

import Control.Concurrent
import Control.Concurrent.Chan
import Data.IORef
import Data.Maybe
import Data.Queue (Queue)
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import qualified Data.Queue as Queue



-- | Type class for looking up supercombinators
class HasSuperCombinators a where
    superCombInstrLookup :: a -> FunID -> [Instr]
    superCombNameLookup :: a -> FunID -> String

-- | type class for getting a logger.
-- By convention, we adopt that: 
--  - getFileLog is used for logging the application state... (normally
--      logging to a file..)
--  - getStdLog is used to log useful information to the user.. (normally
--      logging things to stdout)
class HasLog a where
    getStdLog :: a -> (String -> IO ())
    getFileLog :: a -> (String -> IO ())

-- | Type class for functions getting the channel manager
class HasChannelManager a where
    getChannelManager :: a -> IORef Chm

-- | Type class for counting the number of alive processes (threads).
class HasProcessCounter a where
    -- | tells us if no process is running (useful for termination
    -- checking)
    getNumRunningProcesses :: MonadIO m => a -> m Word
    -- | Increases the number of processes running
    succNumProcesses :: MonadIO m => a -> m ()
    -- | Decreases the number of processes running
    predNumProcesses :: MonadIO m => a -> m ()

-- | Functions relating to manipulating the broadcast channel.
-- NOte that an exception CANNOT be thrown during any function or the
-- size invariant may be violated..
class HasBroadcastChan a where
    -- | Gets the broadcast channel. The bradcast channel is the 
    -- almighty channel (FIFO queue) that communicates between the 
    -- processes, and the channel manager. 

    -- | Wrapper around writeChan that keeps track of the number
    -- of elements in the Chan.
    writeBroadcastChan :: MonadIO m => a -> BInstr -> m ()

    -- | Wrapper around readChan that keeps track of the number
    -- of elements in the Chan.
    readBroadcastChan :: MonadIO m => a -> m BInstr

    -- | Gets the size of the broadcast channel (helpful as a termination 
    -- condition..
    getSizeOfBroadcastChan :: MonadIO m => a -> m Word

-- | Type class for getting a channel name (note that multiple threads
-- may want to create a channel name at the same time, hence the dependency
-- on MonadAtomicIORef). 
class HasChannelNameGenerator a where
    -- | Gets a new channel name. It /should/ be fresh provided that
    -- each name returned in 'returnChannelName' is fresh
    getNewChannelName :: MonadIO m => a -> m GlobalChanID
    -- | Returns a channel name to be used again.
    returnChannelName :: MonadIO m => a -> GlobalChanID -> m ()

-- | Type class related to things to do with Ampl services..
-- i.e., this gives you lookups for services' information, and
-- a lock for printing stuff to stdout
class HasAmplServices a where
    -- | given a GlobalChanID, we can look up the corresponding environment for that Service...
    lookupServiceEnv :: GlobalChanID -> a -> Maybe ServiceEnv

    -- this is needed because the std service
    -- can have multiple threads which access
    -- stdin/stdout
    getStdServiceLock :: a -> MVar ()

class HasNetworkedConnections a where
    -- | Gets the TCP server info...
    getTCPServerInfo :: a -> AmplTCPServer

    -- | Puts the TCP thread id down (will block! if full already)
    putTcpThreadId :: a -> ThreadId -> IO ()

    -- | Takes the TCP thead id
    takeTcpThreadId :: a -> IO ThreadId

    -- | Get the queued clients (clients associated with
    -- a key)
    getQueuedClients :: a -> QueuedClients


-- | newtype wrapper for an array of supercombinators
-- (supercombinators is the terminolgy Simon Peyton Jones uses for functions)
newtype Supercombinators = Supercombinators (Array FunID (String, [Instr]))

-- | Smart constructor for supercombinators
mkSupercombinators :: 
    [(FunID, (String, [Instr]))] ->          -- ^ association list of funciton ids and its name / instruction
    Supercombinators 
mkSupercombinators defs 
    | null defs = Supercombinators (array (FunID 1, FunID 0) [])
        -- create empty array -- NOTE: from the Data.Array documentation,
        -- it says to create an empty array, we create an array with bounds
        -- exclusive of any values.
    | otherwise = Supercombinators (array (FunID 0, FunID (genericLength defs - 1)) defs)
        -- otherwise, fill up the array with the definitions..

-- Environment that the machine runs in.
-- Includes: supercombinator defintions, data for locks and queues
data AmplEnv = AmplEnv
    {
        -- | function definitions 
        supercombinators :: Supercombinators
        -- | AMPL TCP server....
        , amplTCPServer :: (MVar ThreadId, AmplTCPServer)
        -- | Map from global channel ids to ServiceEnv
        , amplServices :: Services
        -- | Maps the keys to connected clients..
        , amplQueuedClients :: QueuedClients
        -- | Standard service lock (this service is special because we can have multiple 
        -- types mapping to this same service i.e., stdin/stdout can both be an int terminal
        -- and a char terminal. Unlike all other services, where we just open a new terminal up
        -- for each different type..)
        , amplStdServiceLock :: MVar ()
        -- | Used to log strings...
        , amplLogger :: AmplLogger
        -- | channel manager -- map from GlobalChanID to input and output queues..
        -- Note: this could be pure and moved to a state monad (but we will
        -- stick to everything being crammed in a reader monad for now...)
        , channelManager :: IORef Chm
        -- | seed for the channel name
        , channelNameGenerator :: MVar (Stream ChannelIdRep)
        -- | a channel to broadcast commands to the channel manager. We call this the broadcast channel...
        , broadcastChan :: Chan BInstr
        -- | Corresponding to the size of the broadcase channel..
        , broadcastChanSize :: MVar Word
        -- | number of running processes (used for testing temrination...)
        , numRunningProcesses :: MVar Word
    }

    

-- | Smart constructor for the environment
amplEnv :: 
    [(FunID, (String, [Instr]))] ->          -- ^ association list of funciton ids and its name / instruction
    AmplTCPServer ->                         -- ^ AmplTCP server
    AmplLogger ->                            -- ^ logger.
    (Services, Chm, Stream ChannelIdRep) ->  -- ^ service related things
    IO AmplEnv
amplEnv defs tcpsv lg (svs, chm, nmg) = do
    chan <- newChan
    mchm <- newIORef chm
    nmg' <- newMVar nmg
    -- every program starts with running 0 processes
    numrunpr <- newMVar 0  
    -- every program starts with nothing in the broadcast channel
    broadcastchansize <- newMVar 0  

    stdsvlock <- newMVar ()  

    queuedclients <- initQueuedClients svs

    tcpmvarid <- newEmptyMVar

    return AmplEnv
            {
                supercombinators = mkSupercombinators defs
                , amplServices = svs
                , amplStdServiceLock = stdsvlock
                , amplTCPServer = (tcpmvarid, tcpsv)
                , amplQueuedClients = queuedclients
                , amplLogger = lg
                , broadcastChan = chan
                , broadcastChanSize = broadcastchansize
                , channelManager = mchm
                , channelNameGenerator = nmg'
                , numRunningProcesses = numrunpr
            }

instance HasSuperCombinators Supercombinators where
    superCombInstrLookup (Supercombinators arr) ix = snd (arr ! ix)
    superCombNameLookup (Supercombinators arr) ix = fst (arr ! ix)

instance HasSuperCombinators AmplEnv where
    superCombInstrLookup env ix = (`superCombInstrLookup`ix) (supercombinators env)
    superCombNameLookup env ix = (`superCombNameLookup`ix) (supercombinators env)

instance HasLog AmplEnv where
    getFileLog AmplEnv { amplLogger = logger } = nonRedundantFileAmplLogger logger dashesTimeStampLn (return dashesLn) 
    getStdLog AmplEnv { amplLogger = logger } = amplLogStdOut logger

logStdAndFile :: HasLog a => a -> String -> IO ()
logStdAndFile env str = getStdLog env str >> getFileLog env str


instance HasChannelManager AmplEnv where
    getChannelManager = channelManager

instance HasProcessCounter AmplEnv where
    getNumRunningProcesses env = liftIO $ readMVar (numRunningProcesses env)
    succNumProcesses env = liftIO $ modifyMVar_ (numRunningProcesses env) (return . succ)
    predNumProcesses env = liftIO $ modifyMVar_ (numRunningProcesses env) (return . pred)

instance HasBroadcastChan AmplEnv where
    -- Remark: we know that both writeBroadcastChan and readBroadcastChan do not block for long periods of time
    -- Hence, it is safe to use uninterruptibleMask_
    writeBroadcastChan AmplEnv{ broadcastChan = bch, broadcastChanSize = bchsz } n = liftIO . uninterruptibleMask_ $ do
        writeChan bch n 
        modifyMVar_ bchsz (return . succ)
        return ()

    readBroadcastChan AmplEnv{ broadcastChan = bch, broadcastChanSize = bchsz } = liftIO . uninterruptibleMask_ $ do
        n <- readChan bch 
        modifyMVar_ bchsz (return . pred) 
        return n

    getSizeOfBroadcastChan AmplEnv{ broadcastChanSize = bchsz } = 
        liftIO $ readMVar bchsz

instance HasChannelNameGenerator AmplEnv where
    getNewChannelName AmplEnv{ channelNameGenerator = chg } = liftIO $ 
        GlobalChanID <$> modifyMVar chg (return . (Stream.tail &&& Stream.head))

    returnChannelName AmplEnv{ channelNameGenerator = chg } chid = liftIO $ 
        modifyMVar chg  (return . (Stream.Stream (coerce chid) &&& const ()))

instance HasAmplServices AmplEnv where
    lookupServiceEnv gch AmplEnv{ amplServices = svs } = Map.lookup gch svs
    getStdServiceLock AmplEnv{ amplStdServiceLock = lk } = lk

instance HasNetworkedConnections AmplEnv where
    getTCPServerInfo = snd . amplTCPServer

    getQueuedClients = amplQueuedClients

    putTcpThreadId AmplEnv{ amplTCPServer = (m, _) } = putMVar m 

    takeTcpThreadId AmplEnv{ amplTCPServer = (m, _) } = takeMVar m

