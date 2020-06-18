module AMPLServices where

import AMPLTypes

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad 
import Control.Exception
import Control.Concurrent 
import Control.Concurrent.Chan
import Data.List
import Data.Maybe
import Data.IORef
import System.IO
import System.Process
import Network.Socket
import Text.Read

{-
    File for Services for AMPL. Services are processes that interact
    with the real world.. All init*ServiceHandle functions given
    do not handle exceptions and MOST LIKELY WILL THROW AN EXCEPTION
-}

-- | Sum type for the possible data types a service may accept and not accept
data ServiceDataType = 
    IntService
    | CharService
    deriving (Show, Read, Eq)

-- | External services require a Key..
newtype Key = Key String
    deriving (Show, Read, Eq, Ord)

-- | Sum type for the different kind of services..
data ServiceType = 
    StdService                                  -- ^ standard service
    | NetworkedService Key                      -- ^ external network service
    | TerminalNetworkedService String Key       -- ^ external network service that opens a terminal (command, and key)
    deriving (Show, Read, Eq)

-- Information for querying a service...
type ServiceQuery = (GlobalChanID, ServiceRequest Val)
    -- GlobalChanID (the service corresponding to that channel)

    -- Then, from data in AMPLService, we can deduce where we want to get
    -- the data from, and the type of the data we would like to get (e.g., int, char, etc)...

-- | Sum type for telling if a service is open or not
data ServiceOpen = 
    ServiceIsOpen 
    | ServiceIsClosed
    deriving Show

-- | Different kind of requests for a service. Services can either: get, put, or be closed.
data ServiceRequest a = 
    ServiceGet Polarity     -- ^ get a value from a service e.g., ask for input for a terminal...
                            -- Note that this requires the Polarity to put our gotten value on...
                            -- 1 corresponds to this

    | ServicePut a          -- ^ put a value on the service e.g., putStrLn on a terminal..
                            -- 2 corresponds to this 

    | ServiceClose          -- ^ closes a service..
                            -- ^ 3 corresponds to this
    deriving (Show, Eq)

data ServiceEnv = ServiceEnv {
        serviceType :: ServiceType
        , serviceDataType :: ServiceDataType
        , serviceOpen ::  MVar ServiceOpen
        , serviceRequest :: Chan (ServiceRequest Val)
    } 
    -- Maps GlobalChanID to a ServiceEnv
    -- We have the ServiceDataType (e.g. either int or char, etc), the ServiceType (
    -- internal services are different form external services)
    -- MVar ServiceIsOpen corresponds if it the service has been opened already or it
    -- is closed (note that StdService is always open by default)

    -- MVar [ServiceRequest Val] get, put, and close requests. Recall the confusing
    -- convention from Prashant where 1,2, and 3 are get, put and close respecitvely

-- | Map for the dfifferent kinds of services
type Services = Map GlobalChanID ServiceEnv

emptyAmplServices :: Services
emptyAmplServices = Map.empty

-- | Each Key has an associated Queued clients.
-- This is important for networked connections where
-- each networked connection has a unique key.
type QueuedClients = Map Key (MVar (Handle, SockAddr))

-- | Get all the networking keys
initQueuedClients :: Services -> IO QueuedClients
initQueuedClients = 
    (Map.fromList <$>)
    . sequence
    . mapMaybe (f . serviceType)
    . Map.elems
  where
    f StdService = Nothing
    f (NetworkedService k) = Just $ do
        m <- newEmptyMVar
        return (k, m)
    f (TerminalNetworkedService _ k) = Just $ do
        m <- newEmptyMVar
        return (k, m)

-- | Helpful wrapper to generate the Services (recall Services is a map)
initAmplServices :: 
    [(GlobalChanID, (ServiceDataType, ServiceType))] -> 
    IO Services 
initAmplServices svs = 
    Map.fromAscList <$> mapM f (sortBy (\a b -> compare (fst a) (fst b)) svs)
  where
    f (gch, (svdt, svt)) = do
        opn <- newMVar ServiceIsClosed
        rqs <- newChan
        return (gch, ServiceEnv svt svdt opn rqs)

-- | wrapper for a TCP server's state (needed for
-- the clients)
data AmplTCPServer = AmplTCPServer { 
    tcpServerLock :: MVar ()            -- ^ lock for the TCP server's state (not needed actually because later only one thread is accessing the server at at time)
    , serverAddress :: SockAddr         -- ^ Server address
    , serverPort :: String              -- ^ port the server is running on
    , serverSocket :: Socket            -- ^ Socket listening for connections
}


-- | Opens a TCP server... 
initAmplTCPServer :: 
    String ->                  -- ^ port e.g. 514
    IO AmplTCPServer 
initAmplTCPServer port = withSocketsDo $ do
    -- look up the port..  From the documentation, 
    -- raises an exception or returns a non empty list
    addrinfos <- getAddrInfo 
                    (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                    -- AI_PASSIVE gives us a wildcard IP address
                    Nothing
                    (Just port)
    let serveraddr = head addrinfos

    -- create socket
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol

    -- Easier to debug (let's multiple sockets use the same port)
    setSocketOption sock ReusePort 1

    -- bind to the address we are listening too
    bind sock (addrAddress serveraddr)

    -- maximum queue size of 5 (most OS's only allow 5)
    listen sock 5

    lock <- newMVar ()

    return $ AmplTCPServer {
        tcpServerLock = lock
        , serverAddress = addrAddress serveraddr
        , serverPort = port
        , serverSocket = sock
    }
