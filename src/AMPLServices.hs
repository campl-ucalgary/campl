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

data ServiceDataType = 
    IntService
    | CharService
    deriving Show

-- external services require a Key..
newtype Key = Key String
    deriving (Show, Eq, Ord)

data ServiceType = 
    StdService                                  -- ^ standard service
    | NetworkedService Key                      -- ^ external network service
    | TerminalNetworkedService String Key       -- ^ external network service that opens a terminal (command, and key)
    deriving Show

-- Information for querying a service...
type ServiceQuery = (GlobalChanID, ServiceRequest Val)
    -- GlobalChanID (the service corresponding to that channel)

    -- Then, from data in AMPLService, we can deduce where we want to get
    -- the data from, and the type of the data we would like to get (e.g., int, char, etc)...

data ServiceOpen = 
    ServiceIsOpen 
    | ServiceIsClosed
    deriving Show

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

type Services = Map GlobalChanID ServiceEnv
    -- Maps GlobalChanID to (ServiceType, ServiceDataType, (MVar Bool, MVar [Int])).
    -- We have the ServiceDataType (e.g. either int or char, etc), the ServiceType (
    -- internal services are different form external services)
    -- MVar ServiceIsOpen corresponds if it the service has been opened already or it
    -- is closed (note that StdService is always open by default)

    -- MVar [ServiceRequest Val] get, put, and close requests. Recall the confusing
    -- convention from Prashant where 1,2, and 3 are get, put and close respecitvely

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
    tcpServerLock :: MVar ()
    , serverAddress :: SockAddr
    , serverPort :: String
    , serverSocket :: Socket 
}


-- | Opens a TCP server... 
initAmplTCPServer :: 
    String ->                  -- ^ port e.g. 514
    IO AmplTCPServer           -- ^ handle (read write) to the socket
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
