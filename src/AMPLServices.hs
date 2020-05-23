{-# LANGUAGE DeriveAnyClass #-}
    -- DeriveAnyClass is used to be more clear about
    -- writing instances for the Exception class.
    -- i.e., instead of writing: 
    -- data MyException = ...
    -- instance Exception MyException where ...
    -- We can write: 
    -- data MyException = ... deriving Exception
module AMPLServices where

import AMPLTypes

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

import Control.Concurrent 
import System.IO
import Control.Exception
import System.Process
import Network.Socket
import Data.List
import Data.IORef

{-
    File for Services for AMPL. Services are processes that interact
    with the real world.. All init*ServiceHandle functions given
    do not handle exceptions and MOST LIKELY WILL THROW AN EXCEPTION
-}
newtype Services = Services { getServices :: ServiceTypes (Map GlobalChanID ServiceHandle) }

-- | A service handle.. it is recommended to use the 
-- concurrentReadHandle / concurrentWriteHandle to ensure that 
-- services are accessed properly. 
data ServiceHandle = ServiceHandle {
        shLock :: MVar ()
        , isOpened :: IO Bool
        , openService :: IO ()
        , closeService :: IO ()
        , readHandle :: IO String
        , writeHandle :: String -> IO ()
    }

concurrentIsOpened :: ServiceHandle -> IO Bool
concurrentIsOpened ServiceHandle{ shLock = lock, isOpened = isopn } =
    withMVar lock (const isopn)

concurrentOpenService :: ServiceHandle -> IO ()
concurrentOpenService ServiceHandle{ shLock = lock, openService = opn } =
    withMVar lock (const opn)

concurrentCloseService :: ServiceHandle -> IO ()
concurrentCloseService ServiceHandle{ shLock = lock, closeService = cls } =
    withMVar lock (const cls)

concurrentReadHandle :: ServiceHandle -> IO String
concurrentReadHandle ServiceHandle{ shLock = lock, readHandle = rdhdle} =
    withMVar lock (const rdhdle)

concurrentWriteHandle :: ServiceHandle -> String -> IO ()
concurrentWriteHandle ServiceHandle{ shLock = lock, writeHandle = wrthdle} str =
    withMVar lock (const (wrthdle str))

-- product type of all the service types..
data ServiceTypes a = ServiceTypes {
        intTerm :: a
        , charTerm :: a
    } deriving Show

-- | tests if a GlobalChanID is a service..
isService :: GlobalChanID -> Services -> Bool
isService gch (Services (ServiceTypes intterm charterm)) =
    gch `Map.member` intterm || gch `Map.member` charterm
    --  Could use SYB Typeable

-- | Standard input and io service handle
initStdServiceHandle :: IO ServiceHandle
initStdServiceHandle = do
    lock <- newMVar ()
    return $ ServiceHandle {
        shLock = lock
        -- stdin/stdout cannot be opened or closed / it is always
        -- open too
        , isOpened = return True
        , openService = return ()
        , closeService = return ()
        , readHandle = getLine
        , writeHandle = putStrLn
    }

data TerminalProcessHandleException =
    FailedToCloseTerminal
    | FailedToOpenTerminalReadHandle
    | FailedToOpenTerminalWriteHandle
    deriving (Show, Exception)

-- this actually just runs a command and does not really do anything..
initTerminalProcessHandle :: 
    String ->           -- ^ command to open up a terminal
    IO ServiceHandle
initTerminalProcessHandle cmd = do
    lock <- newMVar ()
    -- starts off unopened..
    isopen <- newIORef False

    readhdle <- newIORef Nothing
    writehdle <- newIORef Nothing
    prhdle <- newIORef Nothing

    return $ ServiceHandle {
        shLock = lock
        , isOpened = readIORef isopen
        , openService = do
            -- Maybe input handle, Maybe output handle, maybe error handle, process handle
            (inhdle, outhdle, errhdle, prhdle') <- createProcess (shell cmd)
            writeIORef readhdle inhdle
            writeIORef writehdle outhdle
            writeIORef prhdle (Just prhdle')
            writeIORef isopen True

        , closeService = do 
            -- From the documentation, "this should not be used
            -- under normal circumstances because -- no guarantees
            -- are given regarding how cleanly the process is 
            -- terminate", but in this system, we don't really care
            -- and we just want to close the process.
            readIORef prhdle >>= maybe (throw FailedToCloseTerminal) terminateProcess
            writeIORef readhdle Nothing
            writeIORef writehdle Nothing
            writeIORef prhdle Nothing
            writeIORef isopen False
        , readHandle = 
            readIORef readhdle >>= maybe (throw FailedToOpenTerminalReadHandle) hGetLine
        , writeHandle = \str -> 
            readIORef writehdle >>= maybe (throw FailedToOpenTerminalWriteHandle) (`hPutStrLn` str)
    }

-- | wrapper for a TCP server's state (needed for
-- the clients)
data AMPLTCPServer = AMPLTCPServer { 
    tcpServerLock :: MVar ()
    , serverPort :: String
    , serverSocket :: Socket 
    
    -- , MVar []
}

-- | Opens a TCP server... 
initAMPLTCPServer :: 
    String ->           -- ^ port e.g. 514
    IO AMPLTCPServer           -- ^ handle (read write) to the socket
initAMPLTCPServer port = withSocketsDo $ do
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

    undefined
    -- return handle
