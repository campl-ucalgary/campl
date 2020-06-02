module AMPLMach where

import AMPLEnv
import AMPLTypes
import AMPLConcurrent
import AMPLSequential
import AMPLLogger
import AMPLServices
import ServiceConstants

import Data.Queue (Queue)
import qualified Data.Queue as Queue
import Data.Stream (Stream)
import qualified Data.Stream as Stream

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Exception
import Text.Read
import Data.IORef
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import System.Process
import Network.Socket

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

{-
    This file is for the AMPL machine. 

    It includes the loops and state update to actually
    run the thing...
-}

-- | runAMPLMach will run the machine. 
runAmplMach :: 
    ( HasBroadcastChan r 
    , HasChannelManager r 
    , HasProcessCounter r 
    , HasAmplServices r 
    , HasSuperCombinators r 
    , HasNetworkedConnections r
    , HasLog r 
    , HasChannelNameGenerator r ) =>
    ([Instr], [Translation]) ->    -- ^ (main process instructions, translations)
    ReaderT r IO ()
runAmplMach (mainf, maint) = do
    env <- ask 
    -- starts the main process...
    amplForkProcess ([], maint, [], mainf) 

    -- Opens the tcp server and get its threadid (needed to terminate the server)
    tcpid <- liftIO $ forkIO (catch (runReaderT amplRunTCPServer env ) (\e ->  return (const () (e :: AmplExit)))) 
                    -- runs the server, and catches if it recieves an AmplExit exception
    liftIO $ putTcpThreadId env tcpid

    amplMACHLoop 

-- | main loop for the channel manager...
amplMACHLoop :: 
    ( HasChannelManager r 
    , HasBroadcastChan r 
    , HasProcessCounter r 
    , HasAmplServices r 
    , HasSuperCombinators r 
    , HasNetworkedConnections r
    , HasLog r 
    , HasChannelNameGenerator r) =>
    ReaderT r IO ()
amplMACHLoop = do
    env <- ask 
    bdchsz <- getSizeOfBroadcastChan env
    -- reads multiple at once 
    --bdcmds <- sequence (genericReplicate bdchsz (readBroadcastChan env))
    bdcmds <- if bdchsz > 0 then pure <$> readBroadcastChan env else return []
    -- we can use either of the two... Although, the latter is a little more
    -- fair with how commands get processed since we only processes exactly one at a time.

    chm <- liftIO $ readIORef (getChannelManager env)

    let chm' = foldl (flip addCommandToChannelManager) chm bdcmds
        -- remark: this MUST be foldl and correspond to the order of
        -- which commands were put in otherwise this will not play well
        -- with certain commands. For example, if it is foldr, then, with the
        -- plug command, this will silently drop certain commands...
        (StepChannelManagerResult { 
            chmNewProcesses = ps
            , chmFreeChannelNames = ngchs
            , chmNewServices = svs
            }, chm'') = stepChannelManager chm'
        
    -- update the channel manager...
    liftIO $ writeIORef (getChannelManager env) chm''

    -- logging
    amplLogChm chm'
    amplLogChm chm''

    -- Fork the new processes, and run new services...
    mapM_ amplForkProcess ps
    mapM_ amplRunService svs

    -- the order of these are important for termination...
    numrningprs <- getNumRunningProcesses env
    bdchsz' <- getSizeOfBroadcastChan env 

    -- termination condition...
    if numrningprs == 0 && bdchsz == 0 && bdchsz' == 0 && chm == chm''
        then liftIO $ do
            tid <- takeTcpThreadId env
            throwTo tid AmplExit
            return ()
        else amplMACHLoop
    {-
    This is the termination condition. If there are no running 
    proccesses, the size of the broadcast channel at the start and
    end is 0, and the channel manager was unchanged, this implies 
    there is no more work to be done (termination condition)

    We briefly explain this. Note that we explain in the order of
        - numrningprs == 0 
        - bdchsz' == 0 
        - bdchsz == 0 && chm == chm''
    which is different from the order presented because of shortcircuiting
    (lazy evalation) of Haskell.

    Firstly, if numrningprs == 0, then we know there are no processes
    running. By inspection of all cases where a process dies (and hence,
    the counter is decremented), we know that it will only spawn
    a new process before it dies (if it should) in which case 
    the total number of processes would not be 0. Hence, when there
    are 0 processes, there must really be 0 processes running (until
    the next loop iteration which would of course potentialy spawn more 
    processes).

    If bdchsz' == 0, then we know that the size of the broadcast
    channel is 0. This means that there are no more commands
    to be processed. In particular, since we already know that
    there are no more processes alive, we know that if a proceess
    were to send a broadcast command, then it would have waited to
    send the broadcast command before termiating. So, since there
    are no more processes alive and there are 0 broadcast commands
    to be executed, this implies that there is no more work to 
     be done processing broadcast commands.

    Finally,  if bdchsz == 0 and chm == chm'', this implies that 
    this channel manager did not change at all and since from the
    aforementioned conditions, this implies that there is no more
    work to be done and we can terminate.
    -}

-- | runs the TCP server to accept connections
amplRunTCPServer ::
    ( HasProcessCounter r
    , HasAmplServices r
    , HasNetworkedConnections r
    , HasLog r ) => 
    ReaderT r IO ()
amplRunTCPServer = do 
    env <- ask
    let tcpserver = getTCPServerInfo env
        queuedclients = getQueuedClients env
    liftIO $ do
        logStdAndFile env "Server is waiting for a connection..."
        (clientsock, clientaddr) <- accept (serverSocket tcpserver)
        logStdAndFile env ("Accepted connection: " ++ show clientaddr)
        clienthandle <- socketToHandle clientsock ReadWriteMode
        hSetBuffering clienthandle LineBuffering

        -- Get the key from the client...
        logStdAndFile env "Waiting for key from client..."
        k <- Key <$> hGetLine clienthandle
        logStdAndFile env ("Key received: " ++ show k)

        case Map.lookup k queuedclients of
            -- if it is a valid key, proceed with connecting the channel manager to it
            Just client -> do
                logStdAndFile env ("Key authenticated with " ++ show clientaddr ++ " and " ++ show k)
                putMVar client (clienthandle, clientaddr)
            -- Otherwise, just say its an invalid connection
            Nothing -> do 
                logStdAndFile env ("Invalid connection from " ++ show clientaddr ++ " with key " ++ show k)
                hPutStrLn clienthandle "Invalid client connection..."
                hClose clienthandle

    -- recurse..
    amplRunTCPServer

-- | Runs a service. It will open the service again if it has not already been
-- opened..
amplRunService :: 
    ( HasProcessCounter r
    , HasAmplServices r
    , HasBroadcastChan r
    , HasNetworkedConnections r
    , HasLog r ) =>
    ServiceQuery -> ReaderT r IO ()
amplRunService (gch, rq) = do 
    env <- ask 
    let svenv = fromJust $ lookupServiceEnv gch env
    -- put the service request in for the server...
    liftIO (writeChan (serviceRequest svenv) rq)
    -- open the service (if it needs to be opened)...
    oldOpen <- liftIO (modifyMVar (serviceOpen svenv) (f env svenv))
    case oldOpen of
        ServiceIsClosed -> liftIO (runReaderT (amplOpenService (gch, svenv)) env)
        _ -> return ()
  where
    -- change the service to be opened...
    f env svenv ServiceIsClosed = 
            return (ServiceIsOpen, ServiceIsClosed)
    -- Otherwise, leave it as open and do nothing 
    -- (thread should be running and taking requests)
    f env svenv ServiceIsOpen = 
        return (ServiceIsOpen, ServiceIsOpen)
    
-- | amplOpenService will open a service..
amplOpenService :: 
    ( HasProcessCounter r
    , HasAmplServices r
    , HasNetworkedConnections r
    , HasBroadcastChan r
    , HasLog r ) =>
    (GlobalChanID, ServiceEnv) -> ReaderT r IO ()
amplOpenService sv@(_, svenv) = do 
    env <- ask 
    succNumProcesses env
    case serviceType svenv of
        StdService -> 
            void $ liftIO $ forkIO (runReaderT (amplStdServiceLoop sv) env)
        NetworkedService k -> void $ liftIO $ forkIO (runReaderT (amplOpenNetworkedService k sv) env)
        TerminalNetworkedService cmd k -> liftIO $ do
            forkIO (runReaderT (amplOpenNetworkedService k sv) env)
            void $ createProcess (shell cmd) 

-- | This will open a networked service.
amplOpenNetworkedService :: 
    ( HasProcessCounter r
    , HasAmplServices r
    , HasNetworkedConnections r
    , HasBroadcastChan r
    , HasLog r ) =>
    Key -> (GlobalChanID, ServiceEnv) -> ReaderT r IO ()
amplOpenNetworkedService k sv = do
    env <- ask
    let queuedclients = getQueuedClients env
        mclientinfo = fromJust (Map.lookup k queuedclients)
    clientconnection <- liftIO $ takeMVar mclientinfo
    amplNetworkedServiceLoop clientconnection sv

-- | Main loop for a networked service
amplNetworkedServiceLoop :: 
    ( HasProcessCounter r
    , HasAmplServices r
    , HasNetworkedConnections r
    , HasBroadcastChan r
    , HasLog r ) =>
    (Handle, SockAddr) ->           -- ^ client connection (note that we do not actually need SockAddr)
    (GlobalChanID, ServiceEnv) ->   -- ^ required environment from the service
    ReaderT r IO ()
amplNetworkedServiceLoop client@(clienthandle, clientaddr) sv@(gch, ServiceEnv{ serviceDataType = svdty, serviceOpen = open, serviceRequest = svrq }) = do
    env <- ask
    rq <- liftIO (readChan svrq)
    case rq of 
        ServiceGet pol -> do
            liftIO $ hPutStrLn clienthandle getRequest 
            v <- liftIO networkGet
            writeBroadcastChan env (BPut (pol, gch) v)
            amplNetworkedServiceLoop client sv

        ServicePut v -> do
            liftIO (hPutStrLn clienthandle putRequest >> networkPut (show v))
            amplNetworkedServiceLoop client sv

        ServiceClose -> do
            liftIO $ hPutStrLn clienthandle closeRequest
            liftIO $ modifyMVar_ open (return . const ServiceIsClosed)
            liftIO $ hClose clienthandle
            predNumProcesses env
            return ()
  where
    networkGet = case svdty of
        IntService -> do
            hPutStrLn clienthandle "Please enter an integer..."
            networkIntGet
        CharService -> do
            hPutStrLn clienthandle "Please enter a character..."
            networkCharGet
    
    networkIntGet = do
            mint <- readMaybe <$> hGetLine clienthandle :: IO (Maybe Int)
            case mint of 
                Just int -> do
                    hPutStrLn clienthandle validGetRequest
                    return (VInt int)
                Nothing -> do
                    hPutStrLn clienthandle invalidGetRequest
                    hPutStrLn clienthandle "Invalid integer... Please try again..."
                    networkIntGet

    networkCharGet = do
        mchar <- hGetLine clienthandle
        case mchar of
            [c] -> do
                hPutStrLn clienthandle validGetRequest
                return (VChar c)
            _ -> do
                hPutStrLn clienthandle invalidGetRequest
                hPutStrLn clienthandle "Invalid character... Please try again..."
                networkCharGet

    networkPut = hPutStrLn clienthandle
        
-- Main loop for a stadnard input and output service
amplStdServiceLoop ::
    ( HasProcessCounter r
    , HasAmplServices r
    , HasBroadcastChan r
    , HasLog r ) =>
    (GlobalChanID, ServiceEnv) -> ReaderT r IO ()
amplStdServiceLoop sv@(gch, ServiceEnv{ serviceDataType = svdty, serviceOpen = open, serviceRequest = svrq }) = do
    env <- ask
    let stdsvlk = getStdServiceLock env
    rq <- liftIO (readChan svrq)
    case rq of
        ServiceGet pol -> do
            v <- liftIO $ withMVar stdsvlk (const (stdGet env))
            writeBroadcastChan env (BPut (pol, gch) v)
            amplStdServiceLoop sv

        ServicePut v -> do
            liftIO (withMVar stdsvlk (const (stdPut env v)))
            amplStdServiceLoop sv

        ServiceClose -> do
            liftIO $ modifyMVar open (return . (const ServiceIsClosed &&& const ()))
            predNumProcesses env
            return ()
            -- do not recurse..
  where
    stdGet env = case svdty of 
                IntService -> do
                    getStdLog env "Please enter an integer..."
                    stdGetInt env
                CharService -> 
                    stdGetChar env


    stdGetInt env = do
        mint <- readMaybe <$> getLine 
        case mint of
            Just int -> return (VInt int)
            Nothing -> getStdLog env "Please try again (invalid int).." >> stdGetInt env

    stdGetChar env = do
        getStdLog env "Please enter a character..."
        chars <- getLine 
        case chars of
            [c] -> return (VChar c)
            _ -> getStdLog env "Please try again (invalid char).." >> stdGetChar env

    stdPut env = getStdLog env . show

    
    
-- | Forks an ampl process.. Note that using this process is necessary
-- for termination since it modifies the number of processes (used for the
-- termination condition)..
amplForkProcess :: 
    ( HasProcessCounter r
    , HasBroadcastChan r
    , HasSuperCombinators r
    , HasLog r 
    , HasChannelNameGenerator r ) =>
    Stec -> ReaderT r IO ThreadId
amplForkProcess stec = do
    env <- ask
    succNumProcesses env
    liftIO $ forkIO (runReaderT (amplProcessLoop stec) env >> predNumProcesses env)
    -- Note the ordering -- on the main thread, we increment the numnber of processes
    -- then leave it to the forked thread when it finishes to decrement it

-- | Main loop for an amplProcess
amplProcessLoop :: 
    ( HasBroadcastChan r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasLog r 
    , HasChannelNameGenerator r) =>
    Stec -> 
    ReaderT r IO ()
amplProcessLoop stec = amplLogProcess stec >> f stec
  where
    f stec = 
        case stec of
            (s,t,e,ConcurrentInstr c:cs) -> do
                ps <- stepConcurrent c (s,t,e,cs)
                case ps of 
                    ProcessEnd -> return ()
                    ProcessContinue p -> amplProcessLoop p
                    ProcessDiverge p1 p2 -> do
                        amplForkProcess p1
                        amplForkProcess p2
                        return ()
            (s,t,e,SequentialInstr c:cs) -> do
                (cs', e', s') <- stepSequential c (cs, e, s)
                amplProcessLoop (s',t,e',cs')
            _ -> return ()


-- | Logs a process
amplLogProcess :: HasLog r => Stec -> ReaderT r IO ()
amplLogProcess (s,t,e,c) = do
    env <- ask
    thid <- liftIO myThreadId
    liftIO $ getFileLog env
        (intercalate "\n" 
            [ 
            "Process on: " ++ show thid
            , "Stack:"
            , render (doc s)
            , "Translations:"
            , render (doc t)
            , "Environment:"
            , render (doc e)
            , "Code:"
            , render (doc c)
            ]
        )

-- | Logs the channel manager
amplLogChm :: ( HasLog r, HasProcessCounter r ) => Chm -> ReaderT r IO ()
amplLogChm chm = do
    env <- ask
    numrningprs <- getNumRunningProcesses env
    liftIO $ getFileLog env
        (intercalate "\n" $
            "Channel Manager: ": map show (Map.toList chm) ++
            [ "\nNumber of running processes: "
            , show numrningprs
            ]
        )
