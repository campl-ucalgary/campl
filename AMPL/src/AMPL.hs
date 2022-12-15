{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
 module AMPL where

import AMPLSequential
import AMPLConcurrent
import AMPLTypes
import AMPLEnv
import AMPLMach
import AMPLLogger
import AMPLServices

import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Data.Queue (Queue)
import qualified Data.Queue as Queue

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception
import System.Environment
import Data.List
import Data.Bool

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Network.Socket

{-
    This file includes helpful wrappers around 
    runAmplMach from AmplMach (and more!) to make running the
    machine easier..
-}

{-
The general structure for writing the program is as follows...
main = do
    svs <- genServicesChannelManagerAndStream 
                    [ {- Non service channels -} ] 
                    [ {- Service channels -} ]
    execAmplMachWithDefaults 
        {- (main function instructions, translations) -}
        [ {- (function index, function name, function definition) -} ]
        {- Port to run TCP server on e.g. 5000 -}
        svs
-}


data InitAMPLMachState = InitAMPLMachState {
    initAmplMachStateServices :: ([GlobalChanID], [(GlobalChanID, (ServiceDataType, ServiceType))])
            -- ^ interanl services, external services..
    , initAmplMachMainFun :: ([Instr], [Translation]) 
            -- ^ Main function, translations
    , initAmplMachFuns :: [(FunID, (String, [Instr]))]
    }
  deriving (Show, Read, Eq, Generic, Out)

execAmplMachWithDefaultsFromInitAMPLMachState :: 
    String ->               -- ^ Port number
    InitAMPLMachState -> 
    IO ()
execAmplMachWithDefaultsFromInitAMPLMachState port InitAMPLMachState 
    { initAmplMachStateServices = svs 
    , initAmplMachMainFun = mainfun
    , initAmplMachFuns = funs } = do
        svs' <- uncurry genServicesChannelManagerAndStream svs
        execAmplMachWithDefaults mainfun funs port svs'

-- |  wrapper around execAmplMach specifically designed for the AmplEnv type
-- with a default logger (logs to logs/filnameXX_log.txt) and opens the server for you
execAmplMachWithDefaults :: 
    ([Instr], [Translation]) ->                         -- ^ Main function
    [(FunID, (String, [Instr]))] ->                     -- ^ Function definitions..
    String ->                                           -- ^ AmplTCPServer port
    (Services, ChannelManager, Stream ChannelIdRep) ->  -- ^ note that Services and Chm 
                                                        -- must correspond (i.e., if a global channel is in
                                                        -- Services, then there should be corresponding
                                                        -- empty queues with that global channel id
                                                        -- and each of these MUST be distinct from the elements in
                                                        -- Stream ChannelIdRep. Use genServicesChannelManagerAndStream to generate
                                                        -- this triple)
    IO ()
execAmplMachWithDefaults mainf fdefs tcpsvr svs = 
    bracket 
        (do prgname <- getProgName ;  initAmplLogger "logs" (prgname ++ "_log.txt"))
        closeAmplLoggerWithTimeStamp
        (\lgger -> execAmplMach mainf fdefs tcpsvr lgger svs)

-- | Wrapper around runAmplMach specifically for the AmplEnv type...
execAmplMach :: 
    ([Instr], [Translation]) ->                     -- ^ Main function
    [(FunID, (String, [Instr]))] ->                 -- ^ Function definitions..
    String ->                                       -- ^ AmplTCPServer Port
    AmplLogger ->                                   -- ^ logger
    (Services, ChannelManager, Stream ChannelIdRep) ->
    IO ()
execAmplMach mainf fdefs tcpsv lgr svs = 
    bracket 
        (initAmplTCPServer tcpsv)
        (close . serverSocket)
        $ \tcp -> do 
            env <- amplEnv fdefs tcp lgr svs
            runReaderT (runAmplMach mainf :: ReaderT AmplEnv IO ()) env

-- | Default way to generate services. Expects all SERVICE channels
-- to be less than or equal to 0 (throws error if this is not the case). 
-- Moreover, all internal channels should be positive (this is unchecked)....

-- Service Channels will have a corresponding ``(ServiceDataType, ServiceType)."

-- Note that by convention (Prashant), 0 should be the stdin/stdout 
-- int terminal and -100 is the stdin/sdtout char terminal (although, this machine
-- will run programs perfectly fine if this is not the case)
genServicesChannelManagerAndStream :: 
    [GlobalChanID] ->                                       -- ^ internal channels
    [(GlobalChanID, (ServiceDataType, ServiceType))] ->     -- ^ external channels (services)
    IO (Services, ChannelManager, Stream ChannelIdRep)                 
genServicesChannelManagerAndStream internal externalassocs 
    | all checkValid externalassocs = do
        svs <- initAmplServices externalassocs 
        return (svs, chm, strm)
    | otherwise = error ermsg
  where
    checkValid = (<= GlobalChanID 0) . fst

    strm = Stream.iterate succ imax

    imax = succ $ case map (\(GlobalChanID n) -> n) internal of
        [] -> 0
        xs -> maximum xs

    chm = initChannelManager (map fst externalassocs ++ internal)

    ermsg = "User error: Invalid channel -- all channels should be negative, but we have: \n" 
        ++ show externalassocs
    
