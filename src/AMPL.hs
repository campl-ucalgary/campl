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

import Network.Socket

{-
    This file includes helpful wrappers around 
    runAmplMach from AmplMach (and more!) to make running the
    machine easier..
-}

-- |  wrapper around execAmplMach specifically designed for the AmplEnv type
-- with a default logger and server
execAmplMachWithDefaults :: 
    ([Instr], [Translation]) ->                         -- ^ Main function
    [(FunID, (String, [Instr]))] ->                     -- ^ Function definitions..
    String ->                                           -- ^ AmplTCPServer port
    (Services, Chm, Stream ChannelIdRep) ->             -- ^ note that Services and Chm 
                                                        -- must correspond (i.e., if a global channel is in
                                                        -- Services, then there should be corresponding
                                                        -- empty queues with that global channel id
                                                        -- and each of these MUST be distinct from the elemnts in
                                                        -- Stream ChannelIdRep
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
    (Services, Chm, Stream ChannelIdRep) ->
    IO ()
execAmplMach mainf fdefs tcpsv lgr svs = 
    bracket 
        (initAmplTCPServer tcpsv)
        (close' . serverSocket)
        $ \tcp -> do 
            env <- amplEnv fdefs tcp lgr svs
            runReaderT (runAmplMach mainf :: ReaderT AmplEnv IO ()) env

-- | Default way to generate services. Expects all SERVICE channels
-- to be less than or equal to 0 (throws error). Then, all internal
-- channels should be positive (this is unchecked)...

-- Service Channels will have a ``(ServiceDataType, ServiceType)"
-- Note that by convention (Prashant), 0 should be the stdin/stdout 
-- int terminal and -100 is the stdin/sdtout char terminal
genServicesChmAndStream :: 
    [GlobalChanID] ->                                       -- ^ internal channels
    [(GlobalChanID, (ServiceDataType, ServiceType))] ->     -- ^ external channels
    IO (Services, Chm, Stream ChannelIdRep)                 
genServicesChmAndStream internal externalassocs 
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

    chm = initChm (map fst externalassocs ++ internal)

    ermsg = "User error: Invalid channel -- all channels should be negative, but we have: \n" 
        ++ show externalassocs
    
