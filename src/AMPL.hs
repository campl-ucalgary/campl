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

{-
    This file includes helpful wrappers around 
    runAMPLMach from AMPLMach (and more!) to make running the
    machine easier..
-}

-- |  wrapper around execAMPLMach specifically designed for the AmplEnv type
-- with a default logger
execAMPLMachWithDefaultLogger :: 
    ([Instr], [Translation]) ->                         -- ^ Main function
    ([(FunID, (String, [Instr]))]                       -- ^ Function definitions..
    , (Services, Chm, Stream ChannelIdRep) ) ->                 -- ^ note that Services and Chm 
                                                        -- must correspond (i.e., if a global channel is in
                                                        -- Services, then there should be corresponding
                                                        -- empty queues with that global channel id
                                                        -- and each of these MUST be distinct from the elemnts in
                                                        -- Stream ChannelIdRep
    IO ()
execAMPLMachWithDefaultLogger mainf (fdefs, (svs, chm, chmg)) = 
    bracket 
        (do prgname <- getProgName ;  initLogger "logs" (prgname ++ "_log.txt"))
        closeLogger 
        (\lgger -> execAMPLMach mainf (fdefs, fileLogger lgger, (svs, chm, chmg)))

-- | Wrapper around runAMPLMach specifically for the AmplEnv type...
execAMPLMach :: 
    ([Instr], [Translation]) ->                         -- ^ Main function
    ([(FunID, (String, [Instr]))]                       -- ^ Function definitions..
    , String -> IO ()                                   -- ^ logger
    , (Services, Chm, Stream ChannelIdRep)) ->
    IO ()
execAMPLMach mainf amplenv = do
    env <- amplEnv amplenv
    runReaderT (runAMPLMach mainf :: ReaderT AmplEnv IO ()) env

-- | Default way to generate services. Expects all channels
-- to be negative (throws error).
-- By convention (Prashant), 0 is stdin/stdout int terminal
-- and -100 is the stdin/sdtout char terminal
genServicesChmAndStream :: 
    String ->                           -- ^ command to open the terminals
    ServiceTypes [GlobalChanID] ->      -- ^ services types
    IO (Services, Chm, Stream ChannelIdRep)
genServicesChmAndStream cmd svstyps@(ServiceTypes intids charids) 
    | checkValidChannels intids && checkValidChannels charids = do
        stdhandle <- initStdServiceHandle
        undefined
        
        {-
        return (Services (ServiceTypes 
                            (Map.insert stdintchan stdhandle intids') 
                            (Map.insert stdcharchan stdhandle charids')
                            )
                , chm, stream)
                -}
    | otherwise = error 
        ("Invalid channel with genServicesChmAndStream. All channels must be negative and not equal to -100. " ++ show svstyps)

  where
    stdintchan = GlobalChanID 0
    stdcharchan = GlobalChanID (-100)

    allterminalglobalchannelids = stdintchan : stdcharchan : intids ++ charids

    chm = Map.fromAscList (zip (sort allterminalglobalchannelids) (repeat emptyQInstrQueues))
    stream = Stream.iterate succ 1

    checkValidChannels = all isValidChannel
    isValidChannel a = a < GlobalChanID 0 && a /= stdintchan && a /= stdcharchan
