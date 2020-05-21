module AMPL where

import AMPLSequential
import AMPLConcurrent
import AMPLTypes
import AMPLEnv
import AMPLMach
import AMPLLogger

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

{-
    This file includes helpful wrappers around 
    runAMPLMach from AMPLMach to make running the
    machine easier..
-}

-- |  wrapper around execAMPLMach specifically designed for the AmplEnv type
-- with a default logger
execAMPLMachWithDefaultLogger :: 
    ([Instr], [Translation]) ->                         -- ^ Main function
    ([(FunID, (String, [Instr]))]                       -- ^ Function definitions..
    , (Services, Chm, Stream Word) ) ->                 -- ^ note that Services and Chm 
                                                        -- must correspond (i.e., if a global channel is in
                                                        -- Services, then there should be corresponding
                                                        -- empty queues with that global channel id
                                                        -- and each of these MUST be distinct from the elemnts in
                                                        -- Stream Word
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
    , (Services, Chm, Stream Word)) ->
    IO ()
execAMPLMach mainf amplenv = do
    env <- amplEnv amplenv
    runReaderT (runAMPLMach mainf :: ReaderT AmplEnv IO ()) env
