{-# LANGUAGE FlexibleContexts #-}
module AMPLMach where

import AMPLEnv
import AMPLTypes
import AMPLConcurrent
import AMPLSequential
import AMPLLogger

import Control.MonadIORef
import Control.MonadChan
import Control.MonadConcurrent
import Data.Queue (Queue)
import qualified Data.Queue as Queue
import Data.Stream (Stream)
import qualified Data.Stream as Stream

import Control.Monad.Reader
import Data.List
import Control.Exception

-- | Wrapper around runAMPLMach specifically for the AmplEnv type...
execAMPLMach :: 
    ([Instr], [Translation]) ->                         -- ^ Main function
    ([(FunID, (String, [Instr]))]                       -- ^ Function definitions..
    , String -> IO ()                                   -- ^ logger
    , Chm                                               -- ^ channel manager..
    , Stream Word ) ->                                  -- ^ Channel name generator..
    IO ()
execAMPLMach mainf (fs, lg, chm, chmnmeg) = do
    env <- amplEnv fs lg chm chmnmeg
    runReaderT (runAMPLMach mainf :: ReaderT AmplEnv IO ()) env

-- | Call runAMPLMach to run the machine. 
runAMPLMach :: 
    ( HasBroadcastChan r 
    , HasChannelManager r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasLog r 
    , HasChannelNameGenerator r ) =>
    ([Instr], [Translation]) ->    -- ^ (main process instructions, translations)
    ReaderT r IO ()
runAMPLMach (mainf, maint) = do
    env <- ask 
    amplForkProcess ([], maint, [], mainf) 
    amplMACHLoop 

-- | main loop for the channel manager...
amplMACHLoop :: 
    ( HasChannelManager r 
    , HasBroadcastChan r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasLog r 
    , HasChannelNameGenerator r) =>
    ReaderT r IO ()
amplMACHLoop = do
    env <- ask 
    bdchsz <- getSizeOfBroadcastChan env
    bdcmds <- sequence (genericReplicate bdchsz (readBroadcastChan env))

    chm <- readIORef (getChannelManager env)
    logChm chm
    let (ps, chm') = stepChannelManager (foldr addCommandToChannelManager chm bdcmds)
    writeIORef (getChannelManager env) chm'

    mapM_ amplForkProcess ps

    runningps <- noProcessesRunning env
    bdchsz <- getSizeOfBroadcastChan env

    unless (chm == chm' && runningps && bdchsz == 0) amplMACHLoop
        -- ^ this is the termination condition. If the channel manager
        -- did not change, and there are no running processes, and the 
        -- size of the broadcastChan is 0, this implies that there are 
        -- no further actions that will happen in this machine.
        -- The order of getting the runningps then bdchsz is very important...
        -- If this is swapped, there will exist (albeit rare) instances when
        -- this will terminate prematurely e.g., if the exact moment we get
        -- bdchsz, there is nothing in it, but immediately after, the only 
        -- process in the system places something in bdchsz and terminates, we have 0 processes being executed -- But we can see that there is still 
        -- a BInstr in the broadcast channel to process.
    
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
    -- here is the only reason why the whole thing is 
    -- in the IO monad...
    liftIO $ bracket_ 
        (succNumProcesses env) 
        (predNumProcesses env) 
        (forkIO (runReaderT (amplProcessLoop stec) env))

-- | Main loop for an amplProcess
amplProcessLoop :: 
    ( HasBroadcastChan r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasLog r 
    , HasChannelNameGenerator r) =>
    Stec -> 
    ReaderT r IO ()
amplProcessLoop stec = logProcess stec >> f stec
  where
    f stec
        | isProcessFinished stec = return ()
        | otherwise = case stec of
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
            _ -> error "illegal state"

-- | process temrination condition as given in
-- Prashant's thesis..
isProcessFinished :: Stec -> Bool
isProcessFinished (_, [], [], []) = True
isProcessFinished _ = False
