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

{-
    This file is for the AMPL machine. 

    It includes the loops and state update to actually
    run the thing...
-}

-- | runAMPLMach will run the machine. 
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
    -- reads multiple at once 
    --bdcmds <- sequence (genericReplicate bdchsz (readBroadcastChan env))
    bdcmds <- if bdchsz > 0 then pure <$> readBroadcastChan env else return []
    -- we can use either or...

    chm <- readIORef (getChannelManager env)

    let chm' = foldl (flip addCommandToChannelManager) chm bdcmds
        -- remark: this MUST be foldl and correspond to the order of
        -- which commands were put in otherwise this will not play well
        -- with certain commands. For example, if it is foldr, with the
        -- plug command, this will silently drop certain commands...
        (ps, chm'') = stepChannelManager chm'
        
    writeIORef (getChannelManager env) chm''

    logChm chm'
    logChm chm''

    mapM_ amplForkProcess ps

    -- the order of these are important for termination...
    numrningprs <- getNumRunningProcesses env
    bdchsz' <- getSizeOfBroadcastChan env

    unless (numrningprs == 0 && bdchsz == 0 && bdchsz' == 0 && chm == chm'') amplMACHLoop
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
amplProcessLoop stec = logProcess stec >> f stec
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

