{-# LANGUAGE FlexibleContexts #-}
module AMPLMach where

import AMPLEnv
import AMPLTypes
import AMPLConcurrent
import AMPLSequential

import Control.MonadIORef
import Control.MonadChan
import qualified Data.Queue as Queue

import Control.Monad.Reader
import Control.Concurrent
import Data.Queue (Queue)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Exception

import Control.Concurrent.Async


runAMPLMach :: 
    ( MonadReader r IO
    , HasBroadcastChan r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasChannelNameGenerator r ) =>
    (([Instr], [Translation]), r) ->    -- ((main process instructions, translations), machine reader environment)
    IO ()
runAMPLMach ((mainf, maint), env) = do
    amplForkProcess ([], maint, [], mainf) env
    runReaderT amplMACHLoop env

amplMACHLoop :: 
    ( MonadReader r IO
    , HasBroadcastChan r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasChannelNameGenerator r) =>
    ReaderT r IO ()
amplMACHLoop = undefined
    -- HOME STRTCH, Just write this last little bit out okay? you'll be all done from here :)

-- | Forks an amplForkProcess.. Note that using this process is necessary
-- for termination since it modifies the number of processes (used for the
-- termination condition)..
amplForkProcess :: 
    ( MonadIO m
    , HasProcessCounter r
    , MonadReader r IO
    , HasBroadcastChan r
    , HasSuperCombinators r
    , HasChannelNameGenerator r ) =>
    Stec -> r -> m ThreadId
amplForkProcess stec env = liftIO $ 
    bracket_ (succNumProcesses env) (predNumProcesses env)
        (forkIO (runReaderT (amplProcessLoop stec) env))

-- | Main loop for an amplProcess
amplProcessLoop :: 
    ( MonadReader r IO
    , HasBroadcastChan r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasChannelNameGenerator r) =>
    Stec -> 
    ReaderT r IO ()
amplProcessLoop stec 
    | isProcessFinished stec = return ()
    | otherwise = case stec of
        (s,t,e,ConcurrentInstr c:cs) -> do
            ps <- lift (stepConcurrent c (s,t,e,cs))
            case ps of 
                ProcessEnd -> return ()
                ProcessContinue p -> amplProcessLoop p
                ProcessDiverge p1 p2 -> do
                    env <- ask
                    amplForkProcess p1 env
                    amplForkProcess p2 env
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
