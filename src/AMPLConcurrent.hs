{-# LANGUAGE TupleSections #-}
module AMPLConcurrent where

import AMPLTypes
import AMPLEnv
import Control.MonadChan
import Control.MonadIORef

import Control.Monad.Reader
import Data.Maybe
import Data.Functor
import Data.List
import Data.Coerce


{- 
    A first draft of the Concurrent machine
-}

data ConcurrentFlag =
    Unchanged   
        -- after processing the channels, nothing has changed
        -- i.e., the machine has finished computing..
    | Changed   
        -- after processing the channels, something has changed 
        -- i.e., we are not finished computing things...
    | RaceFinished      
        -- a race has finished (and we need to remove extra
        -- the race from the appriate channels) 

-- | steps a process by the focused concurrent instruction...
-- If an asynchronous exception is thrown to this thread, it will
-- put the whole system in an undefined state -- ensure that 
-- mask is used to ensure that this does not happen..
stepConcurrent :: 
    ( MonadReader r m
    , MonadChan m 
    , MonadAtomicIORef m 
    , HasBroadcastChan r 
    , HasProcessCounter r 
    , HasSuperCombinators r 
    , HasChannelNameGenerator r) =>
    ConcurrentInstr ->                                      
        -- ^ concurrent instruction to execute
    ([Val], [Translation], [Val], [Instr]) ->               
        -- ^ (s, t, e, c) of focused process...
    m [([Val], [Translation], [Val], [Instr])]
        -- ^ Either a list of 0, 1, or 2 elements..
        -- 0 implies the process is suspended..
        -- 1 implies the process is continuing..
        -- 2 implies the process has been plugged
stepConcurrent (IGet a) pr@(s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    chan <- asks getBroadcastChan
    writeChan chan (BGet ta pr)

    env <- ask ; predNumProcesses env 
    return []

stepConcurrent (IPut a) pr@(v:s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    chan <- asks getBroadcastChan
    writeChan chan (BPut ta v)

    return [(s, t, e, c)]

-- Technically the s must be empty from Table 2..
stepConcurrent (ISplit a (a1, a2)) (s, t, e, c) = do
    let ta@(pol, _) = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask ; b1 <- getNewChannelName env ; b2 <- getNewChannelName env
    chan <- asks getBroadcastChan
    writeChan chan (BSplit ta (b1, b2))

    return [(s, (pol, (a1, b1)):(pol, (a2, b2)):t, e, c)]
    -- new translations should have the same polarity as the orignal translation

stepConcurrent (IClose a) (s, t, e, c) = do 
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    chan <- asks getBroadcastChan
    writeChan chan (BClose ta)

    return [(s, deleteTranslation a t, e, c)]

stepConcurrent (IFork a ((a1, g1, c1), (a2, g2, c2))) (s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    chan <- asks getBroadcastChan
    writeChan chan 
        (BFork ta (t, e, (a1, restrictTranslation g1 t, c1), (a2, restrictTranslation g2 t, c2)))
        -- remark: we could do the restriction in the channel manager,
        -- but since the channel manager will probably be very busy managing
        -- channels for other processes, we make each process do it itself..
    env <- ask ; predNumProcesses env 
    return []

-- c should be empty (as said in Table 2)
-- NOTE -- Prashant does some weird stuff with this command.
-- LOOK INTO WHAT HE IS DOING AND HOW HE IS GENERATING THE CODE
-- SO IT WORKS WITH THIS ABSTRACT MACHINE -- I believe he wants
-- to use this to help identify services -- BUT we do not do that
-- in this machine -- this machine wants services and process' to 
-- be no different!
stepConcurrent (IId a b) (s, t, e, c) = do
    let (pa, ta) = fromJust (lookupLocalChanIDToGlobalChanID a t)
        (pb, tb) = fromJust (lookupLocalChanIDToGlobalChanID b t)
    chan <- asks getBroadcastChan
    if pa == pb 
        then do
            writeChan chan (BId (pa, ta) (ta, tb))
            env <- ask ; predNumProcesses env 
            return []
        else error "invalid IId instruction"

-- s should be empty here too
stepConcurrent (IPlug as ((g1, c1), (g2, c2))) (s, t, e, c) = do
    env <- ask
    as' <- mapM (const (getNewChannelName env)) as
    -- by convention (i.e., Prashant), the (g1,c1) is the output channel
    -- and (g2, c2) is the input channel
    let t1 = zipWith (curry (Output,)) as as' ++ restrictTranslation g1 t
        t2 = zipWith (curry (Input,)) as as' ++ restrictTranslation g2 t
    writeChan (getBroadcastChan env) (BPlug as')
    succNumProcesses env ; succNumProcesses env
    return [(s, t1, e, c1), (s, t2, e, c2)]

-- c should be empty here too
stepConcurrent (IHalt as) (s, t, e, c) = do
    let as' = map (fromJust . flip lookupLocalChanIDToGlobalChanID t) as
    env <- ask
    mapM_ (writeChan (getBroadcastChan env) . BHalt) as'
    predNumProcesses env
    return [] 

-- c should be empty
stepConcurrent (IRun t' fid args) (s, t, e, c) = do
    env <- ask
    let e' = genericTake args s
        s' = genericDrop args s
        t'' = fromJust (composeTranslation t' t)
        c' = superCombInstrLookup env fid
    return [([], t'', e', c')]

stepConcurrent (IHPut a n) (s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask
    writeChan (getBroadcastChan env) (BHPut ta n)
    return [(s, t, e, c)]
    
stepConcurrent (IHCase b arr) (s, t, e, []) = do
    let tb = fromJust (lookupLocalChanIDToGlobalChanID b t)
    env <- ask
    writeChan (getBroadcastChan env) (BHCase tb arr)
    predNumProcesses env
    return [] 

stepConcurrent (IRace rcs) (s, t, e, []) = do
    env <- ask
    mapM_ (f env) rcs
    predNumProcesses env
    return []
  where
    f env (lch, cs) = do
        let lch' = fromJust (lookupLocalChanIDToGlobalChanID lch t)
        writeChan (getBroadcastChan env) (BRace lch' (delete lch (map fst rcs), (s,t,e,cs)))

