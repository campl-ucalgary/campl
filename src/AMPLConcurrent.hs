{-# LANGUAGE TupleSections #-}
module AMPLConcurrent where

import AMPLTypes
import AMPLEnv
import Control.MonadChan
import Control.MonadIORef
import Data.Queue (Queue)
import qualified Data.Queue as Queue
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow
import Data.Maybe
import Data.Array
import Data.Functor
import Data.List
import Data.Coerce
import Data.Function
import Data.Semigroup
import Data.Tuple 

{- 
    A first draft of the Concurrent machine
-} 

-- | Options of a concurrent list result
data ConcurrentStepResult = 
    ProcessEnd
    | ProcessContinue Stec
    | ProcessDiverge Stec Stec

-- | steps a process by the focused concurrent instruction...
-- If an asynchronous exception is thrown to this thread, it will
-- put the whole system in an undefined state -- ensure that 
-- mask is used to ensure that this does not happen..
stepConcurrent :: 
    ( MonadReader r m
    , MonadChan m 
    , MonadAtomicIORef m
    , HasBroadcastChan r 
    , HasSuperCombinators r 
    , HasChannelNameGenerator r) =>
    ConcurrentInstr ->                                      
        -- ^ concurrent instruction to execute
    Stec ->               
        -- ^ (s, t, e, c) of focused process...
    m ConcurrentStepResult
        -- ^ Either a list of 0, 1, or 2 elements..
        -- 0 implies the process is suspended..
        -- 1 implies the process is continuing..
        -- 2 implies the process has been plugged
stepConcurrent (IGet a) pr@(s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask 
    writeBroadcastChan env (BGet ta pr)
    return ProcessEnd

stepConcurrent (IPut a) pr@(v:s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask 
    writeBroadcastChan env (BPut ta v)
    return $ ProcessContinue (s, t, e, c)

-- Technically the s must be empty from Table 2..
stepConcurrent (ISplit a (a1, a2)) (s, t, e, c) = do
    let ta@(pol, _) = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask ; b1 <- getNewChannelName env ; b2 <- getNewChannelName env
    writeBroadcastChan env (BSplit ta (b1, b2))
    return $ ProcessContinue (s, (pol, (a1, b1)):(pol, (a2, b2)):t, e, c)
    -- new translations should have the same polarity as the orignal translation

stepConcurrent (IClose a) (s, t, e, c) = do 
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask 
    writeBroadcastChan env (BClose ta)
    return $ ProcessContinue (s, deleteTranslation a t, e, c)

stepConcurrent (IFork a ((a1, g1, c1), (a2, g2, c2))) (s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask
    writeBroadcastChan env 
        (BFork ta (t, e, (a1, restrictTranslation g1 t, c1), (a2, restrictTranslation g2 t, c2)))
        -- remark: we could do the restriction in the channel manager,
        -- but since the channel manager will probably be very busy managing
        -- channels for other processes, we make each process do it itself..
    return ProcessEnd

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
    env <- ask
    if pa == pb 
        then do
            writeBroadcastChan env (BId (pa, ta) (ta, tb))
            return ProcessEnd
        else error "invalid IId instruction"

-- s should be empty here too
stepConcurrent (IPlug as ((g1, c1), (g2, c2))) (s, t, e, c) = do
    env <- ask
    as' <- mapM (const (getNewChannelName env)) as
    -- by convention (i.e., Prashant), the (g1,c1) is the output channel
    -- and (g2, c2) is the input channel
    let t1 = zipWith (curry (Output,)) as as' ++ restrictTranslation g1 t
        t2 = zipWith (curry (Input,)) as as' ++ restrictTranslation g2 t
    writeBroadcastChan env (BPlug as')
    return $ ProcessDiverge (s, t1, e, c1) (s, t2, e, c2)

-- c should be empty here too
stepConcurrent (IHalt as) (s, t, e, c) = do
    let as' = map (fromJust . flip lookupLocalChanIDToGlobalChanID t) as
    env <- ask
    writeBroadcastChan env (BHalt as')
    return ProcessEnd

-- c should be empty
stepConcurrent (IRun t' fid args) (s, t, e, c) = do
    env <- ask
    let e' = genericTake args s
        s' = genericDrop args s
        t'' = fromJust (composeTranslation t' t)
        c' = superCombInstrLookup env fid
    return $ ProcessContinue ([], t'', e', c')

stepConcurrent (IHPut a n) (s, t, e, c) = do
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask
    writeBroadcastChan env (BHPut ta n)
    return $ ProcessContinue (s, t, e, c)
    
stepConcurrent (IHCase b arr) (s, t, e, []) = do
    let tb = fromJust (lookupLocalChanIDToGlobalChanID b t)
    env <- ask
    writeBroadcastChan env (BHCase tb (s,t,e,arr))
    return ProcessEnd

stepConcurrent (IRace rcs) (s, t, e, []) = do
    env <- ask
    writeBroadcastChan env (BRace (map f rcs) (s,t,e))
    return ProcessEnd
  where
    f :: (LocalChanID, [Instr]) -> (Polarity, GlobalChanID, [LocalChanID], [Instr])
    f (lch, cs) = 
        let (pol, gch) = fromJust (lookupLocalChanIDToGlobalChanID lch t)
        in (pol, gch, delete lch (map fst rcs), cs)

-- | Puts the QInstr to the back of the queue
-- corresponding to the polarity. Recall by convention
-- (i.e., Prashant) the output queue is to the left
-- and the input queue is to the right.
appendQInstr :: 
    Polarity ->
    QInstr ->
    (Queue QInstr, Queue QInstr) ->
    (Queue QInstr, Queue QInstr)
appendQInstr Output qinstr = first (`Queue.append` qinstr)
appendQInstr Input qinstr = second (`Queue.append` qinstr)

-- | Puts the QInstr to the front of the queue
-- corresponding to the polarity. Recall by convention
-- (i.e., Prashant) the output queue is to the left
-- and the input queue is to the right.
prependQInstr :: 
    Polarity ->
    QInstr ->
    (Queue QInstr, Queue QInstr) ->
    (Queue QInstr, Queue QInstr)
prependQInstr Output qinstr = first (qinstr `Queue.prepend`)
prependQInstr Input qinstr = second (qinstr `Queue.prepend`)

-- | Gets the head instructions (if they exist..) and 
-- the moodified queue from the head instruction...
headQInstrs :: 
    (Queue QInstr, Queue QInstr) ->
    ((Queue QInstr, Queue QInstr), (Maybe QInstr, Maybe QInstr))
headQInstrs (q1, q2) = ((q1', q2'), (mqinstr1, mqinstr2))
  where
    (q1', mqinstr1) = f q1
    (q2', mqinstr2) = f q2

    f q = 
        case Queue.head q of
            Nothing -> (q, Nothing)
            Just (q', v) -> (q', Just v)

emptyQInstrQueues :: (Queue QInstr, Queue QInstr)
emptyQInstrQueues = (Queue.empty, Queue.empty)

-- | Given a broadcast channel command, and a channel manager 
-- (recall the type is: Map GlobalChanID (Queue, Queue)),
-- this returns a new channel manger with the appropriate 
-- modifcations of the broadcast command
addCommandToChannelManager :: 
    BInstr ->
    Map GlobalChanID (Queue QInstr, Queue QInstr) ->
    Map GlobalChanID (Queue QInstr, Queue QInstr) 

addCommandToChannelManager (BGet (pol, gch) n) chm = 
    Map.adjust (appendQInstr pol (QGet n)) gch chm

addCommandToChannelManager (BPut (pol, gch) n) chm =
    Map.adjust (appendQInstr pol (QPut n)) gch chm 

addCommandToChannelManager (BSplit (pol, gch) n@(b1, b2)) chm =
    Map.insert b1 emptyQInstrQueues 
    $ Map.insert b2 emptyQInstrQueues 
    $ Map.adjust (appendQInstr pol (QSplit n)) gch chm

addCommandToChannelManager (BFork (pol, gch) n) chm = 
    Map.adjust (appendQInstr pol (QFork n)) gch chm

addCommandToChannelManager (BId (pol, gch) n) chm = 
    Map.adjust (appendQInstr pol (QId n)) gch chm

addCommandToChannelManager (BClose (pol, gch)) chm = 
    Map.adjust (appendQInstr pol QClose) gch chm

addCommandToChannelManager (BHalt ns) chm = 
    foldr 
        (\(pol, gch) acc -> Map.adjust (appendQInstr pol QClose) gch acc) 
        chm 
        ns

addCommandToChannelManager (BPlug ns) chm =
    foldr (`Map.insert` emptyQInstrQueues) chm ns

addCommandToChannelManager (BHPut (pol, gch) n) chm = 
    Map.adjust (appendQInstr pol (QHPut n)) gch chm

addCommandToChannelManager (BHCase (pol, gch) n) chm = 
    Map.adjust (appendQInstr pol (QHCase n)) gch chm

addCommandToChannelManager (BRace ds (s, t, e)) chm = 
    foldr f chm ds
  where
    f :: (Polarity, GlobalChanID, [LocalChanID], [Instr]) -> 
        Map GlobalChanID (Queue QInstr, Queue QInstr) ->
        Map GlobalChanID (Queue QInstr, Queue QInstr) 
    f (pol, gch, lchs, cs) = 
        Map.adjust (prependQInstr pol (QRace (lchs, (s, t, e, cs)))) gch 

-- | Steps the channel manager (corresponding to Table 3)
stepChannelManager :: 
    Map GlobalChanID (Queue QInstr, Queue QInstr) ->
    ([Stec] , Map GlobalChanID (Queue QInstr, Queue QInstr))   -- ^ new processes, and new map
stepChannelManager = 
    second (Map.fromAscList . fst)
                        -- get all the proccesed channels, and since they are in ascending order,
                        -- the precondition of Map.fromAscList is satisified so we may use it to 
                        -- create a map.

    . fix f             -- process all the channels i.e., we will have (processes to spawn, (processed channels, []))
                        -- note that due to the nature of zippers, the processed channels list will be reversed 
                        -- (i.e., in ascending order now since we started in descending order) so the precondition 
                        -- of Map.fromAscList list above will be satisfied.

    . ([],)             -- list for the processes to be spawned i.e. 
                        -- (processes to be spawned, (channels already procssed, channels to process))
                        -- when starting to process the channels, not processes are to be spawned yet...

    . ([],)             -- creates a zipper (channels already processed, channels to process)
                        -- no channel has been processed to begin with (hence the empty list)

    . Map.toDescList    -- converts the map into a descending list...
  where
    -- Don't be intimidated by this type signature! The first argument is the recursion (as it usually 
    -- is with the fix operations), the second argument is the zipper of channels to process with the 
    -- suspended processes to run
     f :: (([Stec], ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))])) 
            -> ([Stec], ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))]))) -> 
            ([Stec], ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))])) -> 
            ([Stec], ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))]))
     f rec (ps, (chs', [])) = (ps, (chs', []))
     f rec (ps, (chs', (gch, qs@(q1,q2)):chs)) 
        = case headQInstrs qs of
            (qs', (Just (QPut v), Just (QGet (s,t,e,c))))
                -> rec ((v:s,t,e,c):ps, ((gch, qs'):chs', chs))
            (qs', (Just (QGet (s,t,e,c)), Just (QPut v)))
                -> rec ((v:s,t,e,c):ps, ((gch, qs'):chs', chs))

            (qs', (Just (QHPut i), Just (QHCase (s,t,e,arr))))
                -> rec ((s,t,e,arr ! i):ps, ((gch, qs'):chs', chs))
            (qs', (Just (QHCase (s,t,e,arr)), Just (QHPut i)))
                -> rec ((s,t,e,arr ! i):ps, ((gch, qs'):chs', chs))

            -- From Prashant, if QSplit is on the left, and QFork is on 
            -- the right then they are both Input. Otherwise, they are both
            -- Output
            (qs', (Just (QSplit (b1, b2)), Just (QFork (t, e, (a1, g1, c1), (a2, g2, c2)))))
                -> rec (([], (Input, (a1,b1)):t, e, c1):([], (Input, (a2,b2)):t, e, c2):ps, ((gch, qs'):chs', chs))
            (qs', (Just (QFork (t, e, (a1, g1, c1), (a2, g2, c2))), Just (QSplit (b1, b2))))
                -> rec (([], (Output, (a1,b1)):t, e, c1):([], (Output, (a2,b2)):t, e, c2):ps, ((gch, qs'):chs', chs))

            (qs', (Just QClose, Just QHalt))
                -> rec (ps, (chs', chs))
            (qs', (Just QHalt, Just QClose))
                -> rec (ps, (chs', chs))

            (qs', (_, Just (QId (b, a))))
                | b < a -> 
                    -- a must be further ahead, so merge b with a
                    let nchs = map (\(a', (q, q')) -> if a' == a && Queue.isEmpty q then (a', (q1, q')) else (a', (q, q'))) chs
                    in rec (ps, ([(gch, qs) | nchs == chs], const nchs) <*> (chs', chs))
                    -- b must be behind a...
                | otherwise -> 
                    let nchs' = map (\(a', (q, q')) -> if a' == a && Queue.isEmpty q then (a', (q1, q')) else (a', (q, q'))) chs'
                    in rec (ps, swap (([(gch, qs) | nchs' == chs'], const nchs') <*> (chs, chs')))
            (qs', (Just (QId (b, a)), _))
                | b < a -> 
                    -- a must be further ahead, so merge b with a
                    let nchs = map (\(a', (q, q')) -> if a' == a && Queue.isEmpty q' then (a', (q, q2)) else (a', (q, q'))) chs
                    in rec (ps, ([(gch, qs) | nchs == chs], const nchs) <*> (chs', chs))
                    -- b must be behind a...
                | otherwise -> 
                    let nchs' = map (\(a', (q, q')) -> if a' == a && Queue.isEmpty q' then (a', (q, q2)) else (a', (q, q'))) chs'
                    in rec (ps, swap (([(gch, qs) | nchs' == chs'], const nchs') <*> (chs, chs')))

            (qs', (Just (QPut v), Just (QRace (rcs, (s,t,e,c))))) -> 
                let grcs = map snd (fromJust (mapM (`lookupLocalChanIDToGlobalChanID` t) rcs))
                    rmvrace (gch', (q1', q2')) 
                        | gch' `elem` grcs 
                            = ((gch',) . (q1',))
                                (maybe 
                                q2' (\(q2'', hq) -> case hq of QRace _ -> q2'' ; _ -> q2') 
                                (Queue.head q2'))
                        | otherwise = (gch', (q1', q2'))
                in rec (ps, ((gch,qs) : map rmvrace chs', map rmvrace chs))
            (qs', (Just (QRace (rcs, (s,t,e,c))), Just (QPut v))) -> 
                let grcs = map snd (fromJust (mapM (`lookupLocalChanIDToGlobalChanID` t) rcs))
                    rmvrace (gch', (q1', q2')) 
                        | gch' `elem` grcs 
                            = ((gch',) . (,q2'))
                                (maybe 
                                q1' (\(q1'', hq) -> case hq of QRace _ -> q1'' ; _ -> q1') 
                                (Queue.head q1'))
                        | otherwise = (gch', (q1', q2'))
                in rec (ps, ((gch,qs) : map rmvrace chs', map rmvrace chs))

            (qs', _) -> rec (ps, ((gch, qs):chs', chs))
            
