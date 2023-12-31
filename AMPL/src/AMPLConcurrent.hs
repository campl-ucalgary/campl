{-# LANGUAGE TupleSections #-}
    -- This allows us to use syntax like
    -- (,b) to denote (\a -> (a,b))
{-# LANGUAGE PatternSynonyms #-}
    -- this is needed to import the 
    -- pattern synonyms from Data.Queue and ServiceConstants
module AMPLConcurrent where

import AMPLTypes
import AMPLEnv
import AMPLServices

import ServiceConstants

import Data.Queue 
    ( Queue
    , pattern (:<||)
    , pattern (:<|)
    , pattern Empty
    )
import qualified Data.Queue as Queue
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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
    A first draft of the Concurrent machine. 
    Contains functions to do exactly ONE concurrent step for a process
    and the channel manager.
-} 

-- | Options of a stepConcurrent result...
data ProcessConcurrentStepResult = 
    ProcessEnd                      -- ^ process has ended
    | ProcessContinue Stec          -- ^ process will continue running
    | ProcessDiverge Stec Stec      -- ^ process splits into two processes (e.g. from plug)

-- | steps a process by the focused concurrent instruction...
stepConcurrent :: 
    ( HasBroadcastChan r 
    , HasSuperCombinators r 
    , HasChannelNameGenerator r) =>
    -- | concurrent instruction to execute
    ConcurrentInstr ->                                      
        -- | (s, t, e, c) of the focused process...
    Stec ->               
        -- | result of the concurrent step.. Either the process ends,
        -- the process continues, or the process diverges..
        -- Note we need IO here because we are modifying the broadcast 
        -- channel, and need unique names...
    ReaderT r IO ProcessConcurrentStepResult
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

-- Technically the s must be empty from Table 2, but we leave it open for non empty stacks
stepConcurrent (ISplit a (a1, a2)) (s, t, e, c) = do
    let ta@(pol, _) = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask ; b1 <- getNewChannelName env ; b2 <- getNewChannelName env
    writeBroadcastChan env (BNewGlobalChannels [b1,b2])
    writeBroadcastChan env (BSplit ta (b1, b2))
    return $ ProcessContinue (s, (pol, (a1, b1)):(pol, (a2, b2)):t, e, c)
    -- new translations should have the same polarity as the orignal translation

stepConcurrent (IClose a) (s, t, e, c) = do 
    let ta = fromJust (lookupLocalChanIDToGlobalChanID a t)
    env <- ask 
    writeBroadcastChan env (BClose ta)
    return $ ProcessContinue (s, deleteTranslation a t, e, c)

stepConcurrent (IFork a ((a1, g1, c1), (a2, g2, c2))) (s, t, e, []) = do
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
-- He checks to see if we are identifying service channels
-- and throws an error otherwise... (we do not do that here!)
stepConcurrent (IId a b) (s, t, e, []) = do
    let (pa, ta) = fromJust (lookupLocalChanIDToGlobalChanID a t)
        (pb, tb) = fromJust (lookupLocalChanIDToGlobalChanID b t)
    env <- ask
    writeBroadcastChan env (BId (pa, ta) (ta, tb))
    -- writeBroadcastChan env (BId (pa, ta) (ta, tb))
    return ProcessEnd

stepConcurrent (IPlug as ((g1, c1), (g2, c2))) (s, t, e, []) = do
    env <- ask
    as' <- mapM (const (getNewChannelName env)) as
    -- by convention (i.e., Prashant), the (g1,c1) is the output channel
    -- and (g2, c2) is the input channel
    let t1 = zipWith (curry (Output,)) as as' ++ restrictTranslation g1 t
        t2 = zipWith (curry (Input,)) as as' ++ restrictTranslation g2 t
    writeBroadcastChan env (BNewGlobalChannels as')
    writeBroadcastChan env (BPlug as')
    return $ ProcessDiverge (s, t1, e, c1) (s, t2, e, c2)

stepConcurrent (IHalt as) (s, t, e, []) = do
    let as' = map (fromJust . flip lookupLocalChanIDToGlobalChanID t) as
    env <- ask
    writeBroadcastChan env (BHalt as')
    return ProcessEnd

-- c should be empty
stepConcurrent (IRun t' fid args) (s, t, e, []) = do
    env <- ask
    let e' = genericTake args s
        s' = genericDrop args s
            -- unused...
        t'' = fromJust (composeTranslationWithLocalTranslationMapping t t')
            -- note that Prashant writes `composeTranslation t' t`
            -- but our composeTranslation function differs in the sense
            -- that `composeTranslation f g` is the translation f after 
            -- the translation g, whereas his is the translation g after 
            -- the translation f.
        c' = superCombInstrLookup env fid
    return $ ProcessContinue ([], t'', e', c')
    -- this step is similar to Prashant's code.
    

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
stepConcurrent a b = error ("Bad stepConcurrent with: " ++ show a ++ " and " ++ show b)

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

-- | Given a broadcast channel command, and a channel manager 
-- this returns a new channel manger with the appropriate 
-- modifcations of the broadcast commands
addCommandToChannelManager :: 
    -- | Broadcast channel instruction
    BInstr ->       
    -- | old channel manager
    ChannelManager ->          
    -- | new channel manager
    ChannelManager             
addCommandToChannelManager (BGet (pol, gch) n) chm = 
    chm { channelManager = Map.adjust (appendQInstr pol (QGet n)) gch $ channelManager chm }

addCommandToChannelManager (BPut (pol, gch) n) chm =
    chm { channelManager = Map.adjust (appendQInstr pol (QPut n)) gch $ channelManager chm  }

addCommandToChannelManager (BSplit (pol, gch) n@(b1, b2)) chm =
    chm { channelManager = channelmanager }
  where
    channelmanager = Map.insert b1 emptyQInstrQueues 
            $ Map.insert b2 emptyQInstrQueues 
            $ Map.adjust (appendQInstr pol (QSplit n)) gch $ channelManager chm

addCommandToChannelManager (BFork (pol, gch) n) chm = 
    chm { channelManager = Map.adjust (appendQInstr pol (QFork n)) gch $ channelManager chm }

addCommandToChannelManager (BId (pol, gch) n) chm = 
    chm { channelManager = Map.adjust (appendQInstr pol (QId n)) gch $ channelManager chm }

addCommandToChannelManager (BClose (pol, gch)) chm = 
    chm { channelManager = Map.adjust (appendQInstr pol QClose) gch $ channelManager chm }

addCommandToChannelManager (BHalt ns) chm = 
  chm { channelManager = channelmanager }
  where
    channelmanager = foldr 
                        (\(pol, gch) acc -> Map.adjust (appendQInstr pol QClose) gch acc) 
                        (channelManager chm)
                        ns

addCommandToChannelManager (BPlug ns) chm =
    chm { channelManager = channelmanager }
  where
    channelmanager = foldr (`Map.insert` emptyQInstrQueues) (channelManager chm) ns

addCommandToChannelManager (BHPut (pol, gch) n) chm = 
    chm { channelManager = Map.adjust (appendQInstr pol (QHPut n)) gch $ channelManager chm }

addCommandToChannelManager (BHCase (pol, gch) n) chm = 
    chm { channelManager = Map.adjust (appendQInstr pol (QHCase n)) gch $ channelManager chm }

addCommandToChannelManager (BRace ds (s, t, e)) chm = 
    chm { channelManager = channelmanager }
  where
    channelmanager = foldr f (channelManager chm) ds

    f :: (Polarity, GlobalChanID, [LocalChanID], [Instr]) -> 
        Map GlobalChanID (Queue QInstr, Queue QInstr) ->
        Map GlobalChanID (Queue QInstr, Queue QInstr) 
    f (pol, gch, lchs, cs) = 
        Map.adjust (prependQInstr pol (QRace (lchs, (s, t, e, cs)))) gch 

addCommandToChannelManager (BNewGlobalChannels ngchs) chm = 
    chm { channelManagerTranslations = foldl f (channelManagerTranslations chm) ngchs }
  where
    f chmts gch = Map.insert gch gch chmts

-- | Result of one step of the channel manager
data StepChannelManagerResult = StepChannelManagerResult {
        chmNewProcesses :: [Stec]                   -- ^ new processes could spawn
        , chmFreeChannelNames :: [GlobalChanID]     -- ^ we could have some new free channel names
        , chmNewServices :: [ServiceQuery]          -- ^ New services could be spawned as well
        , chmIdentifications :: [(GlobalChanID, GlobalChanID)]          
            -- ^ When we identify a to b, written as (a,b), we REMOVE the queue
            -- a and defer all things on a to be put on b.
    } deriving (Show, Eq)

-- | An empty StepChannelManagerResult.
emptyStepChannelManagerResult :: StepChannelManagerResult
emptyStepChannelManagerResult = StepChannelManagerResult [] [] [] []


-- | Steps the channel manager (corresponding to Table 3)
stepChannelManager :: 
    -- | services
    Set GlobalChanID -> 
    -- | channel manager map
    Chm ->
    -- | new processes, and new map
    (StepChannelManagerResult, Chm)   
stepChannelManager services = 
    second (Map.fromAscList . fst)
                        -- get all the proccesed channels, and since they are in ascending order,
                        -- the precondition of Map.fromAscList is satisified so we may use it to 
                        -- create a map.

    . fix f             -- process all the channels i.e., we will have (processes to spawn, (processed channels, []))
                        -- note that due to the nature of zippers, the processed channels list will be reversed 
                        -- (i.e., in ascending order now since we started in descending order) so the precondition 
                        -- of Map.fromAscList list above will be satisfied.

    . (emptyStepChannelManagerResult,)     -- list for the processes to be spawned, globalchannel ids that can be returned, and
                                            -- services to spawn. i.e.  (emptyStepChannelManagerResult, (channels already procssed, channels to process))

    . ([],)             -- creates a zipper (channels already processed, channels to process)
                        -- no channel has been processed to begin with (hence the empty list)

    . Map.toDescList    -- converts the map into a descending list...
  where
    {- 
        Don't be afraid of this scary type signature! The first argument is the recursion (as usual with
        fixed point operators) and second argument is ((processes to be spawned, names to return, Services ), (channels already procssed, channels to process))
    -}
    f :: 
        ((StepChannelManagerResult, ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))]))
           -> (StepChannelManagerResult, ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))]))) -> 
        (StepChannelManagerResult, ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))])) -> 
        (StepChannelManagerResult, ([(GlobalChanID, (Queue QInstr, Queue QInstr))], [(GlobalChanID, (Queue QInstr, Queue QInstr))]))
    f rec (res, (chs', [])) = (res, (chs', []))

    {- 
        Notes about notation: 
          - :<|| means to pattern match against the front of a tuple of queues.
            e.g. given (q1:q1s, q2:q2s), we can do (q1,q2) :<|| (q1s,q2s); but given (Empty, q2) pattern matching will fail.

          - :<| means to pattern match against the front of a single queue
            e.g. given (q1:q1s), we can do (q1 :<| q1s); but given Empty pattern matching will fail.
    -}
    f rec (res, (chs', (gch, qs) : chs)) = case qs of
            (QPut v, QGet (s,t,e,c)) :<|| qs' 
                -> rec ( res { chmNewProcesses = (v:s,t,e,c) : chmNewProcesses res }
                    , ((gch, qs'):chs', chs))
            (QGet (s,t,e,c), QPut v) :<|| qs' 
                -> rec ( res { chmNewProcesses = (v:s,t,e,c) : chmNewProcesses res }
                    , ((gch, qs'):chs', chs))

            (QHPut i, QHCase (s,t,e,arr)) :<|| qs'
                -> rec (res { chmNewProcesses = (s,t,e,arr ! i) : chmNewProcesses res }
                    , ((gch, qs'):chs', chs))
            (QHCase (s,t,e,arr), QHPut i) :<|| qs'
                -> rec (res { chmNewProcesses = (s,t,e,arr ! i) : chmNewProcesses res }
                    , ((gch, qs'):chs', chs))

            -- From Prashant, if QSplit is on the left, and QFork is on 
            -- the right then they are both Input. Otherwise, they are both
            -- Output
            (QSplit (b1, b2), QFork (t, e, (a1, g1, c1), (a2, g2, c2))) :<|| qs'
                -> rec ( res { chmNewProcesses = [([], (Input, (a1,b1)):t, e, c1), ([], (Input, (a2,b2)):t, e, c2)] ++ chmNewProcesses res } 
                    , ((gch, qs'):chs', chs))
            (QFork (t, e, (a1, g1, c1), (a2, g2, c2)), QSplit (b1, b2)) :<|| qs'
                -> rec ( res { chmNewProcesses = [([], (Output, (a1,b1)):t, e, c1),([], (Output, (a2,b2)):t, e, c2)] ++ chmNewProcesses res }
                    , ((gch, qs'):chs', chs))

            (QClose, QHalt) :<|| qs'
                -> rec ( res { chmFreeChannelNames = gch : chmFreeChannelNames res }
                    , (chs', chs))
            (QHalt, QClose) :<|| qs'
                -> rec ( res { chmFreeChannelNames = gch : chmFreeChannelNames res }
                    , (chs', chs))
            -- Technically, the (close, close) and (halt, halt) commands are not in the 
            -- table, but we borrow this from Prashant 
            (QClose, QClose) :<|| qs'
                -> rec ( res { chmFreeChannelNames = gch : chmFreeChannelNames res }
                    , (chs', chs))
            (QHalt, QHalt) :<|| qs'
                -> rec ( res { chmFreeChannelNames = gch : chmFreeChannelNames res }
                    , (chs', chs))


            -- invariant: b == gch 
            (q1, q2@(QId (b, a) :<| Queue.Empty))
                | b > a -> 
                    -- a must be in chs and not in chs' 
                    -- since this is in descending order
                    let (q2', nchs) = first (fromMaybe q2)
                                        $ foldr g (Nothing, []) chs
                        isIdExecuted = q2 /= q2'
                    in rec ( res { chmFreeChannelNames = chmFreeChannelNames res
                                 , chmIdentifications = [(a, b) | isIdExecuted] ++ chmIdentifications res }
                        , ( (gch, (q1,q2')) : chs', nchs))
                | otherwise ->  
                    let (q2', nchs') = first (fromMaybe q2)
                                        $ foldr g (Nothing, []) chs'
                        isIdExecuted = q2 /= q2'
                    in rec ( res { chmFreeChannelNames = chmFreeChannelNames res 
                                 , chmIdentifications = [(a, b) | isIdExecuted] ++ chmIdentifications res }
                        , ( (gch, (q1,q2')) : nchs', chs))
                -- What about self identifying channels? ie a == b
              where
                g chq1q2 (Just desiredq2, rest) = (Just desiredq2, chq1q2 : rest)
                g chq1q2@(focused, (focusedq1, focusedq2)) (Nothing, rest) 
                    | focused == a && Queue.isEmpty focusedq1  = (Just focusedq2, rest)
                    | otherwise = (Nothing, chq1q2:rest)

            -- invariant: b == gch 
            (q1@(QId (b, a) :<| Queue.Empty), q2)
                | b > a -> 
                    -- a must be in chs and not in chs' 
                    -- since this is in descending order
                    let (q1', nchs) = first (fromMaybe q1)
                                        $ foldr g (Nothing, []) chs
                        isIdExecuted = q1 /= q1'
                    in rec ( res { chmFreeChannelNames = chmFreeChannelNames res
                                 , chmIdentifications = [(a, b) | isIdExecuted] ++ chmIdentifications res }
                        , ( (gch, (q1',q2)) : chs', nchs))
                | otherwise ->  
                    let (q1', nchs') = first (fromMaybe q1)
                                        $ foldr g (Nothing, []) chs'
                        isIdExecuted = q1 /= q1'
                    in rec ( res { chmFreeChannelNames = chmFreeChannelNames res
                                 , chmIdentifications = [(a, b) | isIdExecuted] ++ chmIdentifications res }
                        , ( (gch, (q1',q2)) : nchs', chs))
                -- What about self identifying channels? ie a == b
              where
                g chq1q2 (Just desiredq2, rest) = (Just desiredq2, chq1q2 : rest)
                g chq1q2@(focused, (focusedq1, focusedq2)) (Nothing, rest) 
                    | focused == a && Queue.isEmpty focusedq2 = (Just focusedq1, rest)
                    | otherwise = (Nothing, chq1q2:rest)


            (q1@(QPut v :<| _), QRace (rcs, (s,t,e,c)) :<| q2) -> 
                let grcs = map snd (fromJust (mapM (`lookupLocalChanIDToGlobalChanID` t) rcs))
                    rmvrace (gch', (q1', q2')) 
                        | gch' `elem` grcs 
                            = ((gch',) . (q1',))
                                (maybe 
                                q2' (\(q2'', hq) -> case hq of QRace _ -> q2'' ; _ -> q2') 
                                (Queue.head q2'))
                        | otherwise = (gch', (q1', q2'))
                in rec ( res { chmNewProcesses = (s,t,e,c) : chmNewProcesses res}
                    , ((gch,(q1,q2)) : map rmvrace chs', map rmvrace chs))
            (QRace (rcs, (s,t,e,c)) :<| q1, q2@(QPut v :<| _ ))  -> 
                let grcs = map snd (fromJust (mapM (`lookupLocalChanIDToGlobalChanID` t) rcs))
                    rmvrace (gch', (q1', q2')) 
                        | gch' `elem` grcs 
                            = ((gch',) . (,q2'))
                                (maybe 
                                q1' (\(q1'', hq) -> case hq of QRace _ -> q1'' ; _ -> q1') 
                                (Queue.head q1'))
                        | otherwise = (gch', (q1', q2'))
                in rec ( res { chmNewProcesses = (s,t,e,c) : chmNewProcesses res }
                        , ((gch,(q1, q2)) : map rmvrace chs', map rmvrace chs))

            {- opening services... Recall from Prashant that 
                - 1 then it corresponds to get (i.e., enter a number in the terminal)
                    (we write this as IxGet with PatternSynonyms and ViewPatterns)
                - 2 then it corresponds to put (i.e., enter a number on the terminal)
                    (we write this as IxPut with PatternSynonyms and ViewPatterns)
                - 3 then it corresponds to close (i.e., close the service)
                    (we write this as IxClose with PatternSynonyms and ViewPatterns)

                -- although, in the actual system, we go from 0,1,2 
            -}
            (Queue.Empty, QHPut (HCaseIx IxGet) :<| q2@(QGet (s,t,e,c) :<| _)) 
                | gch `Set.member` services -> rec
                (res { chmNewServices = (gch, ServiceGet Output) : chmNewServices res } 
                , ((gch, (Queue.empty, q2)):chs' 
                , chs) )

            (QHPut (HCaseIx IxGet) :<| q1@(QGet (s,t,e,c) :<| _), Queue.Empty) 
                | gch `Set.member` services -> rec
                (res { chmNewServices = (gch, ServiceGet Input) : chmNewServices res } 
                , ((gch, (q1, Queue.empty)):chs' 
                , chs) )

            (Queue.Empty, QHPut (HCaseIx IxPut) :<| (QPut v :<| q2)) 
                | gch `Set.member` services -> rec
                (res { chmNewServices = (gch, ServicePut v) : chmNewServices res } 
                , ((gch, (Queue.empty, q2)):chs' 
                , chs) )
            (QHPut (HCaseIx IxPut) :<| (QPut v :<| q1), Queue.Empty) 
                | gch `Set.member` services -> rec
                (res { chmNewServices = (gch, ServicePut v) : chmNewServices res } 
                , ((gch, (q1, Queue.empty)):chs' 
                , chs) )

            (Queue.Empty, QHPut (HCaseIx IxClose) :<| q2) 
                | gch `Set.member` services -> rec
                (res { chmNewServices = (gch, ServiceClose) : chmNewServices res } 
                , ((gch, (Queue.empty, q2)):chs' 
                , chs) )
            (QHPut (HCaseIx IxClose) :<| q1, Queue.Empty) 
                | gch `Set.member` services -> rec
                (res { chmNewServices = (gch, ServiceClose) : chmNewServices res } 
                , ((gch, (q1, Queue.empty)):chs' 
                , chs) )

            -- fall back catch all case.
            qs -> rec (res, ((gch, qs):chs', chs))

