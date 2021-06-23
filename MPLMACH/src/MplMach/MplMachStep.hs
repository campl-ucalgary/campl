{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module MplMach.MplMachStep where

import Optics
import Data.Coerce
import Control.Monad
import Control.Exception
import Data.Array (Array (..))
import qualified Data.Array as Arr
import Control.Arrow
import Data.Foldable
import Data.Traversable
import Data.IORef
import Data.Maybe

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Sequence

import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map

import MplMach.MplMachTypes
import MplMach.MplMachStack
import MplMach.MplMachException

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.IO.Class

import Debug.Trace
import qualified Text.Show.Pretty as PrettyShow

import System.Process



{- | runs the MplMach from start to finish -}
mplMachSteps :: 
    Stec ->
    MplMach MplMachEnv ()
mplMachSteps = go . Just 
  where
    go = \case
        {-
        Just stec -> seqStep (concStep (liftIO . throwIO . ppShowIllegalStep)) stec 
            >>= go
        -}
        Just stec -> do
            {-
            mvar <- gview stdLock

            liftIO $ do
                takeMVar mvar
                PrettyShow.pPrint stec

                fkk <- liftIO 
                    $ sequenceOf (traversed % _2) $ stec ^. translation 
                    % to (Map.toList >>> map (second showTranslationLkup))
                PrettyShow.pPrint $ "Queues"
                PrettyShow.pPrint $ fkk

                putMVar mvar ()
            -}

            stec' <- seqStep (concStep (liftIO . throwIO . ppShowIllegalStep)) stec 

            go stec'
        Nothing -> return ()

{- | Executes a sequential step, and if all matches fails, exectutes the 
 - continutaiton. Returns Nothing if the machine has reached its final
 - execution state. -}
seqStep :: 
    ( HasMplMachSuperCombinators r ) =>
    -- | continuation with what to do next if all sequential pattern
    -- matches fails
    (Stec -> MplMach r (Maybe Stec)) ->
    -- | current machine
    Stec ->
    -- | result 
    MplMach r (Maybe Stec)
seqStep k stec = case steccode of
    {- (code, environemnt, stack) -}
    -- no code left, means we are done
    [] -> pure Nothing
    SeqInstr instr : c -> case (instr, stecenv, stecstack) of
        {- Store:c, e, v:s ---> c, v:e, s -}
        (IStore, e, v:s) -> pure $ Just $
            stec & code !~ c
                 & environment %!~ cons v
                 & stack !~ s
        {- Access(n):c, e, s ---> c, e, e[n]:s -}
        (IAccess n, e, s) -> pure $ Just $
            stec & code !~ c
                 & stack %!~ cons (e !! n)
        {- Call(c'):c, e, s --> c', e, clos(c,e):s -}
        (ICall cix, e, s) -> do
            c' <- gviews supercombinators $ (Arr.! cix)
            return $ Just $ 
                stec & code !~ c'
                    & stack %!~ cons (VClos c e)

        {- Ret:c, e, v:clos(c',e'):s ---> c', e', v:s -}
        (IRet, e , v : VClos c' e': s) -> pure $ Just $
            stec & code !~ c'
                 & environment !~ e'
                 & stack !~ cons v s

        {- Cons(i,n):c, e, v_1:...:v_n:s --> c,e,cons(i,[v_1,...,v_n]):s-}
        (ICons i n, e, s) -> pure $ Just $
            stec & code !~ c
                 & stack !~ s'
                 & stack %!~ cons (VCons i args) 
          where
            (args, s') = splitAt n s

        {- Case(c_1,...,c_n):c, e: Cons(i,[v_1,...,v_n]):s ---> c_i, v_1:...:v_n:e, clo(c,e):s) -}
        (ICase cases, e, VCons i vs : s) -> pure $ Just $
            stec & code !~ (cases Arr.! i)
                 & environment %!~ (vs++)
                 & stack %!~ cons (VClos c e) 

        {- Rec(c_1,...c_n):c, e, s ---> c, e, rec([c_1,..,c_n], e):s -}
        (IRec recs, e, s) -> pure $ Just $
            stec & code !~ c
                 & stack %!~ cons (VRec recs e) 

        {- Dest(i, n) : c, e , rec([c_1...c_n,e'):v_n:...:v_1:s ---> c_i, v_1...v_n:e', clo(c,e):s -}
        (IDest i n, e, VRec recs e' : s) -> pure $ Just $
            stec & code !~ (recs Arr.! i)
                 & environment !~ e'
                 & environment %!~ (reverse args ++)
                 & stack %!~ cons (VClos c e)
          where
            (args, s') = splitAt n s

        {- Const(v) :c, e, s --> c, e, v : s -}
        (IConst v, e, s) -> pure $ Just $
            stec & code !~ c
                 & stack %!~ cons v

        {- built in operations -}
        (IAddInt, e, VInt n : VInt m : s) -> pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ m + n)
        (IMulInt, e, VInt n : VInt m : s) -> pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ m * n)
        (IEqInt, e, VInt n : VInt m : s) -> pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ m == n)
        (ILeqInt, e, VInt n : VInt m : s) -> pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ m <= n)
        _ -> k stec
    _ -> k stec
  where
    steccode = stec ^. code
    stecenv = stec ^. environment
    stecstack = stec ^. stack

{- | executes a concurrent step in the machine -}
concStep ::
    -- | continuation with what to do next if all pattern matches fails
    (Stec -> MplMach MplMachEnv (Maybe Stec)) ->
    -- | current state to execute a step
    Stec ->
    MplMach MplMachEnv (Maybe Stec)
concStep k stec = gview equality >>= \fundefns -> let mplMachSteps' inpstec = runMplMach (mplMachSteps inpstec) fundefns in case steccode of
    {- (stack, translation, environment, code -}
    ConcInstr instr : c -> case (stecstack, stectranslation, stecenv, instr) of
        (s, t, e, IGet ch) -> do
            writeChMQueue (chlkup ^. activeQueue) (QGet (stec & code !~ c))

            -- the channel manager action
            stec' <- liftIO $ atomically $ do
                chactivequeue <- chlkup ^. activeQueue % to readChMQueue
                peekTQueue chactivequeue >>= \case
                    QGet gstec -> do
                        chotherqueue <- chlkup ^. otherQueue % to readChMQueue
                        peekTQueue chotherqueue >>= \case
                            QPut v -> do
                                _ <- readTQueue chactivequeue
                                _ <- readTQueue chotherqueue
                                return $ gstec & stack %!~ cons v
                            _ -> retry
                    _ -> retry
                {-
                readActiveQueueAndOtherQueue chlkup >>= \case
                    (QGet gstec, QPut v) -> return $ gstec & stack %!~ cons v
                    _ -> retry
                -}

            return $ Just $ stec'
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (v:s, t, e, IPut ch) -> do
            writeChMQueue (chlkup ^. activeQueue) (QPut v)
            return $ Just $ stec 
                & stack !~ s
                & code !~ c
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (_, t, e, ISplit ch (lch, rch)) -> do
            glch <- newGlobalChan
            grch <- newGlobalChan

            writeChMQueue (chlkup ^. activeQueue) (QSplit glch grch)

            return $ Just $ stec 
                -- first set the lechlkup channel
                & translation % at lch ?!~ setTranslationLkup chlkup glch
                -- then set the right channel
                & translation % at rch ?!~ setTranslationLkup chlkup grch
                -- and update the code
                & code !~ c
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (_, t, e, IFork ch ((ch0, chwiths0, instrs0), (ch1, chwiths1, instrs1))) -> do
            let stec0 = stec
                    & code !~ instrs0
                    & translation %!~ flip Map.restrictKeys chwiths0
                stec1 = stec
                    & code !~ instrs1
                    & translation %!~ flip Map.restrictKeys chwiths1
            writeChMQueue (chlkup ^. activeQueue) (QFork (ch0, stec0 ) (ch1, stec1))

            -- the channel manager action
            (stec0', stec1') <- liftIO $ atomically $ do
                {- Notes:
                    - the active queue must be QFork (since we just put QFork there)
                    - So, in a well typed program, the other queue should be a split 
                    - thus, in the new stec's we inherit the polarity of the fork channel
                -}
                chactivequeue <- chlkup ^. activeQueue % to readChMQueue
                peekTQueue chactivequeue >>= \case
                    QFork (flch0, fstec0) (flch1, fstec1) -> do
                        chotherqueue <- chlkup ^. otherQueue % to readChMQueue
                        peekTQueue chotherqueue >>= \case
                            QSplit sgch0 sgch1 -> do
                                _ <- readTQueue chactivequeue
                                _ <- readTQueue chotherqueue
                                return 
                                    ( fstec0 & translation % at flch0 ?!~
                                        setTranslationLkup chlkup sgch0
                                    , fstec1 & translation % at flch1 ?!~
                                        setTranslationLkup chlkup sgch1
                                    )
                            _ -> retry
                    _ -> retry
                {-
                readActiveQueueAndOtherQueue chlkup >>= \case
                    (QFork (flch0, fstec0) (flch1, fstec1), QSplit sgch0 sgch1) ->
                        return 
                            ( fstec0 & translation % at flch0 ?!~
                                setTranslationLkup chlkup sgch0
                            , fstec1 & translation % at flch1 ?!~
                                setTranslationLkup chlkup sgch1
                            )
                    _ -> retry
                -}
                
            liftIO $ concurrently_ 
                (mplMachSteps' stec0') 
                (mplMachSteps' stec1')
            
            return Nothing
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        -- actually there's probably a bug here: need to do some extra work (but when?)
        -- to run the channel manager when we have (close, close)
        (s, t, e, IClose ch) -> do
            -- writeChMQueue (chlkup ^. activeQueue) QClose
            return $ Just $ stec 
                & code !~ c
                & translation % at ch !~ Nothing
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (s, t, e, IId lch rch) -> do
            writeChMQueue (lchlkup ^. activeQueue) $ QId (translationLkupToGlobalChan rchlkup)

            -- the channel manager action
            liftIO $ atomically $ do
                lchmqueue <- readChMQueue (lchlkup ^. activeQueue)
                peekTQueue lchmqueue >>= \case
                    QId rgch -> readTQueue lchmqueue >> case lchlkup of
                        {- the idea is as follows.
                            Given:
                                (b, q | [id b = a]) , (a, [] | q')
                            Set:

                                * [] to point to q

                                * the pointer after removing the head of [id b = a]
                                    to point to q'
                            -}
                        InputLkup linqueue loutqueue -> do
                            -- wait for other queue to be empty of the right 
                            -- channel to be empty
                            ((isEmptyTQueue <=< readTVar) outrgch) >>= check

                            -- get the left channels output queue and set this
                            -- output queue to the right channels output queue (
                            -- which we have just checked that it is empty)
                            loutqueue' <- loutqueue 
                                ^. coerced @ChMQueue @(TVar (TQueue QInstr)) 
                                % to readTVar
                            writeTVar outrgch loutqueue'

                            -- get the left channels input queue, and set this
                            -- to the input queue of the right channels input queue
                            -- (which should be empty since id should be the last
                            -- command of any well typed program)
                            inrgch' <- readTVar inrgch 
                            writeTVar (coerce linqueue) inrgch' 
                          where
                            outrgch = rgch ^. coerced 
                                % chMOutputQueue 
                                % coerced @ChMQueue @(TVar (TQueue QInstr)) 
                            inrgch = rgch ^. coerced 
                                % chMInputQueue 
                                % coerced @ChMQueue @(TVar (TQueue QInstr)) 

                        {- the idea is as follows (compeltely similar to above.
                            Given:
                                (b, [id b = a] | q ) , (a, q'  | [])
                            Set:

                                * the pointer after removing the head of [id b = a]
                                    to point to q'

                                * [] to point to q

                            Mostly duplciated code from previous case:
                            -}
                        OutputLkup loutqueue linqueue -> do
                            ((isEmptyTQueue <=< readTVar) inrgch) >>= 
                                check

                            outrgch' <- readTVar outrgch
                            writeTVar (coerce loutqueue) outrgch'


                            linqueue' <- linqueue
                                ^. coerced @ChMQueue @(TVar (TQueue QInstr)) 
                                % to readTVar
                            writeTVar inrgch linqueue'


                            {-
                            linqueue' <- linqueue 
                                ^. coerced @ChMQueue @(TVar (TQueue QInstr)) 
                                % to readTVar
                            writeTVar inrgch linqueue'

                            outrgch' <- readTVar outrgch 
                            writeTVar (coerce loutqueue) outrgch' 
                            -}
                          where
                            inrgch = rgch ^. coerced 
                                % chMInputQueue 
                                % coerced @ChMQueue @(TVar (TQueue QInstr)) 
                            outrgch = rgch ^. coerced 
                                % chMOutputQueue 
                                % coerced @ChMQueue @(TVar (TQueue QInstr)) 

                    _ -> retry
            return Nothing

          where
            -- lchlkup = t Map.! lch
            Just lchlkup = Map.lookup lch t 
            -- rchlkup = t Map.! rch
            Just rchlkup = Map.lookup rch t 

        (s, t, e, IPlug chs ((chs0, instrs0), (chs1, instrs1))) -> do
            -- this is essentially the channel manager actions
            -- lchsgchs <- for chs $ \ch -> (\ch -> fmap (ch,) newGlobalChan) chs
            -- Okay this is kinda stupid 
            lchsgchs <- mplMachOpenChs chs

            let -- new translations for t0 (recall the first phrase should be 
                -- all output polarity)
                t0n = Map.fromList 
                        ( lchsgchs & mapped % _2 %~
                            \gch -> OutputLkup 
                                { _activeQueue = gch ^. coerced % chMOutputQueue
                                , _otherQueue = gch ^. coerced % chMInputQueue
                                }
                        )
                -- new translations for t1 (recall the second phrase should be 
                -- all input polarity)
                t1n = Map.fromList 
                        ( lchsgchs & mapped % _2 %~
                            \gch -> InputLkup 
                                { _activeQueue = gch ^. coerced % chMInputQueue 
                                , _otherQueue = gch ^. coerced % chMOutputQueue
                                }
                        )
                t0 = Map.restrictKeys t chs0 <> t0n
                t1 = Map.restrictKeys t chs1 <> t1n

                stec0 = stec 
                    & stack !~ []
                    & translation !~ t0
                    & code !~ instrs0

                stec1 = stec 
                    & stack !~ []
                    & translation !~ t1
                    & code !~ instrs1

            liftIO $ concurrently_ 
                (mplMachSteps' stec0) 
                (mplMachSteps' stec1) 

            return Nothing

        (_, t, e, IHalt ch) -> do
            -- writeChMQueue (chlkup ^. activeQueue) QHalt
            return Nothing
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t

        (s, t, e, IRun tmapping callix ) -> do
            instrs <- gviews supercombinators (Arr.! callix)
            -- let t' = Map.map (\lch -> t Map.! lch) $ coerce tmapping
            let t' = Map.map (\lch -> fromJust $ Map.lookup lch t) $ coerce tmapping

            return $ Just $ stec 
                & translation !~ t'
                & code !~ instrs

        (s, t, e, IHPut ch hcaseix) -> do
            writeChMQueue (chlkup ^. activeQueue) $ QHPut hcaseix
            return $ Just $ stec
                & code !~ c
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (s, t, e, ISHPut ch sinstr) -> do
            writeChMQueue (chlkup ^. activeQueue) $ QSHPut sinstr
            return $ Just $ stec
                & code !~ c
          where
            Just chlkup = Map.lookup ch t 
            -- chlkup = t Map.! ch

        (s, t , e, IHCase ch hcases) -> do
            writeChMQueue (chlkup ^. activeQueue) $ QHCase (stec & code !~ c) hcases

            stec' <- liftIO $ atomically $ do
                chactivequeue <- chlkup ^. activeQueue % to readChMQueue
                peekTQueue chactivequeue >>= \case
                    QHCase hstec hcases -> do
                        chotherqueue <- chlkup ^. otherQueue % to readChMQueue
                        peekTQueue chotherqueue >>= \case
                            QHPut hcaseix -> do
                                _ <- readTQueue chactivequeue
                                _ <- readTQueue chotherqueue
                                return $ hstec & code !~ hcases Arr.! hcaseix
                            _ -> retry
                    _ -> retry
                {-
                readActiveQueueAndOtherQueue chlkup >>= \case
                (QHCase hstec hcases, QHPut hcaseix) -> do
                    return $ hstec & code !~ hcases Arr.! hcaseix
                _ -> retry
                -}

            return $ Just $ stec'
          where
            chlkup = t Map.! ch

        (s, t , e, IRace races) -> assert (not (null races)) $ do
            {- N.B.  This works by doing the following steps

                * put the race in the queues. 

                * for each of the queues, wait for ANY of them to finish (but 
                    do NOT modify anything!)

                * Once we have a winner, first update the queue that won,
                    THEN, concurrently update all the queues and continute this 
                    process concurrently as well.
            -}
            let raceslkups = fmap (first (id &&& (t Map.!))) races 
                lchs = map fst races

            for_ raceslkups $ \((_lch, chlkup), rc) -> 
                writeChMQueue 
                    (chlkup ^. activeQueue) 
                    (QRace (stec & code !~ rc))

            liftIO $ do 
                (rch, rstec, rchlkup) <- fmap snd $ 
                    foldrM go [] (raceslkups & mapped %~ fst) 
                    >>= waitAny

                -- pop the race off the top of the queue.
                _ <- atomically $ rchlkup ^. activeQueue % to 
                    (readTQueue <=< readChMQueue)

                mapConcurrently_ id $
                    -- run the race which won
                    void (mplMachSteps' rstec) :
                    -- concurrently remove all other races
                    map 
                        ( void 
                        . atomically 
                        . view 
                            ( _1
                            % _2
                            % activeQueue 
                            % to (readTQueue <=< readChMQueue)
                            )
                        )
                        (filter ((/=rch) . fst . fst ) raceslkups)

            return Nothing
          where
            go :: 
                (LocalChan, TranslationLkup) -> 
                [Async (LocalChan, Stec, TranslationLkup)] -> 
                IO [Async (LocalChan, Stec, TranslationLkup)]
            go (lch, chlkup) acc = withAsync go' $ \asyncchlkup ->
                return $ asyncchlkup:acc
              where
                go' = atomically $ do
                    chactivequeue <- chlkup ^. activeQueue % to readChMQueue
                    peekTQueue chactivequeue >>= \case
                        QRace rstec -> do
                            chotherqueue <- chlkup ^. otherQueue % to readChMQueue
                            peekTQueue chotherqueue >>= \case
                                QPut _ -> return (lch, rstec, chlkup)
                                _ -> retry
                        _ -> retry

        _ -> k stec

    _ -> k stec
            
  where
    steccode = stec ^. code
    stecenv = stec ^. environment
    stecstack = stec ^. stack
    stectranslation = stec ^. translation

-- | this is essentially the channel manager actions
-- lchsgchs <- for chs $ \ch -> (\ch -> fmap (ch,) newGlobalChan) chs
-- Okay this is kinda stupid. like not kinda, like really really stupid..
mplMachOpenChs :: 
    -- | local channels to open up
    [LocalChan] ->
    -- | each local channel and its coresponding global channel
    MplMach MplMachEnv [(LocalChan, GlobalChan)]
mplMachOpenChs chs = for chs $ \ch -> do
    gch <- newGlobalChan 
    let chint = coerce @LocalChan @Int ch

    -- negative channels are reserved for servicechannels
    when (isTermServiceCh ch) $ void $ do
        liftIO $ putStrLn $ "SERVICE CH: "++ show ch
        -- give this service channel a unique id
        chref <- gview serviceChRefFresh 
        -- Okay a bit strange -- @svch@ SHOULD be the same integer
        -- as @ch@.
        {-
        svch <- liftIO $ atomicModifyIORef' chref 
            ( coerce @Int @ServiceCh 
            . pred 
            . coerce @ServiceCh @Int &&& id)
        -}
        let svch = coerce @LocalChan @ServiceCh ch

        -- then, add this to the map of all services.
        svmap <- gview serviceMap
        () <- liftIO $ atomicModifyIORef' svmap
            ( 
              Map.insert svch 
                ( InputLkup 
                    { _activeQueue = gch ^. coerced % chMInputQueue
                    , _otherQueue = gch ^. coerced % chMOutputQueue
                    }
                )  
              &&& const ()
            ) 


        -- silly way of doing services. 
        -- like actually so silly. still can't believe
        -- this was decided to do it this way...
        hn <- gview serviceHostName
        pn <- gview servicePortName
        liftIO $ spawnCommand $ concat
            [ "xterm -e "
            , "'"
            , "mpl-client"
            , " --hostname=" ++ show hn
            , " --port=" ++ show pn
            , " --service-ch=" ++ show (show $ coerce @LocalChan @Int ch)
            , "; read"
            , "'"
            ]
        liftIO $ threadDelay 100000

    return (ch, gch)
