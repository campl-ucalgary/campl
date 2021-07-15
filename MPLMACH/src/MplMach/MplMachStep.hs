{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module MplMach.MplMachStep 
    ( mplMachSteps
    , mplMachOpenChs
    ) 
    where

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
import Data.List

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Sequence

import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map

import Data.Set (Set (..))
import qualified Data.Set as Set

import MplMach.MplMachTypes
import MplMach.MplMachStack
import MplMach.MplMachException
import MplMach.MplMachServices

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
            -- traceM $ PrettyShow.ppShow stec
            stec' <- seqStep k stec 
            go stec'
          where
            k = concStep  k'
            k' = liftIO . throwIO . ppShowIllegalStep

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
        (IStore, e, v:s) -> {-# SCC "IStore" #-} pure $ Just $
            stec & code !~ c
                 & environment %!~ cons v
                 & stack !~ s
        {- Access(n):c, e, s ---> c, e, e[n]:s -}
        (IAccess n, e, s) -> {-# SCC "IAccess" #-} pure $ Just $
            stec & code !~ c
                 & stack %!~ cons (fromJust $ e ^? element n)
        {- Call(c'):c, e, s --> c', e, clos(c,e):s [NOT LONGER DOES THIS]-}
        {- Call(c'):c, e_1... e_n,e', s --> c', e_1...e_n, clos(c,e'):s -}
        (ICall cix n, e, s) -> {-# SCC "ICall" #-} do
            c' <- gviews supercombinators $ (Arr.! cix)
            return $ Just $ 
                stec & code !~ c'
                     & environment !~ args
                     & stack %!~ cons (VClos c e')
          where
            (args, e') = splitAt n e

        {- Ret:c, e, v:clos(c',e'):s ---> c', e', v:s -}
        (IRet, e , v : VClos c' e': s) -> {-# SCC "IRet" #-} pure $ Just $
            stec & code !~ c'
                 & environment !~ e'
                 & stack !~ cons v s

        {- Cons(i,n):c, e, v_1:...:v_n:s --> c,e,cons(i,[v_1,...,v_n]):s-}
        (ICons i n, e, s) -> {-# SCC "ICons" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s'
                 & stack %!~ cons (VCons i args) 
          where
            (args, s') = splitAt n s

        {- Case(c_1,...,c_n):c, e: Cons(i,[v_1,...,v_n]):s ---> c_i, v_1:...:v_n:e, clo(c,e):s) -}
        (ICase cases, e, VCons i vs : s) -> {-# SCC "ICase" #-} pure $ Just $
            stec & code !~ (cases Arr.! i)
                 & environment %!~ (vs++)
                 & stack !~ s
                 & stack %!~ cons (VClos c e) 

        {- Rec(c_1,...c_n):c, e, s ---> c, e, rec([c_1,..,c_n], e):s -}
        (IRec recs, e, s) -> {-# SCC "IRec" #-} pure $ Just $
            stec & code !~ c
                 & stack %!~ cons (VRec recs e) 

        {- Dest(i, n) : c, e , rec([c_1...c_n,e'):v_n:...:v_1:s ---> c_i, v_1...v_n:e', clo(c,e):s -}
        (IDest i n, e, VRec recs e' : s) -> {-# SCC "IDest" #-} pure $ Just $
            stec & code !~ (recs Arr.! i)
                 & environment !~ e'
                 & environment %!~ (reverse args ++)
                 & stack !~ s'
                 & stack %!~ cons (VClos c e)
          where
            (args, s') = splitAt n s

        {- IIf(c1,c2) :c, e, True:s --> c1, e, s -}
        {- IIf(c1,c2) :c, e, False:s --> c2, e, s -}
        (IIf c1 c2, e, VBool True : s) -> {-# SCC "IIf" #-} pure $ Just $
            stec & code !~ c1
                 & stack !~ s
                 & stack %!~ cons (VClos c e)
        (IIf c1 c2, e, VBool False : s) -> {-# SCC "IIf" #-} pure $ Just $
            stec & code !~ c2
                 & stack !~ s
                 & stack %!~ cons (VClos c e)

        {- Const(v) :c, e, s --> c, e, v : s -}
        (IConst v, e, s) -> {-# SCC "IConst" #-} pure $ Just $
            stec & code !~ c
                 & stack %!~ cons v

        {- built in operations -}
        (ITuple n, e, s) -> {-# SCC "ITuple" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s'
                 & stack %!~ cons 
                    (VTuple $ Arr.listArray (TupleIx 0, TupleIx $ pred $ length tp) tp)
          where
            (tp, s') = splitAt n s

        (ITupleElem n, e, VTuple tuplearr : s) -> {-# SCC "ITupleElem" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (tuplearr Arr.! n)

        (IAddInt, e, VInt n : VInt m : s) -> {-# SCC "IAddInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ n + m)
        (ISubInt, e, VInt n : VInt m : s) -> {-# SCC "ISubInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ n - m)
        (IMulInt, e, VInt n : VInt m : s) -> {-# SCC "IMulInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ n * m)
        (IEqInt, e, VInt n : VInt m : s) -> {-# SCC "IEqInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n == m)
        (ILeqInt, e, VInt n : VInt m : s) -> {-# SCC "ILeqInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n <= m)
        (ILtInt, e, VInt n : VInt m : s) -> {-# SCC "ILtInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n < m)

        (IEqBool, e, VBool n : VBool m : s) -> {-# SCC "IEqBool" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n == m)
        (IEqChar, e, VChar n : VChar m : s) -> {-# SCC "IEqChar" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n == m)

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
            fetchAndWriteChMQueue (chlkup ^. activeQueue) (QGet (stec & code !~ c))

            -- the channel manager action
            stec' <- liftIO $ atomically $ do
                -- traceTranslationLkupWithHeader  "get" chlkup
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
                liftIO $ atomically $ do
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
                    -}
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
            fetchAndWriteChMQueue (chlkup ^. activeQueue) (QPut v)
            -- liftIO $ atomically $ traceTranslationLkupWithHeader "put" chlkup
            return $ Just $ stec 
                & stack !~ s
                & code !~ c
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (_, t, e, ISplit ch (lch, rch)) -> do
            glch <- newGlobalChan
            grch <- newGlobalChan

            fetchAndWriteChMQueue (chlkup ^. activeQueue) (QSplit glch grch)

            -- liftIO $ atomically $ traceTranslationLkupWithHeader "split" chlkup

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
            fetchAndWriteChMQueue (chlkup ^. activeQueue) (QFork (ch0, stec0 ) (ch1, stec1))

            -- the channel manager action
            (stec0', stec1') <- liftIO $ atomically $ do
                -- traceTranslationLkupWithHeader  "fork" chlkup
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
                
            liftIO $ concurrently_ (mplMachSteps' stec0') (mplMachSteps' stec1')
            
            return Nothing
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 


        (s, t, e, IId lch rch) -> do
            fetchAndWriteChMQueue (lchlkup ^. activeQueue) $ QId (translationLkupToGlobalChan rchlkup)

            -- the channel manager action
            {- Okay, the new way... Essentially, there table isn't completely right -- and
            we're a little bit ``too'' specific and we just execute id if we can go either way....
            
            I think the 2.* instructions can be moved to a seperate neg command entirely...
            I don't think these should be all in the IId commmand and there in fact needs
            to be some seperation between the two commands.
            -}
            {-
            _ <- liftIO $ atomically $ do
                traceTranslationLkupWithHeader "idl" lchlkup
                traceTranslationLkupWithHeader "idr" rchlkup
                (lchmlastptr, lchmqueue) <- readChMQueueWithLastPtr (lchlkup ^. activeQueue)
                peekTQueue lchmqueue >>= \case
                    QId rgch -> readTQueue lchmqueue >> case lchlkup of
                        {- the idea is as follows.
                            Given:
                                (b, q | [id b = a]) , (a, [] | q')
                            Set:

                                1.1. [] to point to q

                                1.2. the pointer after removing the head of [id b = a]
                                    to point to q' (why do we do this? this is to
                                    update the "global translation" manager so all
                                    other references to "b" now reference "a".  

                            Alternatively, given
                            Given:
                                (b, [] | id b = a : q) , (a, q' | [])
                            Set:

                                2.1. [] to point to q' (on the right)

                                2.2. the pointer after removing the head of [id b = a]
                                    to point to q (why do we do this? this is to
                                    update the "global translation" manager so all
                                    other references to "b" now reference "a".  
                            -}
                        InputLkup linqueue loutqueue -> do
                            -- This does 1.1
                            (routlstptr, routqueue) <- rgch ^. coerced 
                                % chMOutputQueue 
                                % to readChMQueueWithLastPtr 
                            isempty <- (&&) <$> isEmptyTQueue routqueue <*> isEmptyTQueue lchmqueue

                            if isempty
                                then do 
                                    writeTVar routlstptr (CCons (loutqueue ^. chMQueueChainRef)) 
                                    -- This does 1.2. 
                                    writeTVar lchmlastptr (CCons (rgch ^. coerced % chMInputQueue % chMQueueChainRef))
                                else do 
                                    -- this does 2.1
                                    (rinlstptr, rinqueue) <- rgch ^. coerced 
                                        % chMInputQueue 
                                        % to readChMQueueWithLastPtr 
                                    isEmptyTQueue rinqueue >>= check
                                    writeTVar rinlstptr (CCons (linqueue ^. chMQueueChainRef))

                                    -- 2.2.
                                    (loutlstptr, loutqueue) <- loutqueue ^. to readChMQueueWithLastPtr
                                    isEmptyTQueue loutqueue >>= check
                                    writeTVar loutlstptr (CCons (rgch ^. coerced % chMOutputQueue % chMQueueChainRef))


                        {- the idea is as follows (compeltely similar to above.
                            Given:
                                (b, [id b = a] | q ) , (a, q'  | [])
                            Set:

                                1.1. the pointer after removing the head of [id b = a]
                                    to point to q'

                                1.2. [] to point to q

                            Mostly duplciated code from previous case...


                            Alternatively
                            Given:
                                (b, id b = a:q | [] ) , (a, []  | q')
                            Set:

                                2.1. (left null to q')

                                2.2. (right null [] to point to q)
                            -}
                        OutputLkup loutqueue linqueue -> do
                            -- This does 1.2.
                            (rinlstptr, rinqueue) <- rgch ^. coerced 
                                % chMInputQueue 
                                % to readChMQueueWithLastPtr 
                            isempty <- (&&) <$> isEmptyTQueue rinqueue <*> isEmptyTQueue lchmqueue
                            if isempty
                                then do
                                    writeTVar rinlstptr (CCons (linqueue ^. chMQueueChainRef))
                                    -- This does 1.1. 
                                    writeTVar lchmlastptr (CCons 
                                        (rgch ^. coerced % chMOutputQueue % chMQueueChainRef))
                                else do
                                    -- 2.1.
                                    (linlstptr, linqueue) <- linqueue ^. to readChMQueueWithLastPtr
                                    isEmptyTQueue linqueue >>= check
                                    writeTVar linlstptr (CCons (rgch ^. coerced % chMInputQueue % chMQueueChainRef))

                                    -- 2.2. 
                                    -- this does 2.1
                                    (routlstptr, routqueue) <- rgch ^. coerced 
                                        % chMOutputQueue 
                                        % to readChMQueueWithLastPtr 
                                    isEmptyTQueue routqueue >>= check
                                    writeTVar routlstptr (CCons (loutqueue ^. chMQueueChainRef))


                            
                    _ -> retry
            -}

            -- N.B. this is the old method (which works)
            liftIO $ atomically $ do
                -- traceTranslationLkupWithHeader "idl" lchlkup
                -- traceTranslationLkupWithHeader "idr" rchlkup
                (lchmlastptr, lchmqueue) <- readChMQueueWithLastPtr (lchlkup ^. activeQueue)
                peekTQueue lchmqueue >>= \case
                    QId rgch -> readTQueue lchmqueue >> case lchlkup of
                        {- the idea is as follows.
                            Given:
                                (b, q | [id b = a]) , (a, [] | q')
                            Set:

                                1. [] to point to q

                                2. the pointer after removing the head of [id b = a]
                                    to point to q' (why do we do this? this is to
                                    update the "global translation" manager so all
                                    other references to "b" now reference "a".  

                            -}
                        InputLkup linqueue loutqueue -> do
                            -- This does 2. 
                            writeTVar lchmlastptr (CCons (rgch ^. coerced % chMInputQueue % chMQueueChainRef))

                            {-
                            flushed <- flushTQueue lchmqueue
                            rinqueue <- rgch ^. coerced
                                % chMInputQueue
                                % to readChMQueue
                            for flushed $ \instr -> writeTQueue rinqueue instr
                            -}


                            -- This does 1.
                            (routlstptr, routqueue) <- rgch ^. coerced 
                                % chMOutputQueue 
                                % to readChMQueueWithLastPtr 
                            isEmptyTQueue routqueue >>= check
                            {-
                            flushed <- flushTQueue routqueue
                            check $ null flushed || isSuspendingQInstr (head flushed)
                            loutqueue' <- readChMQueue loutqueue
                            for (reverse flushed) $ \instr -> unGetTQueue loutqueue' instr
                            -}
                            writeTVar routlstptr (CCons (loutqueue ^. chMQueueChainRef))

                        {- the idea is as follows (compeltely similar to above.
                            Given:
                                (b, [id b = a] | q ) , (a, q'  | [])
                            Set:

                                1. the pointer after removing the head of [id b = a]
                                    to point to q'

                                2. [] to point to q

                            Mostly duplciated code from previous case:
                            -}
                        OutputLkup loutqueue linqueue -> do
                            -- This does 1. 
                            writeTVar lchmlastptr (CCons 
                                (rgch ^. coerced % chMOutputQueue % chMQueueChainRef))

                            {-
                            flushed <- flushTQueue lchmqueue
                            routqueue <- rgch ^. coerced
                                % chMOutputQueue
                                % to readChMQueue
                            for flushed $ \instr -> writeTQueue routqueue instr
                            -}

                            -- This does 2.
                            (rinlstptr, rinqueue) <- rgch ^. coerced 
                                % chMInputQueue 
                                % to readChMQueueWithLastPtr 
                            isEmptyTQueue rinqueue >>= check
  
                            {-
                            flushed <- flushTQueue rinqueue
                            check $ null flushed || isSuspendingQInstr (head flushed)
                            linqueue' <- readChMQueue linqueue
                            for (reverse flushed) $ \instr -> unGetTQueue linqueue' instr
                            -}

                            writeTVar rinlstptr (CCons (linqueue ^. chMQueueChainRef))
                            
                    _ -> retry

            return Nothing

          where
            -- lchlkup = t Map.! lch
            Just lchlkup = Map.lookup lch t 
            -- rchlkup = t Map.! rch
            Just rchlkup = Map.lookup rch t 
        {-
        (s, t, e, IPlug chs ((chs0, instrs0), (chs1, instrs1))) -> do
            -- this is essentially the channel manager actions
            -- lchsgchs <- for chs $ \ch -> (\ch -> fmap (ch,) newGlobalChan) chs
            -- Okay this is kinda stupid 
            -- Also, _svs /SHOULD/ always be empty list..
            (lchsgchs, _svs) <- mplMachOpenChs chs


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
        -}
        (s, t, e, IPlug chs (((chsins0, chsouts0), instrs0), ((chsins1, chsouts1), instrs1))) -> do
            -- this is essentially the channel manager actions
            -- lchsgchs <- for chs $ \ch -> (\ch -> fmap (ch,) newGlobalChan) chs
            -- Okay this is kinda stupid 
            -- Also, _svs /SHOULD/ always be empty list..
            (lchsgchs, _svs) <- mplMachOpenChs chs


            let 
                -- the setter for new translations 
                tn ins outs (lch, gch)
                    | lch `Set.member` outs = (lch, outqueue)
                    | lch `Set.member` ins  = (lch, inqueue)
                    | otherwise = error "bad plug command"
                  where
                    outqueue = OutputLkup 
                        { _activeQueue = gch ^. coerced % chMOutputQueue
                        , _otherQueue = gch ^. coerced % chMInputQueue
                        }
                    inqueue = InputLkup 
                        { _activeQueue = gch ^. coerced % chMInputQueue 
                        , _otherQueue = gch ^. coerced % chMOutputQueue
                        }
                

                -- new translations for t0 (recall the first phrase should be 
                -- all output polarity) N.B. this is changed os we have expicity polarity passed in..
                t0n = Map.fromList ( lchsgchs & mapped %~ tn chsins0 chsouts0 )
                -- new translations for t1 (recall the second phrase should be 
                -- all input polarity) N.B. this is changed os we have expicity polarity passed in..
                t1n = Map.fromList ( lchsgchs & mapped %~ tn chsins1 chsouts1 )

                t0 = Map.restrictKeys t (chsins0 <> chsouts0) <> t0n
                t1 = Map.restrictKeys t (chsins1 <> chsouts1) <> t1n

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

            _ <- liftIO $ atomically $ do
                -- traceTranslationLkupWithHeader  "ihalt" chlkup
                chactivequeue <- chlkup ^. activeQueue % to readChMQueue
                isEmptyTQueue chactivequeue >>= check

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

        (s, t, e, IRun tmapping callix n) -> do
            instrs <- gviews supercombinators (Arr.! callix)
            -- let t' = Map.map (\lch -> t Map.! lch) $ coerce tmapping
            let t' = Map.map (\lch -> fromJust $ Map.lookup lch t) $ coerce tmapping

            return $ Just $ stec 
                & translation !~ t'
                & environment !~ args
                & code !~ instrs
          where
            (args, _e') = splitAt n e

        (s, t, e, IHPut ch hcaseix) -> do
            fetchAndWriteChMQueue (chlkup ^. activeQueue) $ QHPut hcaseix
            -- liftIO $ atomically $ traceTranslationLkupWithHeader "hput" chlkup
            return $ Just $ stec
                & code !~ c
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (s, t, e, ISHPut ch sinstr) -> do
            fetchAndWriteChMQueue (chlkup ^. activeQueue) $ QSHPut sinstr
            -- liftIO $ atomically $ traceTranslationLkupWithHeader "ihsput" chlkup
            return $ Just $ stec
                & code !~ c
          where
            Just chlkup = Map.lookup ch t 
            -- chlkup = t Map.! ch

        (s, t , e, IHCase ch hcases) -> do
            fetchAndWriteChMQueue (chlkup ^. activeQueue) $ QHCase (stec & code !~ c) hcases

            stec' <- liftIO $ atomically $ do
                -- traceTranslationLkupWithHeader  "ihcase" chlkup
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

                (1) put the race in the queues. 

                (2) for each of the queues, wait for ANY of them to finish (but 
                    do NOT modify anything!)

                (3) Once we have a winner, first update the queue that won (3.1),
                    THEN, concurrently update all the queues and continute this 
                    process concurrently as well (3.2).
            -}
            let raceslkups = fmap (first (id &&& (t Map.!))) races 
                lchs = map fst races

            -- This is (1)
            for_ raceslkups $ \((_lch, chlkup), rc) -> 
                fetchAndWriteChMQueue
                    (chlkup ^. activeQueue) 
                    (QRace (stec & code !~ rc))

            liftIO $ do 
                -- This is (2)
                (rch, rstec, rchlkup) <- fmap snd $ 
                    foldrM go2 [] (raceslkups & mapped %~ fst) 
                        >>= waitAnyCancel

                -- atomically $ for raceslkups $ \lkup -> traceTranslationLkupWithHeader  "race" $ lkup ^. _1 % _2

                -- This is (3.1)
                -- pop the race off the top of the queue.
                -- _ <- atomically $ rchlkup ^. activeQueue % to (readTQueue <=< readChMQueue)
                _ <- atomically $ go3 rchlkup 

                mapConcurrently_ id $
                    -- run the race which won
                    void (mplMachSteps' rstec) :
                    -- concurrently remove all other races (3.2)
                    map 
                        ( void 
                        . atomically 
                        . view 
                            ( _1
                            % _2
                            % to go3
                            -- % activeQueue 
                            -- % to (readTQueue <=< readChMQueue)
                            )
                        )
                        (filter ((/=rch) . fst . fst ) raceslkups)

            return Nothing
          where
            -- helper function for (2).
            go2 :: 
                (LocalChan, TranslationLkup) -> 
                [Async (LocalChan, Stec, TranslationLkup)] -> 
                IO [Async (LocalChan, Stec, TranslationLkup)]
            {-
            go2 (lch, chlkup) acc = withAsync go2' $ \asyncchlkup ->
                return $ asyncchlkup:acc
            -}
            go2 (lch, chlkup) acc = (:) <$> (racer >>= link >> racer) <*> pure acc
              where
                racer = async $ atomically $ do
                    chactivequeue <- chlkup ^. activeQueue % to readChMQueue
                    peekTQueue chactivequeue >>= \case
                        QRace rstec -> do
                            chotherqueue <- chlkup ^. otherQueue % to readChMQueue
                            peekTQueue chotherqueue >>= \case
                                QPut _ -> return (lch, rstec, chlkup)
                                _ -> retry
                        _ -> retry

            -- helper function for (3).
            go3 :: TranslationLkup -> STM ()
            go3 lkup = do 
                activqueue <- lkup ^. activeQueue % to readChMQueue
                peekTQueue activqueue >>= \case 
                    QRace _ -> void $ readTQueue activqueue
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
    -- | each local channel and its coresponding global channel; AND
    -- a thread to manage the service.
    MplMach MplMachEnv ([(LocalChan, GlobalChan)], [MplMach MplMachEnv ()])
mplMachOpenChs chs = fmap (second catMaybes . unzip) $ for chs $ \ch -> do
    gch <- newGlobalChan 
    let chint = coerce @LocalChan @Int ch

    -- negative channels are reserved for servicechannels
    ioact <- if isServiceCh ch
        then do
            -- give this service channel a unique id
            chref <- gview serviceChRefFresh 
            let svch = coerce @LocalChan @ServiceCh ch

            if isInputServiceCh ch
                then do
                    -- then, add this to the map of all services.
                    let glkup = OutputLkup 
                                { _activeQueue = gch ^. coerced % chMOutputQueue
                                , _otherQueue = gch ^. coerced % chMInputQueue
                                }

                    svmap <- gview serviceMap
                    () <- liftIO $ atomicModifyIORef' svmap
                        ( 
                          Map.insert svch glkup
                          &&& const ()
                        ) 
                    return $ Just $ serviceThread glkup
                
                else do
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
                    -- oh god, why is this thread delay here?
                    -- Well unfortunately, when we open a terminal window, we literally
                    -- have no control over when it finishes and ends, so we just put a
                    -- somewhat long thread delay so that it can for sure open before this
                    -- program proceeds. This is mainly in place for when the program is
                    -- simply putting an int on a terminal.
                    liftIO $ threadDelay 100000
                    return Nothing
        else
            return Nothing

    return ((ch, gch), ioact)


