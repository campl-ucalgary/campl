{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
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
import Data.List
import Data.Functor
import Control.Applicative

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Sequence

import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map

import Data.Set (Set (..))
import qualified Data.Set as Set

import MplMach.MplMachTypes
import MplMach.MplMachStack
import MplMach.MplMachException

import Control.Monad.IO.Class

import System.IO.Unsafe

import Pipes hiding (for)
import Pipes.Core 
import qualified Pipes.Parse as P
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A

import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent (forkFinally)

import Text.Read
import Data.Coerce
import System.IO (isEOF)

import Network.Socket
import Network.Socket.ByteString

import System.Process (spawnCommand)

import Debug.Trace
import qualified Text.Show.Pretty as PrettyShow


{- Some C macros which are enabled when MPL_MACH_DEBUG is defined. 
These are used to trace the queue outputs before any process takes a step. 

Notes:

    - MSG should be a string; 

    - CHLKUP should be the lookuped channel; and 
    -
    - CHLKUPS should be a list of lookuped channels.
-}
#if MPL_MACH_DEBUG

#define TRACE_TRANSLATION_LKUP_WITH_HEADER(MSG, CHLKUP) \
    (traceTranslationLkupWithHeader ((MSG)) (CHLKUP))
    -- (traceTranslationLkupWithHeader ((MSG)) (CHLKUP))
#define ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER(MSG, CHLKUP)       \
    (liftIO $ do                                                         \
        { threadid <- myThreadId                                         \
        ; let { msg = intercalate " " [(MSG), show threadid] }           \
        ; atomically $ TRACE_TRANSLATION_LKUP_WITH_HEADER(msg, CHLKUP)   \
        }                                                                \
    )
#define ATOMICALLY_TRACE_MANY_TRANSLATION_LKUP_WITH_HEADER(MSG, CHLKUPS) \
    (liftIO $ do                                                        \
        { threadid <- myThreadId                                         \
        ; let { msg = intercalate " " [(MSG), show threadid] }           \
        ; atomically                                                     \
            $ for (CHLKUPS)                                              \
            $ \lkup -> TRACE_TRANSLATION_LKUP_WITH_HEADER(msg, lkup)     \
        }                                                                \
    )

#else 

#define TRACE_TRANSLATION_LKUP_WITH_HEADER(MSG, CHLKUP)  
#define ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER(MSG, CHLKUP) 
#define ATOMICALLY_TRACE_MANY_TRANSLATION_LKUP_WITH_HEADER(MSG, CHLKUPS) 

#endif 

-- * Regualar steps as given by the table

{- | runs the MplMach from start to finish -}
mplMachSteps :: 
    Stec ->
    MplMach MplMachEnv ()
mplMachSteps = go . Just 
  where
    go = \case
        Just stec -> seqStep (concStep (liftIO . throwIO . ppShowIllegalStep)) stec 
            >>= go

        {-
        Just stec -> do
            -- traceM $ PrettyShow.ppShow stec
            stec' <- seqStep k stec 
            go stec'
          where
            k = concStep  k'
            k' = liftIO . throwIO . ppShowIllegalStep
        -}

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
        (IDivInt, e, VInt n : VInt m : s) -> {-# SCC "IDivInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ n `div` m)
        (IEqInt, e, VInt n : VInt m : s) -> {-# SCC "IEqInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n == m)
        (ILeqInt, e, VInt n : VInt m : s) -> {-# SCC "ILeqInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n <= m)
        (IGeqInt, e, VInt n : VInt m : s) -> {-# SCC "IGeqInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n >= m)
        (ILtInt, e, VInt n : VInt m : s) -> {-# SCC "ILtInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n < m)
        (IGtInt, e, VInt n : VInt m : s) -> {-# SCC "IGtInt" #-} pure $ Just $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ n > m)

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
concStep k stec = gview equality >>= \env -> let mplMachSteps' inpstec = runMplMach (mplMachSteps inpstec) env in case steccode of
    {- (stack, translation, environment, code -}
    ConcInstr instr : c -> case (stecstack, stectranslation, stecenv, instr) of
        (s, t, e, IGet ch) -> do
            fetchAndWriteChMQueue (chlkup ^. activeQueue) (QGet (stec & code !~ c))

            -- the channel manager action
            stec' <- liftIO $ atomically $ do

                -- traceTranslationLkupWithHeader  "get" chlkup
                TRACE_TRANSLATION_LKUP_WITH_HEADER("get", chlkup)

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

            return $ Just $ stec'
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 

        (v:s, t, e, IPut ch) -> do
            fetchAndWriteChMQueue (chlkup ^. activeQueue) (QPut v)

            -- liftIO $ atomically $ traceTranslationLkupWithHeader "put" chlkup
            ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("put", chlkup)

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
            ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("split", chlkup)

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
                TRACE_TRANSLATION_LKUP_WITH_HEADER("fork", chlkup)
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
            liftIO $ atomically $ do
                -- traceTranslationLkupWithHeader "idl" lchlkup
                TRACE_TRANSLATION_LKUP_WITH_HEADER("idl", lchlkup)
                -- traceTranslationLkupWithHeader "idr" rchlkup
                TRACE_TRANSLATION_LKUP_WITH_HEADER("idr", rchlkup)

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

        (s, t, e, IPlug chs (((chsins0, chsouts0), instrs0), ((chsins1, chsouts1), instrs1))) -> do
            -- this is essentially the channel manager actions
            -- lchsgchs <- for chs $ \ch -> (\ch -> fmap (ch,) newGlobalChan) chs
            -- Okay this is kinda stupid 
            -- lchsgchs <- for chs $ ap (flip (,) <$> newGlobalChan)
            lchsgchs <- for chs $ ((flip (,) <$> newGlobalChan) <*>) . pure

            let 
                -- the setter for new translations 
                tn ins outs (lch, gch)
                    | lch `Set.member` outs = (lch, outqueue)
                    | lch `Set.member` ins  = (lch, inqueue)
                    -- This will only occur if there is bad code.. i.e., this
                    -- means that we are plugging a channel which is not in the
                    -- context
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
                TRACE_TRANSLATION_LKUP_WITH_HEADER("ihalt", chlkup)

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
            ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("hput", chlkup)

            return $ Just $ stec
                & code !~ c
          where
            -- chlkup = t Map.! ch
            Just chlkup = Map.lookup ch t 


        (s, t, e, ISHPut ch sinstr) -> do
            svs <- case sinstr of 
                SHOpenThread -> do
                    svmap <- gview serviceMap
                    let slkup =  flipTranslationLkup chlkup
                    -- Don't add to the map since this thread will wait anyways 
                    {-
                    _ <- liftIO $ modifyMVar_ svmap
                        ( pure
                        . Map.insert (coerce ch) slkup
                        ) 
                    -}
                    return $ Just $ liftIO $ runMplMach (serviceThread slkup) env


                -- SHOpenTerm -> sOpenTerm ch chlkup >> return Nothing
                SHOpenTerm -> sOpenTerm chlkup >> return Nothing


                -- TODO: implement without hardcoded port 4000 (maybe add a list of server ports to env or something?)
                -- also maybe we don't need this first runMplMach i have no idea lmao
                SHOpenServer -> 
                    -- withSocketsDo $ flip runMplMach env $    -- this was causing a type error idfk
                    do
                    let slkup = flipTranslationLkup chlkup
                        hints = defaultHints { addrSocketType = Stream }
                    addrinf <- liftIO $ fmap head $ getAddrInfo (Just hints) (Just $ env ^. serviceHostName) (Just "4000")
                    return $ Just $ liftIO $ bracket 
                        -- open the socket resource
                        (open addrinf)
                        -- close the socket (library call)
                        close
                        -- run the server? (like the serviceManager function for the other terminals)
                        -- but we return type IO (...) instead of MplMach MplMachEnv (...) and this is bad??
                        (flip runMplMach env . serviceSCServer slkup)
                  where
                    -- opens the socket with sane defaults (standard C way of opening a socket translated to Haskell)
                    open addrinf = do
                        s <- socket (addrFamily addrinf) (addrSocketType addrinf) (addrProtocol addrinf)
                        setSocketOption s ReuseAddr 1
                        withFdSocket s setCloseOnExecIfNeeded
                        bind s $ addrAddress addrinf
                        listen s 1024
                        return s


                -- TODO: implement without hardcoded ip address and port 4000?
                SHOpenClient -> 
                    -- withSocketsDo $ flip runMplMach env $     -- this was causing a type error idfk
                    do
                    let slkup = flipTranslationLkup chlkup
                        hints = defaultHints { addrSocketType = Stream }
                    addrinf <- liftIO $ fmap head $ getAddrInfo (Just hints) (Just "0.0.0.0") (Just "4000")
                    return $ Just $ liftIO $ bracket 
                        (open addrinf) 
                        close 
                        (flip runMplMach env . serviceSCClient slkup)
                  where
                    open addrinf = do
                        s <- socket (addrFamily addrinf) (addrSocketType addrinf) (addrProtocol addrinf)
                        connect s $ addrAddress addrinf
                        return s


                _ -> do
                    fetchAndWriteChMQueue (chlkup ^. activeQueue) $ QSHPut sinstr
                    -- liftIO $ atomically $ traceTranslationLkupWithHeader "ihsput" chlkup
                    ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("ihsput", chlkup)

                    return Nothing
    

            let next = stec & code !~ c
            case svs of
                Just sv -> liftIO (concurrently_ (mplMachSteps' next) sv) *> return Nothing
                Nothing -> return $ Just next
                    
          where
            Just chlkup = Map.lookup ch t 
            -- chlkup = t Map.! ch

        -- ISHCase LocalChan (Map SInstr [Instr])
        (s, t, e, ISHCase ch shcases) -> do
            fetchAndWriteChMQueue (chlkup ^. activeQueue) $ QSHCase (stec & code !~ c) shcases

            stec' <- liftIO $ atomically $ do
                TRACE_TRANSLATION_LKUP_WITH_HEADER("ihscase", chlkup)

                chactivequeue <- chlkup ^. activeQueue % to readChMQueue
                peekTQueue chactivequeue >>= \case
                    QSHCase hstec shcases -> do
                        chotherqueue <- chlkup ^. otherQueue % to readChMQueue
                        peekTQueue chotherqueue >>= \case
                            QSHPut sinstr -> do
                                _ <- readTQueue chactivequeue
                                _ <- readTQueue chotherqueue
                                return $ hstec & code !~ shcases Map.! sinstr
                            _ -> retry
                    _ -> retry

            return $ Just $ stec'
          where
            chlkup = t Map.! ch

        -- IHCase LocalChan (Array HCaseIx [Instr])
        (s, t , e, IHCase ch hcases) -> do
            fetchAndWriteChMQueue (chlkup ^. activeQueue) $ QHCase (stec & code !~ c) hcases

            stec' <- liftIO $ atomically $ do
                -- traceTranslationLkupWithHeader  "ihcase" chlkup
                TRACE_TRANSLATION_LKUP_WITH_HEADER("ihcase", chlkup)

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
                (rch, rstec, rchlkup) <- runConcurrently 
                    $ asum 
                    $ map (Concurrently . go2) (raceslkups & mapped %~ fst) 

                -- atomically $ for raceslkups $ \lkup -> traceTranslationLkupWithHeader  "race" $ lkup ^. _1 % _2
                ATOMICALLY_TRACE_MANY_TRANSLATION_LKUP_WITH_HEADER("race", map (view (_1 % _2)) raceslkups)

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
                IO (LocalChan, Stec, TranslationLkup)
            go2 (lch, chlkup) = liftIO $ atomically $ do
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

-- * Services 

-- * Remote Services (Servers and Clients (SC))

-- waits for connections from SC (remote) clients
-- TODO: check that the campl process "running" the server has put a split command on the other end 
-- then we will pass on one of them to the serve remote client function and hold on to the other one for future clients
serviceSCServer :: 
    TranslationLkup ->
    Socket ->
    MplMach MplMachEnv ()
serviceSCServer slkup s = forever $ gview equality >>= \env -> liftIO $ do
    (s', _) <- accept s
    forkFinally (flip runMplMach env $ serviceSCServiceRemoteClient s' slkup) $ \err -> close s' >> case err of
        -- oops, shoudln't use 'forever' here, when it is AsyncCancelled, should probably
        -- just stop recursing.
        Right () -> return ()
        Left e -> case fromException e of
            Just AsyncCancelled -> return ()
            _ -> throwIO e

-- after we accept a connection from a remote client, 
-- TODO: add some sort of authentication. either by using a protocol that does or something,,,
serviceSCServiceRemoteClient ::
    Socket ->
    TranslationLkup ->
    MplMach MplMachEnv ()
serviceSCServiceRemoteClient s slkup = gview equality >>= \env -> do
    ~(Just esec, pbts) <- P.runStateT (PA.parse pSNString) (recvPipe s) 
    -- svmp <- liftIO $ env ^. serviceMap % to readMVar 
    case esec of
        -- silly "authentication" to check the client is "who we want"
        -- when we change the protocol the socket is running then idk if we will still need a check here?
        Right sec 
            | "secret_password" == sec -> liftIO $ 
                runMplMach (serviceClientLoop s slkup pbts (serviceQueueSInstrPipe slkup)) env 
            | otherwise -> liftIO $ throwIO $ userError "illegal client connection"
        Left err -> liftIO $ throwIO err

-- a "remote" client for some server
-- TODO: add some sort of authentication. either by using a protocol that does or something,,,
serviceSCClient :: 
    TranslationLkup ->
    Socket ->
    MplMach MplMachEnv ()
serviceSCClient slkup sock = do
    -- silly "authentication" so that the server "knows it's us"
    -- when we change the protocol the socket is running then idk if we will still need anything here?
    liftIO $ sendAll sock $ snInstrToByteString $ SNString "secret_password"
    loop (recvPipe sock)
  where
    loop ps = do
        (res, ps') <- P.runStateT (PA.parse pSNCmd) ps
        case res of
            Just (Right instr) -> case instr of
                SNGetString -> do
                    -- we are using service instructions as protocol handle ids basically...
                    fetchAndWriteChMQueue (slkup ^. activeQueue) $ QSHPut SHGetString
                    ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("ihsput", slkup)
                    n <- liftIO $ atomically $ do

                        TRACE_TRANSLATION_LKUP_WITH_HEADER("get", slkup)

                        -- idk if we want to peek and retry or not??
                        -- i think we want to retry because we are making a request
                        chotherqueue <- slkup ^. otherQueue % to readChMQueue
                        peekTQueue chotherqueue >>= \case
                            QPut v -> do
                                _ <- readTQueue chotherqueue
                                return $ valToStr v
                            _ -> retry

                    liftIO $ sendAll sock $ snInstrToByteString $ SNString n
                    loop ps'

                SNPutString -> do
                    (res, ps'') <- P.runStateT (PA.parse pSNString) ps'
                    -- the server sent that they are sending a string, so we don't need to loop
                    -- it's either there or something is wrong
                    case res of
                        Just (Right res') -> do
                            -- we are using service instructions as protocol handle ids basically...
                            fetchAndWriteChMQueue (slkup ^. activeQueue) $ QSHPut SHPutString
                            ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("ihsput", slkup)
                            fetchAndWriteChMQueue (slkup ^. activeQueue) $ QPut $ strToVal res'
                            ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("put", slkup)
                            loop ps''
                        bad -> liftIO $ throwIO $ ppShowIllegalStep bad

                SNClose -> do
                    -- we are using service instructions as protocol handle ids basically...
                    fetchAndWriteChMQueue (slkup ^. activeQueue) $ QSHPut SHClose 
                    ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("ihsput", slkup)
                    return ()

                bad -> liftIO $ throwIO $ ppShowIllegalStep bad
            bad -> liftIO $ throwIO $ ppShowIllegalStep bad


-- * Local Services (Terminals and Threads)

serviceManager :: 
    (HasMplMachServicesEnv r, MonadFail (MplMach r)) =>
    Socket ->
    MplMach r ()
serviceManager s = forever $ gview equality >>= \env -> liftIO $ do
    (s', _) <- accept s
    forkFinally (flip runMplMach env $ serviceClient s') $ \err -> close s' >> case err of
        -- oops, shoudln't use 'forever' here, when it is AsyncCancelled, should probably
        -- just stop recursing.
        Right () -> return ()
        Left e -> case fromException e of
            Just AsyncCancelled -> return ()
            _ -> throwIO e

{- | Honestly, literally everything about services is a silly mess. They make the 
language frankly unusable for anything. well actually io in general is a mess.... -}
serviceClient  ::
    (HasMplMachServicesEnv r, MonadFail (MplMach r)) =>
    Socket ->
    MplMach r ()
serviceClient s = gview equality >>= \env -> do
    ~(Just egch, pbts) <- P.runStateT (PA.parse pServiceCh) (recvPipe s) 
    svmp <- liftIO $ env ^. serviceMap % to readMVar 
    case egch of
        -- Right gch -> let gchlkup = fromJust $ svmp ^. at gch in 
        Right gch 
            | Just gchlkup  <- svmp ^. at gch -> liftIO $ 
                runMplMach (serviceClientLoop s gchlkup pbts (serviceQueueSInstrPipe gchlkup)) env 
                    `finally` modifyMVar_ (env ^. serviceMap ) (\v ->  return (Map.delete gch v ))
                -- we delete it from the map at the end
            | otherwise -> liftIO $ throwIO $ userError "illegal client connection"
        Left err -> liftIO $ throwIO err

{- | This is honestly super confusing and literally everything is horrible about this...  Some remarks
    to clear things up...

    * Since we know that services are a protocol / coprotocol, we know the first response must
        FOR SURE be a HPut
    * Then, that HPut (with the particular index) determines what we want to do

Again, this language is literally unusable with this design of services....

Also, there's lots of trash duplicated code in this.
-}

serviceClientLoop ::
    HasMplMachServicesEnv r =>
    -- | socket
    Socket ->
    -- | Translation lookup
    TranslationLkup ->
    -- | producer for the socket
    Producer ByteString (MplMach r) () ->
    -- | producer from the instructions
    Producer SInstr (MplMach r) () ->
    -- | result
    MplMach r ()
serviceClientLoop sock gchlkup psock pinstrs = next pinstrs >>= \case
    Left () -> return ()
    Right (instr, pinstrs') -> case instr of
        -- Int instructions
        SHGetInt -> do
            -- send that we want an int
            liftIO $ sendAll sock $ snInstrToByteString SNGetInt
            -- keep looking querying until we actually parse an int
            (psock', inp) <- loop psock

            -- add it to the queue
            fetchAndWriteChMQueue (gchlkup ^. activeQueue) $ QPut $ VInt inp

            serviceClientLoop sock gchlkup psock' pinstrs'
          where
            loop psock = do
                ~(mval, psock') <- P.runStateT (PA.parse pSNInt) psock 
                case mval of
                    Just (Right val) -> return (psock', val)
                    _ -> loop psock'
        SHPutInt -> do

            v <- liftIO $ atomically $ 
                gchlkup ^. otherQueue % to readChMQueue
                    >>= readTQueue
                    >>= \case
                        QPut (VInt v) -> return v
                        bad -> throwSTM $ ppShowIllegalStep bad

            -- send we want to put an int
            liftIO $ sendAll sock $ snInstrToByteString $ SNPutInt

            -- send actually put the int
            liftIO $ sendAll sock $ snInstrToByteString $ SNInt v
                
            serviceClientLoop sock gchlkup psock pinstrs'

        -- Char instructions
        SHGetChar -> do
            -- send that we want an int
            liftIO $ sendAll sock $ snInstrToByteString SNGetChar
            -- keep looking querying until we actually parse an int
            (psock', inp) <- loop psock

            -- add it to the queue
            fetchAndWriteChMQueue (gchlkup ^. activeQueue) $ QPut $ VChar inp

            serviceClientLoop sock gchlkup psock' pinstrs'
          where
            loop psock = do
                ~(mval, psock') <- P.runStateT (PA.parse pSNChar) psock 
                case mval of
                    Just (Right val) -> return (psock', val)
                    _ -> loop psock'
        SHPutChar -> do

            v <- liftIO $ atomically $ 
                gchlkup ^. otherQueue % to readChMQueue
                    >>= readTQueue
                    >>= \case
                        QPut (VChar v) -> return v
                        bad -> throwSTM $ ppShowIllegalStep bad

            -- send we want to put an int
            liftIO $ sendAll sock $ snInstrToByteString $ SNPutChar

            -- send actually put the int
            liftIO $ sendAll sock $ snInstrToByteString $ SNChar v
                
            serviceClientLoop sock gchlkup psock pinstrs'

        -- String instructions
        SHGetString -> do
            -- send that we want an int
            liftIO $ sendAll sock $ snInstrToByteString SNGetString
            -- keep looking querying until we actually parse a string
            (psock', inp) <- loop psock

            -- add it to the queue
            fetchAndWriteChMQueue (gchlkup ^. activeQueue) $ QPut $ strToVal inp

            serviceClientLoop sock gchlkup psock' pinstrs'
          where
            loop psock = do
                ~(mval, psock') <- P.runStateT (PA.parse pSNString) psock 
                case mval of
                    Just (Right val) -> return (psock', val)
                    _ -> loop psock'

        SHPutString -> do

            v <- liftIO $ atomically $ 
                gchlkup ^. otherQueue % to readChMQueue
                    >>= readTQueue
                    >>= \case
                        QPut v -> return $ valToStr v
                        bad -> throwSTM $ ppShowIllegalStep bad

            -- send we want to put an int
            liftIO $ sendAll sock $ snInstrToByteString $ SNPutString

            -- send actually put the int
            liftIO $ sendAll sock $ snInstrToByteString $ SNString v
                
            serviceClientLoop sock gchlkup psock pinstrs'

        SHClose -> do
            liftIO $ sendAll sock $ snInstrToByteString SNClose
            return ()

        SHOpenThread -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenThread
        SHOpenTerm -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenTerm
        SHSplitNegStringTerm -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHSplitNegStringTerm 
        SHTimeOut -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHTimeOut 
        SHOpenServer -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenServer
        SHOpenClient -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenClient

serviceQueueSInstrPipe ::
    ( HasMplMachServicesEnv r ) =>
    TranslationLkup ->
    Producer SInstr (MplMach r) ()
serviceQueueSInstrPipe gchlkup = go
  where
    -- go = forever $ join $ fmap yield $ liftIO $ atomically $ gchlkup ^. otherQueue % to readChMQueue 
    go = forever $ join $ fmap yield $ liftIO $ atomically $ do
        chotherqueue <- gchlkup ^. otherQueue % to readChMQueue 
        peekTQueue chotherqueue >>= \case
            QSHPut sinstr -> readTQueue chotherqueue >> return sinstr
            _ -> retry
            
recvPipe :: 
    ( MonadIO m ) =>
    Socket ->
    Producer ByteString m ()
recvPipe s = go 
  where
    go = do
        -- urgh, for some reason this isn't blocking if it doesn't receive anything
        -- so we thread delay so it doesn't needlessy spin the cpu...
        res <- liftIO $ recv s rECV_MAX_BYTES_TO_RECEIVE <* threadDelay 10000
        unless (B.null res) $ yield res 
        go

    rECV_MAX_BYTES_TO_RECEIVE :: Int
    rECV_MAX_BYTES_TO_RECEIVE = 4096

{-| Rougly follows the idea here: https://redis.io/topics/protocol. The specification is as follows.

    * Integers the first byte of the reply is ":"

    * Chars the first byte of the reply is ";"

    * Strings the first byte of the reply is "?", then the length @n@ (as an int), then @n@ characters.

    * Errors the first byte of the reply is "-" 

    * Simple strings the first byte of the reply is "+" followed by a string that cannot contain (CRLF)
        (this is used for sending commands e.g. a close comand)

    * We do not support parsing any other values (unclear with how to do this anyways).

    * Different parts of the protocol are always terminted wth "\r\n" (CRLF)
-}
pVal :: A.Parser Val
pVal = undefined

{-| ":<SOMEINT>\r\n" -}
pSNInt :: A.Parser Int
pSNInt = A.char ':' *> A.signed A.decimal <* A.endOfLine

{-| ";<SOMECHAR>\r\n" -}
pSNChar :: A.Parser Char
pSNChar = A.char ';' *> A.anyChar <* A.endOfLine

{-| "?<LENGTH>\r\n<SOMESTRINGOFLENGTH>\r\n" -}
pSNString :: A.Parser String
pSNString = do 
    _ <- A.char '?' 
    (n :: Int) <- A.decimal 
    _ <- A.endOfLine 
    str <- A.take n 
    _ <- A.endOfLine 
    return $ B.unpack str

pSNCmd :: A.Parser SNInstr
pSNCmd = 
    A.char '+' *> 
    A.choice 
        [ SNGetChar <$ A.string "GETCHAR"
        , SNPutChar <$ A.string "PUTCHAR"

        , SNGetInt <$ A.string "GETINT"
        , SNPutInt <$ A.string "PUTINT"

        , SNGetString <$ A.string "GETSTR"
        , SNPutString <$ A.string "PUTSTR"

        , SNClose <$ A.string "CLOSE"
        ]
    <* A.endOfLine

snInstrToByteString :: 
    SNInstr ->
    ByteString
snInstrToByteString = \case
    SNInt n -> ':' `B.cons` (B.pack (show n) `B.append` eoc)
    SNChar n -> ';' `B.cons` (B.pack [n] `B.append` eoc)
    SNString n -> '?' `B.cons` (B.pack (show $ length n) `B.append` eoc `B.append` B.pack n `B.append` eoc)

    SNGetChar -> '+' `B.cons` ("GETCHAR" `B.append` eoc)
    SNPutChar -> '+' `B.cons` ("PUTCHAR" `B.append` eoc)

    SNGetInt -> '+' `B.cons` ("GETINT" `B.append` eoc)
    SNPutInt -> '+' `B.cons` ("PUTINT" `B.append` eoc)

    SNGetString -> '+' `B.cons` ("GETSTR" `B.append` eoc)
    SNPutString -> '+' `B.cons` ("PUTSTR" `B.append` eoc)

    SNClose -> '+' `B.cons` ("CLOSE" `B.append` eoc)
  where
    eoc = "\r\n"

serviceChToByteString :: 
    ServiceCh ->
    ByteString
serviceChToByteString ch = '=' `B.cons` (B.pack (show (coerce @ServiceCh @Int ch)) `B.append` "\r\n")


{-| Parses a service channel which is given by:
    
    * first char is "=" then followed by an integer (of the channel), and ending with "\r\n" as usual
-}
pServiceCh :: A.Parser ServiceCh
pServiceCh = fmap (coerce @Int @ServiceCh) 
    $  A.char '=' *> A.signed A.decimal <* A.endOfLine


-- * service loop for a local thread
-- | this runs a service for a local thread.
-- Some awkardness: why does this require 'LocalChan'? This is required because
-- we are essentially using the size of the map as a reference count to know
-- when all the messages have been sent for the termination condition
serviceThread ::
    TranslationLkup ->
    MplMach MplMachEnv ()
serviceThread chlkup = loop chlkup 
  where
    loop chlkup = gview equality >>= \env -> do
        sinstr <- liftIO $ atomically $ do 
            chotherqueue <- chlkup ^. otherQueue % to readChMQueue
            peekTQueue chotherqueue >>= \case
                QSHPut sinstr -> readTQueue chotherqueue >> return sinstr
                _ -> retry 
        case sinstr of
            -- int instructions 
            SHGetInt -> do 
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Please enter an int: "
                    let inputloop = fmap readMaybe getLine 
                            >>= \case 
                                Just n ->  return (n :: Int)
                                Nothing ->  inputloop
                    n <- inputloop
                    fetchAndWriteChMQueue 
                        (chlkup ^. activeQueue) 
                        (QPut (VInt n))
                loop chlkup
            SHPutInt -> do 
                ~(QPut (VInt n)) <- liftIO $ atomically $ 
                    chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Putting int: "
                    putStrLn $ show n
                loop chlkup

            -- char instructions 
            SHGetChar -> do 
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Please enter an char: "
                    let inputloop = getLine 
                            >>= \case 
                                [c] ->  return c
                                _ ->  inputloop
                    n <- inputloop
                    fetchAndWriteChMQueue 
                        (chlkup ^. activeQueue) 
                        (QPut (VChar n))
                loop chlkup
            SHPutChar -> do 
                ~(QPut (VChar n)) <- liftIO $ atomically $ 
                    chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Putting char: "
                    putStrLn [n]
                loop chlkup

            -- string instructions 
            SHGetString -> do 
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    str <- getLine
                    fetchAndWriteChMQueue 
                        (chlkup ^. activeQueue) 
                        (QPut (strToVal str))
                loop chlkup
            SHPutString -> do
                ~(QPut inp) <- liftIO $ atomically $ 
                    chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                gview stdLock >>= \mvar -> liftIO 
                    $ withMVar mvar $ const $ putStrLn $ valToStr inp
                loop chlkup

            SHTimeOut -> do
                ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("SHTimeOut", chlkup)
                ~(QPut (VInt n)) <- liftIO $ atomically $ chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                ~(QSplit lch rch) <- liftIO $ atomically $ chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                let lch' = setTranslationLkup chlkup lch
                    rch' = setTranslationLkup chlkup rch

                -- left thread, we recurse with whatver

                -- right thread, we time out and put the unit there, then we
                -- close (not a service close, but top bot which is redundent)
                liftIO $ concurrently_ (runMplMach (serviceThread lch') env) $ do
                    threadDelay n
                    fetchAndWriteChMQueue 
                        (rch' ^. activeQueue) 
                        (QPut (unitVCons))
                    
                return ()

            --  S => S (+) Neg(StringTerminal)
            SHSplitNegStringTerm -> do
                ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("SHSplitNegStringTerm(1)", chlkup)
                ~(QSplit glch grch) <- liftIO $ atomically $ chlkup ^. otherQueue % to (peekTQueue <=< readChMQueue)

                ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("SHSplitNegStringTerm(2)", chlkup)

                -- essentially duplicated from the split case of 'concStep'
                {-
                glch <- newGlobalChan
                grch <- newGlobalChan
                fetchAndWriteChMQueue (chlkup ^. activeQueue) (QSplit glch grch)

                ATOMICALLY_TRACE_TRANSLATION_LKUP_WITH_HEADER("SHSplitNegStringTerm(3)", chlkup)

                -}
                let llkup = setTranslationLkup chlkup glch
                    rlkup = setTranslationLkup chlkup grch

                sOpenTerm rlkup

                loop llkup


            SHClose -> return ()


            SHOpenThread -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenThread
            SHOpenTerm -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenTerm
            SHOpenServer -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenServer
            SHOpenClient -> liftIO $ throwIO $ userError $ "illegal service in client: " ++ show SHOpenClient




{- | opens a terminal. It expects
-}
sOpenTerm ::
    TranslationLkup ->
        -- ^ the correspnding lookup in that processes translation 
    MplMach MplMachEnv ()
sOpenTerm chlkup = void $ do
    svmap <- gview serviceMap
    svch <- freshServiceCh 
    _ <- liftIO $ modifyMVar_ svmap 
        ( pure . Map.insert svch (flipTranslationLkup chlkup) ) 

    -- silly way of doing services. 
    -- like actually so silly. still can't believe
    -- this was decided to do it this way...
    hn <- gview serviceHostName
    pn <- gview servicePortName
    liftIO $ spawnCommand $ concat
        -- [ "open -na \"alacritty\" --args  -e "
        -- -- , "'"
        -- , "mpl-client"
        -- , " --hostname=" ++ show hn
        -- , " --port=" ++ show pn
        -- , " --service-ch=" ++ show (show $ coerce @ServiceCh @Int svch)
        -- -- , "; read"
        -- -- , "'"
        -- ]

        -- instead of automatically spawning the terminal, we just echo this command
        -- this gives us the service channel ID that we need to connect to the service client process
        -- note that the host this prints is just local host. 
        -- to connect from a different device, you need the IP address of the "main" device
        -- [ "echo \""
        -- ,"alacritty -e "
        -- , "mpl-client"
        -- , " --hostname=" ++ show hn
        -- , " --port=" ++ show pn
        -- , " --service-ch=" ++ show (show $ coerce @ServiceCh @Int svch)
        -- , "\""
        -- ]

        
        -- ORIGINAL
        [ "alacritty -e "
        -- , "'"
        , "mpl-client"
        , " --hostname=" ++ show hn
        , " --port=" ++ show pn
        , " --service-ch=" ++ show (show $ coerce @ServiceCh @Int svch)
        -- , "; read"
        -- , "'"
        ]
        -- ORIGINAL

        {-
        [ "xterm -e "
        , "'"
        , "mpl-client"
        , " --hostname=" ++ show hn
        , " --port=" ++ show pn
        , " --service-ch=" ++ show (show $ coerce @ServiceCh @Int svch)
        , "; read"
        , "'"
        ]
        -}
