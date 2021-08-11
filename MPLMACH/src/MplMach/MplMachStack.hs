{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{-| Module for the DSL of the machine. 
This includes the monad transformer stack used in this machine and many
utility helper functions.
-}
module MplMach.MplMachStack where

import Optics

import Data.IORef
import Data.Coerce
import Data.Array
import Data.Traversable
import Data.Foldable

import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.List

import Control.Arrow

import Control.Monad.IO.Class
import Control.Monad.Reader

import MplMach.MplMachTypes

import Data.Map (Map)

import qualified Text.Show.Pretty as PrettyShow

import Network.Socket

import Debug.Trace

{- | This is simply a wrapper for the 'IO' monad and 'ReaderT'. -}
newtype MplMach r a = MplMach { unwrapMplMach :: ReaderT r IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader r 
    )

data MplMachEnv = MplMachEnv 
    { _supercombinatorEnv :: MplMachSuperCombinators

    , _servicesEnv :: MplMachServicesEnv 

    , _stdLock :: MVar () 
    }

data MplMachServicesEnv = MplMachServicesEnv
    { _serviceHostName :: String
    , _servicePortName :: String
    , _serviceChGen :: IORef ServiceCh
    , _serviceMap :: MVar (Map ServiceCh TranslationLkup)
    }


{- initializes the mpl mach service environment -}
initMplMachEnv :: 
    MplMachSuperCombinators -> 
    IO MplMachEnv 
initMplMachEnv sp = do
    mp <- newMVar mempty
    svch <- newIORef $ coerce @Int @ServiceCh (-10)
    nmvar <- newMVar ()
    return $
        MplMachEnv 
            { _supercombinatorEnv = sp
            , _servicesEnv = MplMachServicesEnv 
                { _serviceHostName = "127.0.0.1"
                , _servicePortName = "3000"
                , _serviceChGen = svch
                , _serviceMap = mp
                }
            , _stdLock = nmvar
            }

runMplMach ::
    MplMach r a ->
    r ->
    IO a 
runMplMach ma env = runReaderT (unwrapMplMach ma) env
    

    


{- | wrapper for 'newTQueue'. 
Notes about the debug build: 

    - in the debug build, we include not exactly fully correct ids for each of the queues.
        But in the release build we on't really need these anymore.. It's sorta hepful
        when debugging
-}
newGlobalChan ::
    MplMach r GlobalChan
#if MPL_MACH_DEBUG
newGlobalChan = gview equality >>= \env -> liftIO $ do
    out <- newChMQueue
    inp <- newChMQueue
    return $ coerce @ChMQueues @GlobalChan $ ChMQueues out inp
  where
    newChMQueue = fmap CNil newTQueueIO 
        >>= \q -> ChMQueue <$> atomicModifyIORef' iNTFRESHREF  (succ &&& id) <*> newTVarIO q

{-# NOINLINE iNTFRESHREF #-}
iNTFRESHREF :: IORef Int
iNTFRESHREF = unsafePerformIO $ newIORef 0

{-# NOINLINE printPrintingThreadRef #-}
printPrintingThreadRef :: IORef Bool
printPrintingThreadRef = unsafePerformIO $ newIORef True

{-# NOINLINE gGlobalChans #-}
gGlobalChans :: IORef [GlobalChan]
gGlobalChans = unsafePerformIO $ newIORef []

traceTranslationLkupWithHeader ::
    String ->
    TranslationLkup ->
    STM ()
traceTranslationLkupWithHeader str lkup = do
    act <- lkup ^. activeQueue % to readChMQueue
    oth <- lkup ^. otherQueue % to readChMQueue

    resact <- flushTQueue act
    resoth <- flushTQueue oth

    traceM $ intercalate "\n" $
        [ "----------------"
        , str
        , "----------------"
        ]
        ++ 
        case lkup of
            OutputLkup _ _ -> 
                [ show $ lkup ^. activeQueue
                , "act of " ++ "output"
                , PrettyShow.ppShow resact
                , show $ lkup ^. otherQueue
                , "oth of " ++ "input"
                , PrettyShow.ppShow resoth
                ]
            InputLkup _ _ ->
                [ show $ lkup ^. activeQueue
                , "act of " ++ "input"
                , PrettyShow.ppShow resact
                , show $ lkup ^. otherQueue
                , "oth of " ++ "output"
                , PrettyShow.ppShow resoth
                ]

    for_ (reverse resact) (unGetTQueue act)
    for_ (reverse resoth) (unGetTQueue oth)

    return ()
#else
newGlobalChan = gview equality >>= \env -> liftIO $ atomically $ do
    out <- newChMQueue
    inp <- newChMQueue
    return $ coerce @ChMQueues @GlobalChan $ ChMQueues out inp
  where
    newChMQueue = fmap CNil newTQueue 
        >>= \q -> ChMQueue <$> newTVar q
#endif
    

{-| sets a translation lookup to use the channels given by a 'GlobalChan' -}
setTranslationLkup ::
    -- | given translation lkup
    TranslationLkup ->
    -- | normally, this will be a newly created global channel
    GlobalChan ->
    -- | resulting translation with the queues replaces with the
    -- provided global channel
    TranslationLkup 
setTranslationLkup lkup gch = case lkup of
    InputLkup _ _ -> InputLkup 
        (gch ^. coerced % chMInputQueue)
        (gch ^. coerced % chMOutputQueue)
    OutputLkup _ _ -> OutputLkup 
        (gch ^. coerced % chMOutputQueue)
        (gch ^. coerced % chMInputQueue)

{- Converts a 'TranslationLkup' into a 'GlobalChan' -}
translationLkupToGlobalChan ::
    TranslationLkup ->
    GlobalChan
translationLkupToGlobalChan = \case
    InputLkup aq oq -> coerce $ ChMQueues 
        { _chMOutputQueue = oq
        , _chMInputQueue = aq
        }
    OutputLkup aq oq -> coerce $ ChMQueues 
        { _chMOutputQueue = aq
        , _chMInputQueue = oq
        }

{- | Flips a translation lkup so that the new translation lkup would be how a
process of opposite polarity would interact with this translation lkup 
-}
flipTranslationLkup :: 
    TranslationLkup ->
    TranslationLkup 
flipTranslationLkup = \case 
    InputLkup aq oq -> OutputLkup oq aq
    OutputLkup aq oq -> InputLkup oq aq




{- | Given a 'Polarity' and a 'ChMQueues', give the corresponding input or output
 - queue according the polarity providied. -}
getPolChMQueue :: 
    Polarity -> 
    ChMQueues ->
    ChMQueue
getPolChMQueue pol = case pol of
    Output -> view chMOutputQueue
    Input -> view chMInputQueue 



{-| reads the 'TVar' to get the 'TQueue QInstr' from queue -}
readChMQueue :: 
    ChMQueue -> 
    STM (TQueue QInstr)
readChMQueue q = q ^. chMQueueChainRef % to readTVar >>= go 
  where 
    go = \case
        CCons tvarcontinue -> readTVar tvarcontinue >>= go 
        CNil tqueue -> return tqueue

{- | Similar to 'readChMQueue' but returns the last ptr to the queue as well -}
readChMQueueWithLastPtr :: 
    ChMQueue -> 
    STM (TVar ChMQueueChain, TQueue QInstr)
readChMQueueWithLastPtr q = readTVar q' >>= goInit
  where 
    q' = q ^. chMQueueChainRef

    goInit = \case
        CCons tvarcontinue -> go tvarcontinue 
        CNil tqueue -> return (q', tqueue)

    go tvarcontinue = readTVar tvarcontinue >>= \case
        CCons tvarcontinue' -> go tvarcontinue'
        CNil tqueue -> return (tvarcontinue, tqueue)



{- | wrapper to write to the queue -}
writeChMQueue ::
    ChMQueue ->
    QInstr ->
    STM ()
writeChMQueue q instr = readChMQueue q >>= \q' -> writeTQueue q' instr

{- * Notes 
#notes#

Strictly speaking, if you're doing a lot of reads and writes with a ChMQueue,
you should use 'readChMQueue' ONCE and just work with that queue given by the read...
STM does have a performance overhead with lots of reads

TODO: I'm almost certain that these lookups don't need to be all atomic i.e., we can 
break it up to individual reads to get the next pointer (see the fetch function).
-}

{- | fetches the chm queue. -}
fetchChMQueue ::
    MonadIO m =>
    ChMQueue -> 
    m (TQueue QInstr)
fetchChMQueue q = liftIO $ atomically (coerce q ^. chMQueueChainRef % to readTVar)
    >>= go 
  where 
    go = \case
        CCons tvarcontinue -> atomically (readTVar tvarcontinue) >>= go 
        CNil tqueue -> return tqueue

{- | fetches and writes the chm queue -}
fetchAndWriteChMQueue ::
    MonadIO m =>
    ChMQueue -> 
    QInstr ->
    m () 
-- fetchAndWriteChMQueue q instr = liftIO $ fetchChMQueue q >>= atomically . flip writeTQueue instr
fetchAndWriteChMQueue q instr = liftIO $ atomically $ do
    readChMQueue q >>= flip writeTQueue instr
    -- fetchChMQueue q >>= atomically . flip writeTQueue instr

        

{- | Simplifies the chain of a chmqueue one step -- retuning (Just simplified) if 
it can be simplified, otherwise returning Nothing
(note that this mutates the queue, so it is not necessary to use the returned version
since the one passed in has been mutated appropriatly)
-}
simplifyChMQueueChain ::
    MonadIO m =>
    ChMQueue -> 
    m (Maybe ChMQueue)
simplifyChMQueueChain q = liftIO $ atomically $ readTVar headtvar >>= go
  where
    headtvar = q ^. chMQueueChainRef
    go = \case
        CCons tvarcontinue -> do
            chaincont <- readTVar tvarcontinue
            writeTVar headtvar chaincont
            return $ Just q
        CNil _ -> return Nothing

{- | fully simplifies a chm queue. -}
fullySimplifyChMQueue ::
    MonadIO m =>
    ChMQueue -> 
    m ()
fullySimplifyChMQueue q = simplifyChMQueueChain q >>= go
  where
    go = \case
        Just q' -> simplifyChMQueueChain q' >>= go
        Nothing -> return ()

{-| Reads both look up queues. Recall by convention (i.e., Prashant) we have:

    * output is LEFT queue; and 

    * input is RIGHT queue 

Although, if you need to have more specifc control over which is the input 
and output queue, it is probably better you just pattern match against
the 'TranslationLkup'.
This is unused actually.
-}
{-
readTranslationLkupQueues ::
    TranslationLkup ->
    STM (QInstr, QInstr)
readTranslationLkupQueues = \case
    InputLkup aq oq -> (,) <$> peekChMQueue oq <*> peekChMQueue aq
    OutputLkup aq oq -> (,) <$> peekChMQueue aq <*> peekChMQueue oq
-}

{-| Reads both look up queues. Here, we have:

    * the active queue is the left queue

    * the other queue is the right queue

-}
{-
readActiveQueueAndOtherQueue ::
    TranslationLkup ->
    STM (QInstr, QInstr)
readActiveQueueAndOtherQueue chlkup = 
    (,) <$> peekChMQueue (chlkup ^. activeQueue)
        <*> peekChMQueue (chlkup ^. otherQueue)
-}

{-| wrapper to read the channel manager queue. Recall by convetion, output is LEFT queue, and input is RIGHT queue-}
{-
peekChMQueues ::
    ChMQueues ->
    STM (QInstr, QInstr)
peekChMQueues qs = do
    qout <- qs ^.  to (getPolChMQueue Output)
        % coerced @ChMQueue @(TVar (TQueue QInstr)) 
        % to (readTQueue <=< readTVar)
    qin <- qs ^.  to (getPolChMQueue Input)
        % coerced @ChMQueue @(TVar (TQueue QInstr)) 
        % to (readTQueue <=< readTVar)
    return (qout, qin)
-}


$(makeClassy ''MplMachEnv)
$(makeClassy ''MplMachServicesEnv)

instance HasMplMachServicesEnv MplMachEnv where
    mplMachServicesEnv = servicesEnv 

instance HasMplMachSuperCombinators MplMachEnv where
    mplMachSuperCombinators = supercombinatorEnv 


{- | Generates a fresh service channel -}
freshServiceCh ::
    HasMplMachServicesEnv r => 
    MplMach r ServiceCh
freshServiceCh = 
    join 
        $ fmap liftIO
        $ gviews serviceChGen 
        $ flip atomicModifyIORef' 
            ( coerce @Int @ServiceCh
            . pred 
            . coerce @ServiceCh @Int
            &&& id)

    
