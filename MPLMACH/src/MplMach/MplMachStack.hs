{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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
    , _serviceMap :: IORef (Map ServiceCh TranslationLkup)
    , _serviceChRefFresh :: IORef ServiceCh
    }


{- initializes the mpl mach service environment -}
initMplMachEnv :: 
    MplMachSuperCombinators -> 
    IO MplMachEnv 
initMplMachEnv sp = do
    mp <- newIORef mempty
    svch <- newIORef $ coerce @Int @ServiceCh (-10)
    frsh <- newIORef $ 0
    nmvar <- newMVar ()
    return $
        MplMachEnv 
            { _supercombinatorEnv = sp
            , _servicesEnv = MplMachServicesEnv 
                { _serviceHostName = "127.0.0.1"
                , _servicePortName = "3000"
                , _serviceMap = mp
                , _serviceChRefFresh = svch
                }
            , _stdLock = nmvar
            }

runMplMach ::
    MplMach r a ->
    r ->
    IO a 
runMplMach ma env = runReaderT (unwrapMplMach ma) env
    


{- | wrapper for 'newTQueue' -}
newGlobalChan ::
    MplMach r GlobalChan
{-
newGlobalChan = gview equality >>= \env -> liftIO $ do

    ans <- atomically $ do


        out <- newChMQueue
        inp <- newChMQueue
        return 
            $ coerce @ChMQueues @GlobalChan
            $ ChMQueues out inp

    atomicModifyIORef gGlobalChans ((ans:) &&& const())

    res <- atomicModifyIORef printPrintingThreadRef (const False &&& id)

    when res $ liftIO $ do
        let loop oLD = do
                gchs <- readIORef gGlobalChans
                -- putStrLn "START"
                sqs <- for gchs $ \gch -> do
                    let gch' = coerce @GlobalChan @ChMQueues  gch
                    -- putStrLn "output"
                    outt <- gch' ^. chMOutputQueue % to fetchChMQueue
                    uhout <- showChMQueue outt
                    -- PrettyShow.pPrint $ uhout 

                    -- putStrLn "input"
                    inn <- gch' ^. chMInputQueue  % to fetchChMQueue
                    uhin <- showChMQueue $ inn
                    -- PrettyShow.pPrint $ uhin
                    return $ intercalate "\n"
                        [ "output"
                        , PrettyShow.ppShow uhout
                        , "input"
                        , PrettyShow.ppShow uhin
                        , ""
                        ]
                let tres = "START-----------------------":sqs ++ ["END--"]

                when (oLD /= tres) $ putStrLn $ intercalate "\n" tres

                loop tres
                
                -- putStrLn "END"
        void $ forkIO (loop [])

    return ans

  where
    newChMQueue = fmap CNil newTQueue 
        >>= \q -> coerce <$> newTVar q

    -- showChMQueue :: ChMQueue -> IO _ 
    showChMQueue q = atomically $ do
        res <- flushTQueue q
        for (reverse res) (unGetTQueue q)
        return res
-}

{-
newGlobalChan = gview equality >>= \env -> liftIO $ atomically $ do
    out <- newChMQueue
    inp <- newChMQueue
    return $ coerce @ChMQueues @GlobalChan $ ChMQueues out inp
  where
    newChMQueue = fmap CNil newTQueue 
        >>= \q -> ChMQueue <$> newTVar q
-}
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


traceMTranslationLkup ::
    MonadIO m => 
    TranslationLkup -> 
    m ()
traceMTranslationLkup lkup = do
    liftIO $ takeMVar tracelock
    liftIO $ print lkup
    liftIO $ putMVar tracelock ()

    return ()

{-# NOINLINE tracelock  #-}
tracelock = unsafePerformIO $ newMVar ()





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
    