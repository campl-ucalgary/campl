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
    

{- | wrapper to write to the queue -}
writeChMQueue ::
    ChMQueue ->
    QInstr ->
    MplMach r ()
writeChMQueue q instr = liftIO $ atomically $ 
    readTVar (coerce @ChMQueue @(TVar (TQueue QInstr)) q)  
        >>= \q' -> writeTQueue q' instr


{- | wrapper for 'newTQueue' -}
newGlobalChan ::
    MplMach r GlobalChan
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
                    uhout <- showChMQueue $ gch' ^. chMOutputQueue 
                    -- PrettyShow.pPrint $ uhout 

                    -- putStrLn "input"
                    uhin <- showChMQueue $ gch' ^. chMInputQueue 
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
    newChMQueue = newTQueue 
        >>= \q -> coerce @(TVar (TQueue QInstr)) @ChMQueue <$> newTVar q

{-# NOINLINE printPrintingThreadRef #-}
printPrintingThreadRef :: IORef Bool
printPrintingThreadRef = unsafePerformIO $ newIORef True

{-# NOINLINE gGlobalChans #-}
gGlobalChans :: IORef [GlobalChan]
gGlobalChans = unsafePerformIO $ newIORef []

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
readChMQueue = view 
    ( coerced @ChMQueue @(TVar (TQueue QInstr)) 
    % to readTVar
    )

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
    
