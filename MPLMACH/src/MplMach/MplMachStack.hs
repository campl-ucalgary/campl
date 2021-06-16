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

import Control.Concurrent.STM

import Control.Arrow

import Control.Monad.IO.Class
import Control.Monad.Reader

import MplMach.MplMachTypes

import Data.Map (Map)


import qualified Text.Show.Pretty as PrettyShow

import Network.Socket

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
    { _supercombinators :: Array CallIx [Instr]

    , _servicesEnv :: MplMachServicesEnv 
    }

data MplMachServicesEnv = MplMachServicesEnv
    { _serviceHostName :: String
    , _servicePortName :: String
    , _serviceMap :: IORef (Map ServiceCh TranslationLkup)
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
newGlobalChan = liftIO $ atomically $ do
    out <- newChMQueue
    inp <- newChMQueue
    return 
        $ coerce @ChMQueues @GlobalChan
        $ ChMQueues out inp
  where
    newChMQueue = newTQueue 
        >>= \q -> coerce @(TVar (TQueue QInstr)) @ChMQueue <$> newTVar q

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
    
