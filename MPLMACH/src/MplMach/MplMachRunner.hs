{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module MplMach.MplMachRunner where

import Optics 

import Network.Socket
import MplMach.MplMachStep
import MplMach.MplMachTypes
import MplMach.MplMachStack

import Network.Socket

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow

import Control.Exception
import Control.Monad.IO.Class
import Data.Traversable

import Control.Concurrent.Async
import Control.Concurrent hiding (yield)
import System.Exit

import Control.Monad
import Data.Bool


{- | Runs the mpl machine. Some notes:
    
    * This will first set up the server connection.

    * Then, it will fork a seperate thread to manage the services,
        and actually run the machine steps on the main thread.

    * When the main thread finishes, (by exception or normal termination) 
        it it will also have to kill the service thread
-}
mplMachRunnner :: 
    -- | environemnt to run this machine in
    MplMachEnv -> 
    -- | main function to execute.
    (([LocalChan], [LocalChan]), [Instr]) -> 
    -- | resulting in IO monad
    IO ()
mplMachRunnner env ((mainins, mainouts), instrs) = withSocketsDo $ flip runMplMach env $ do
    let hints = defaultHints { addrSocketType = Stream }
    addrinf <- liftIO $ fmap head $ getAddrInfo (Just hints) (Just $ env ^. serviceHostName) (Just $ env ^. servicePortName)
    liftIO $ bracket 
        -- open the socket resource
        (open addrinf)
        -- close the socket (library call)
        close
        -- run the main application
        (flip runMplMach env . loop)
  where
    -- opens the socket with sane defaults (standard C way of opening a socket translated to Haskell)
    open addrinf = do
        s <- socket (addrFamily addrinf) (addrSocketType addrinf) (addrProtocol addrinf)
        setSocketOption s ReuseAddr 1
        withFdSocket s setCloseOnExecIfNeeded
        bind s (addrAddress addrinf)
        listen s 1024
        return s

    -- the main ``loop'' of the program which doesn't actaully loop.
    loop s = gview equality >>= \env -> do

        -- set up the thread for managing services. we use the 'link' so that if
        -- that throws an exception, this thread will also shut down.
        svthd <- liftIO $ do
            svthd <- async $ flip runMplMach env $ serviceManager s 
            link svthd
            return svthd

        liftIO $ mask $ \restore -> flip runMplMach env $ do
            -- Set up the main function with the required translations 
            -- open up the channels used in the arguments of the main function
            gmainins <- for mainins $ ((flip (,) <$> newGlobalChan) <*>) . pure
            gmainouts <- for mainouts $ ((flip (,) <$> newGlobalChan) <*>) . pure

            let maint = Map.fromList $ concat
                    [ gmainins & mapped % _2 %~
                        \gch -> InputLkup 
                            { _activeQueue = gch ^. coerced % chMInputQueue
                            , _otherQueue = gch ^. coerced % chMOutputQueue
                            }
                    , gmainouts & mapped % _2 %~
                        \gch -> OutputLkup 
                            { _activeQueue = gch ^. coerced % chMOutputQueue
                            , _otherQueue = gch ^. coerced % chMInputQueue
                            }
                    ]
                stec = Stec mempty maint mempty instrs

            -- run the machine; and
            --      - If there was an exception thrown at all, ensure we kill the service thread
            --
            --      - Otherwise, busy wait until the serivce threads send all
            --      their messages and close, then kill the services thread
            _ <- liftIO $ do
                res <- try (restore (flip runMplMach env (mplMachSteps stec))) 
                case res of
                    Right () -> do
                        busyloop 
                        cancel svthd
                      where
                        -- oh god, why is this thread delay? TODO: This is here
                        -- because sometimes the main thread will finish before
                        -- sending all the messages out the network to the service thread..
                        -- really, we should have an mvar or something to keep track of 
                        -- all the child threads so we wait on the child threads to die
                        -- and ensure that all messages are sent. (i.e., reference count it)
                        --
                        -- DONE: now it busy loops until all messages are sent (i.e.,
                        -- until everything is closed)
                        busyloop = 
                            readMVar (env ^. serviceMap) >>= \svmp -> bool (threadDelay 10000 *> busyloop) (return ()) $ Map.null svmp 
                    Left err -> do
                        cancel svthd
                        throwIO (err :: SomeException)



            return ()

