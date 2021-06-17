{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module MplMach.MplMachRunner where

import Optics 

import Network.Socket
import MplMach.MplMachStep
import MplMach.MplMachTypes
import MplMach.MplMachStack
import MplMach.MplMachServices

import Network.Socket

import Control.Exception
import Control.Monad.IO.Class

import Control.Concurrent.Async

import Control.Monad

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
    Stec -> 
    -- | resulting in IO monad
    IO ()
mplMachRunnner env stec = withSocketsDo $ flip runMplMach env $ do
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

    -- the main ``loop'' of the program.
    loop s = gview equality >>= \env -> do

        -- set up the thread for managing services. we use the 'link' so that if
        -- that throws an exception, this thread will also shut down.
        svthd <- liftIO $ do
            svthd <- async $ flip runMplMach env $ serviceManager s 
            link svthd
            return svthd

        -- run the machine; and unconditionally kill the other thread
        _ <- liftIO $ liftIO (flip runMplMach env (mplMachSteps stec)) 
                `finally` liftIO (cancel svthd)

        return ()
