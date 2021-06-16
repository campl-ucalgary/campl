{-# LANGUAGE OverloadedStrings #-}
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

{-
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Data.Binary.Builder



mplMachRunnner :: IO ()
mplMachRunnner = do
    run 5000 application

application :: Application
application req respond = respond $ case pathInfo req of
    _ -> responseBuilder status200 [ ("Content-Type", "text/plain") ] "yo"

-}


{-
for : takes results of producers, and 
-}


{-

stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        stdinLn 


triple :: Functor m => a -> Producer a m ()
triple a = do
    yield a
    yield a

incrementer :: Int -> Server Int Int IO r
incrementer question = do
    lift $ putStrLn $ "Server received : " ++ show question
    let answer = question + 1
    lift $ putStrLn $ "Server responded: " ++ show answer
    nextQuestion <- respond answer
    incrementer nextQuestion 

oneTwoThree :: () -> Client Int Int IO ()
oneTwoThree () = forM_ [1, 2, 3] $ \question -> do
    lift $ putStrLn $ "Client requested: " ++ show question
    answer <- request question
    lift $ putStrLn $ "Client received : " ++ show answer
    lift $ putStrLn "*"

-- session = oneTwoThree <-< incrementer

mplMachRunnner :: IO ()
mplMachRunnner = runEffect $ for stdinLn (lift . putStrLn)
-}

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
        -- run the main application
        (flip runMplMach env . loop)
        -- close the socket (library call)
        close
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
