{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Network.Socket
import Network.Socket.ByteString
import Control.Exception
import Control.Concurrent
import Control.Monad.Reader
import System.IO
import System.Environment
import Data.Char
import Data.Function
import Data.Maybe
import Data.Word
import Text.ParserCombinators.ReadP

import ServiceConstants

import qualified Data.ByteString as B

type Key = String

data AmplCEnv = AmplCEnv {
        amplCHandle :: Handle
        , amplCKey :: String -- Key, and its length
    }

openAmplTCPClient ::
    HostName ->         -- ^ Remost host (e.g., google.com) or localhost
    String ->           -- ^ Port number / name 
    IO Socket
openAmplTCPClient hostname port = do
    -- look up the host name and port.. 
    -- from the docs, it either raises an exception or a non-empty
    -- list and first elem of the list is the best option
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos

    -- socket for communication..
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol

    -- CONNET TO THE SERVER!!!!
    connect sock (addrAddress serveraddr)

    return sock

-- opens an ampl tcp client and keeps trying to open it...
tryOpenAmplTCPClient ::
    HostName ->         -- ^ Remost host (e.g., google.com) or localhost
    String ->           -- ^ Port number / name 
    IO Socket
tryOpenAmplTCPClient hostname port = 
    catch
    (openAmplTCPClient hostname port)
    (\e -> do 
        print (e :: IOException)
        putStrLn "Trying again in 3 seconds..."
        threadDelay 3000000
        tryOpenAmplTCPClient hostname port
    )
----------------------------------
-- Parsing command line arguments
----------------------------------
validParse :: [(a, String)] -> Maybe a
validParse = fix f
  where
    f rec ((val, []):as) = Just val
    f rec [] = Nothing
    f rec ((_, _):as) = rec as

-- TODO -- mostly duplicated code.. I'm sure there's a way
-- to generalize parsing command line arguments nicely..
-- Also, it laboursly reparses the same thing... remove that..
parseKey :: String -> Maybe Key
parseKey str = map (toEnum . fromEnum) <$> validParse (readP_to_S keyParser str) 
  where
    keyParser :: ReadP String
    keyParser = do
        string "-k" +++ string "-key" 
        optional (char ' ')
        many1 (satisfy (const True))

parseHostName :: String -> Maybe String
parseHostName str = validParse (readP_to_S keyParser str) 
  where
    keyParser :: ReadP String
    keyParser = do
        string "-hn" +++ string "-hostname" 
        optional (char ' ')
        many1 (satisfy (const True))

parsePort :: String -> Maybe String
parsePort str = validParse (readP_to_S keyParser str) 
  where
    keyParser :: ReadP String
    keyParser = do
        string "-p" +++ string "-port" 
        optional (char ' ')
        many1 (satisfy (const True))


-- Takes the space seperated arguments,
-- and logically changes the seperation
-- so it is easy to parse the the arguments
crunchArgs :: [String] -> [String]
crunchArgs = filter (not . null) . foldr f [[]]
  where
    f :: String -> [String] -> [String]
    f ('-':str) ([]:acc) = [] : ('-' : str)  : acc
    f ('-':str) (prv:acc) = [] : ('-':str ++ " " ++ prv) : acc
    f str ([]:acc) = str : acc
    f str (prv:acc) = (str ++ " " ++ prv) : acc

data ArgError = 
    InvalidKey
    | InvalidHostName
    | InvalidPort
    deriving (Show, Exception)

validateArg :: [a] -> ArgError -> IO a
validateArg [a] e = return a
validateArg _ e = throwIO e

runAmplC :: ReaderT AmplCEnv IO ()
runAmplC = do
    env <- ask
    let handle = amplCHandle env
        key = amplCKey env

    liftIO $ putStrLn "sending key over.." 
    -- send the key over...
    liftIO $ hPutStrLn handle key

    runAmplCLoop

runAmplCLoop :: ReaderT AmplCEnv IO ()
runAmplCLoop = do
    env <- ask
    let handle = amplCHandle env
    liftIO $ putStrLn "Waiting for request..." 
    inp <- liftIO $ hGetLine handle
    if | inp == putRequest -> do
            liftIO (putStrLn "Received a put request...")
            liftIO (hGetLine handle >>= putStrLn)
            runAmplCLoop

       | inp == getRequest -> do
            liftIO (putStrLn "Received a get request...")
            cmd <- liftIO $ hGetLine handle
            liftIO $ putStrLn cmd
            getloop
            runAmplCLoop

       | inp == closeRequest -> liftIO $ do
            putStrLn "Received a close request..."
            putStrLn "Closing terminal.." 
            threadDelay 1000
       | otherwise -> do 
            liftIO (putStrLn ("Invalid server request: " ++ inp))
            runAmplCLoop
  where
    getloop = do
        env <- ask 
        let handle = amplCHandle env
        inp <- liftIO getLine
        liftIO (hPutStrLn handle inp)
        res <- liftIO $ hGetLine handle
        if | res == validGetRequest -> return ()
           | res == invalidGetRequest -> do
                -- get error message
                liftIO (hGetLine handle >>= putStrLn)
                getloop
           | otherwise -> liftIO $ putStrLn ("Invalid state: " ++ res)



main :: IO ()
main = do
    putStrLn "Opening client..."
    args <- crunchArgs <$> getArgs

    -- TODO: what about unknown arguments? 
    -- go through each at a time and try running each of the 
    -- parsers on all of them, and generate errors that way...
    key <- validateArg (mapMaybe parseKey args) InvalidKey
    print key

    hostname <- validateArg (mapMaybe parseHostName args) InvalidHostName
    print hostname

    port <- validateArg (mapMaybe parsePort args) InvalidPort
    print port

    bracket 
        (tryOpenAmplTCPClient hostname port >>= (`socketToHandle` ReadWriteMode))
        hClose
        (\handle -> hSetBuffering handle LineBuffering >> runReaderT runAmplC (AmplCEnv { amplCHandle = handle, amplCKey = key } ))
