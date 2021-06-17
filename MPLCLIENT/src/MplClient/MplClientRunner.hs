{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module MplClient.MplClientRunner where

import Optics

import MplClient.Flags
import MplClient.MplClientStack
import MplMach.MplMachServices
import MplClient.MplClientException

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as B

import Control.Exception
import Control.Monad.IO.Class

import Pipes
import qualified Pipes.Parse as P
import qualified Pipes.Attoparsec as PA

import Text.Read

import Data.Maybe


-- pSNCmd 
mplClientRunner :: IO ()
mplClientRunner = getOpts >>= \env -> flip runMplClient env $ do
    hn <- gview hostname
    p <- gview port
    let hints = defaultHints { addrSocketType = Stream }
    addrinf <- liftIO $ fmap head $ getAddrInfo (Just hints) (Just hn) (Just p)
    liftIO $ bracket 
        (open addrinf) 
        close 
        (flip runMplClient env . mplClient)
  where
    open addrinf = do
        s <- socket (addrFamily addrinf) (addrSocketType addrinf) (addrProtocol addrinf)
        connect s $ addrAddress addrinf
        return s


{- | starts the client
 
    * First, it will essentially "authenticate"

    * then, it will interact according to the protocol givne in the server

 -}
mplClient :: Socket -> MplClient MplClientEnv ()
mplClient sock = do
    sch <- gview serviceCh
    liftIO $ sendAll sock $ serviceChToByteString sch
    loop (recvPipe sock)
  where
    loop ps = do
        (res, ps') <- P.runStateT (PA.parse pSNCmd) ps
        case res of
            Just (Right instr) -> case instr of
                SNGetInt -> do
                    n <- liftIO $ input "Please enter an int: "
                    liftIO $ sendAll sock $ snInstrToByteString $ SNInt n
                    loop ps'

                SNPutInt -> do
                    n <- liftIO $ putStrLn "Receiving an int: "
                    (res, ps'') <- P.runStateT (PA.parse pSNInt) ps'
                    case res of
                        Just (Right res') -> do
                            liftIO $ putStrLn $ show res'
                            loop ps''
                        bad -> liftIO $ throwIO $ IllegalServerCommand $ show bad

                SNClose -> liftIO $ putStrLn "Closing... (press enter)" *> getLine *> return ()

                bad -> liftIO $ throwIO $ IllegalServerCommand $ show bad
            bad -> liftIO $ throwIO $ IllegalServerCommand $ show bad

    input :: Read a => String -> IO a
    input str = putStrLn str >> fmap readMaybe getLine >>= \case
        Just x -> return x
        Nothing -> input str



