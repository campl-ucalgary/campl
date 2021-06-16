{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module MplMach.MplMachServices where

import Optics

import Pipes
import Pipes.Core 
import qualified Pipes.Parse as P
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as A

import Control.Monad.State.Strict

import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent (forkFinally)

import System.IO (isEOF)
import Control.Monad
import Data.Coerce

import MplMach.MplMachTypes
import MplMach.MplMachStack
import MplMach.MplMachException
import MplMach.MplMachTypes

import Data.IORef
import Data.Maybe

import Network.Socket
import Network.Socket.ByteString


data SNInstr
    = SNInt Int
    | SNChar Char
    | SNGet
    | SNPut

defMplMachServicesEnv :: IO MplMachServicesEnv 
defMplMachServicesEnv = do
    mp <- newIORef mempty
    return MplMachServicesEnv 
        { _serviceHostName = "localhost"
        , _servicePortName = "6969"
        , _serviceMap = mp
        }

serviceManager :: 
    HasMplMachServicesEnv r =>
    Socket ->
    MplMach r ()
serviceManager s = forever $ gview equality >>= \env -> liftIO $ do
    (s', _) <- accept s
    forkFinally (flip runMplMach env $ serviceClient s') $ \err -> close s' >> case err of
        Right () -> return ()
        Left e -> throwIO e

{- | Honestly, literally everything about services is a silly mess. They make the 
language frankly unusable for anything. -}
serviceClient  ::
    HasMplMachServicesEnv r =>
    Socket ->
    MplMach r ()
serviceClient s = gview equality >>= \env -> do
    ~(Just egch, ps) <- P.runStateT (PA.parse pServiceCh) (recvPipe s) 
    svmp <- liftIO $ env ^. serviceMap % to readIORef 
    case egch of
        Right gch -> runEffect $ for (queueReader gchlkup) $ \case
            SGetChar -> do
                -- Producer ByteString (MplMach r) () 
                -- P.runStateT (PA.parse pServiceCh) fk

                undefined
            SPutChar -> undefined

            SClose -> error "impossible"
          where
            gchlkup = fromJust $ svmp ^. at gch 
        Left err -> liftIO $ throwIO err
  where
    {- This is honestly super confusing and literally everything is horrible about this...  Some remarks
        to clear things up...

        * Since we know that services are a protocol / coprotocol, we know the first response must
            FOR SURE be a HPut
        * Then, that HPut (with the particular index) determines what we want to do
    -}
    queueReader gchlkup = loop
      where
        loop = do
            sinstr <- liftIO $ atomically $ gchlkup ^. otherQueue % to readChMQueue 
                >>= readTQueue
                >>= \case 
                    QHSPut sinstr -> return sinstr
                    bad -> throwSTM $ ppShowIllegalService bad
            case sinstr of
                -- close is just not doing anything
                SClose -> return ()

                n -> yield n

    {-
    case res of
        Just pres -> case pres of
            Right gch -> return ()
            Left err -> return ()
        Nothing -> return ()
    -}

recvPipe :: 
    ( MonadIO m ) =>
    Socket ->
    Producer ByteString m ()
recvPipe s = join $ fmap yield $ liftIO $ recv s rECV_MAX_BYTES_TO_RECEIVE
  where
    rECV_MAX_BYTES_TO_RECEIVE :: Int
    rECV_MAX_BYTES_TO_RECEIVE = 4096
    



{-| Rougly follows the idea here: https://redis.io/topics/protocol. The specification is as follows.

    * Integers the first byte of the reply is ":"

    * Chars the first byte of the reply is ";"

    * Errors the first byte of the reply is "-" 

    * Simple strings the first byte of the reply is "+" followed by a string that cannot contain (CRLF)

    * We do not support parsing any other values (unclear with how to do this anyways).

    * Different parts of the protocol are always terminted wth "\r\n" (CRLF)
-}
pVal :: A.Parser Val
pVal = A.choice 
    [ VInt <$> pInt
    , VChar <$> pChar
    ]
  where
    pInt :: A.Parser Int
    pInt = A.char ':' *> A.signed A.decimal <* A.endOfLine


    pError :: Char
    pError = undefined

{-| ";<SOMECHAR>\r\n" -}
pChar :: A.Parser Char
pChar = A.char ';' *> A.anyChar <* A.endOfLine

{-| Parses a service channel which is given by:
    
    * first char is "=" then followed by an integer (of the channel), and ending with "\r\n" as usual
-}
pServiceCh :: A.Parser ServiceCh
pServiceCh = fmap (coerce @Int @ServiceCh) $  A.char '=' *> A.signed A.decimal <* A.endOfLine


literallyunusable = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addrinf <- fmap head $ getAddrInfo (Just hints) 
            (Just $ "localhost") 
            (Just $ "6969")
    liftIO $ bracket 
        -- open the socket resource
        (open addrinf)
        -- run the main application
        loop
        -- close the socket (library call)
        close
  where
    -- opens the socket with sane defaults (standard C way of opening a socket translated to Haskell)
    open addrinf = do
        s <- socket 
            (addrFamily addrinf) 
            (addrSocketType addrinf) 
            (addrProtocol addrinf)
        setSocketOption s ReuseAddr 1
        withFdSocket s setCloseOnExecIfNeeded
        bind s (addrAddress addrinf)
        listen s 1024
        return s

    -- the main ``loop'' of the program.
    loop s = forever $ do
        (s', _) <- accept s
        forkFinally (hah s') $ \err -> close s' >> case err of
            Left e -> throwIO e
            Right () -> return () 

    hah s = do
        ~(Just egch, ps) <- P.runStateT (PA.parse pServiceCh) (recvPipe s) 
        print egch
        runEffect $ ps >-> P.print
        runEffect $ ps >-> P.print

