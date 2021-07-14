{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module MplMach.MplMachServices where

import Optics

import Pipes
import Pipes.Core 
import qualified Pipes.Parse as P
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Prelude as P

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A

import Control.Monad.State.Strict

import Control.Exception
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent (forkFinally)

import System.IO (isEOF)
import Control.Monad
import Data.Coerce
import Text.Read

import MplMach.MplMachTypes
import MplMach.MplMachStack
import MplMach.MplMachException
import MplMach.MplMachTypes

import Data.IORef
import Data.Maybe

import Network.Socket
import Network.Socket.ByteString

import qualified Text.Show.Pretty as PrettyShow

import Debug.Trace


data SNInstr
    = SNInt Int
    | SNChar Char
    | SNString String

    | SNGetInt
    | SNPutInt 

    | SNGetChar
    | SNPutChar

    | SNGetString
    | SNPutString

    | SNClose
  deriving Show


serviceManager :: 
    HasMplMachServicesEnv r =>
    Socket ->
    MplMach r ()
serviceManager s = forever $ gview equality >>= \env -> liftIO $ do
    (s', _) <- accept s
    forkFinally (flip runMplMach env $ serviceClient s') $ \err -> close s' >> case err of
        -- oops, shoudln't use 'forever' here, when it is AsyncCancelled, should probably
        -- just stop recursing.
        Right () -> return ()
        Left e -> case fromException e of
            Just AsyncCancelled -> return ()
            _ -> throwIO e

{- | Honestly, literally everything about services is a silly mess. They make the 
language frankly unusable for anything. well actually io in general is a mess.... -}
serviceClient  ::
    HasMplMachServicesEnv r =>
    Socket ->
    MplMach r ()
serviceClient s = gview equality >>= \env -> do
    ~(Just egch, pbts) <- P.runStateT (PA.parse pServiceCh) (recvPipe s) 
    svmp <- liftIO $ env ^. serviceMap % to readIORef 
    case egch of
        Right gch -> let gchlkup = fromJust $ svmp ^. at gch in 
            serviceClientLoop s gchlkup pbts (serviceQueueSInstrPipe gchlkup)
        Left err -> liftIO $ throwIO err

{- | This is honestly super confusing and literally everything is horrible about this...  Some remarks
    to clear things up...

    * Since we know that services are a protocol / coprotocol, we know the first response must
        FOR SURE be a HPut
    * Then, that HPut (with the particular index) determines what we want to do

Again, this language is literally unusable with this design of services....

Also, there's lots of trash duplicated code in this.
-}

serviceClientLoop ::
    HasMplMachServicesEnv r =>
    -- | socket
    Socket ->
    -- | Translation lookup
    TranslationLkup ->
    -- | producer for the socket
    Producer ByteString (MplMach r) () ->
    -- | producer from the instructions
    Producer SInstr (MplMach r) () ->
    -- | result
    MplMach r ()
serviceClientLoop sock gchlkup psock pinstrs = next pinstrs >>= \case
    Left () -> return ()
    Right (instr, pinstrs') -> case instr of
        -- Int instructions
        SHGetInt -> do
            -- send that we want an int
            liftIO $ sendAll sock $ snInstrToByteString SNGetInt
            -- keep looking querying until we actually parse an int
            (psock', inp) <- loop psock

            -- add it to the queue
            fetchAndWriteChMQueue (gchlkup ^. activeQueue) $ QPut $ VInt inp

            serviceClientLoop sock gchlkup psock' pinstrs'
          where
            loop psock = do
                ~(mval, psock') <- P.runStateT (PA.parse pSNInt) psock 
                case mval of
                    Just (Right val) -> return (psock', val)
                    _ -> loop psock'
        SHPutInt -> do

            v <- liftIO $ atomically $ 
                gchlkup ^. otherQueue % to readChMQueue
                    >>= readTQueue
                    >>= \case
                        QPut (VInt v) -> return v
                        bad -> throwSTM $ ppShowIllegalStep bad

            -- send we want to put an int
            liftIO $ sendAll sock $ snInstrToByteString $ SNPutInt

            -- send actually put the int
            liftIO $ sendAll sock $ snInstrToByteString $ SNInt v
                
            serviceClientLoop sock gchlkup psock pinstrs'

        -- Char instructions
        SHGetChar -> do
            -- send that we want an int
            liftIO $ sendAll sock $ snInstrToByteString SNGetChar
            -- keep looking querying until we actually parse an int
            (psock', inp) <- loop psock

            -- add it to the queue
            fetchAndWriteChMQueue (gchlkup ^. activeQueue) $ QPut $ VChar inp

            serviceClientLoop sock gchlkup psock' pinstrs'
          where
            loop psock = do
                ~(mval, psock') <- P.runStateT (PA.parse pSNChar) psock 
                case mval of
                    Just (Right val) -> return (psock', val)
                    _ -> loop psock'
        SHPutChar -> do

            v <- liftIO $ atomically $ 
                gchlkup ^. otherQueue % to readChMQueue
                    >>= readTQueue
                    >>= \case
                        QPut (VChar v) -> return v
                        bad -> throwSTM $ ppShowIllegalStep bad

            -- send we want to put an int
            liftIO $ sendAll sock $ snInstrToByteString $ SNPutChar

            -- send actually put the int
            liftIO $ sendAll sock $ snInstrToByteString $ SNChar v
                
            serviceClientLoop sock gchlkup psock pinstrs'

        -- String instructions
        SHGetString -> do
            -- send that we want an int
            liftIO $ sendAll sock $ snInstrToByteString SNGetString
            -- keep looking querying until we actually parse a string
            (psock', inp) <- loop psock

            -- add it to the queue
            fetchAndWriteChMQueue (gchlkup ^. activeQueue) $ QPut $ strToVal inp

            serviceClientLoop sock gchlkup psock' pinstrs'
          where
            loop psock = do
                ~(mval, psock') <- P.runStateT (PA.parse pSNString) psock 
                case mval of
                    Just (Right val) -> return (psock', val)
                    _ -> loop psock'

        SHPutString -> do

            v <- liftIO $ atomically $ 
                gchlkup ^. otherQueue % to readChMQueue
                    >>= readTQueue
                    >>= \case
                        QPut v -> return $ valToStr v
                        bad -> throwSTM $ ppShowIllegalStep bad

            -- send we want to put an int
            liftIO $ sendAll sock $ snInstrToByteString $ SNPutString

            -- send actually put the int
            liftIO $ sendAll sock $ snInstrToByteString $ SNString v
                
            serviceClientLoop sock gchlkup psock pinstrs'

        SHClose -> do
            liftIO $ sendAll sock $ snInstrToByteString SNClose
            return ()

serviceQueueSInstrPipe ::
    ( HasMplMachServicesEnv r ) =>
    TranslationLkup ->
    Producer SInstr (MplMach r) ()
serviceQueueSInstrPipe gchlkup = go
  where
    -- go = forever $ join $ fmap yield $ liftIO $ atomically $ gchlkup ^. otherQueue % to readChMQueue 
    go = forever $ join $ fmap yield $ liftIO $ atomically $ do
        chotherqueue <- gchlkup ^. otherQueue % to readChMQueue 
        peekTQueue chotherqueue >>= \case
            QSHPut sinstr -> readTQueue chotherqueue >> return sinstr
            _ -> retry
            
recvPipe :: 
    ( MonadIO m ) =>
    Socket ->
    Producer ByteString m ()
recvPipe s = go 
  where
    go = do
        -- urgh, for some reason this isn't blocking if it doesn't receive anything
        -- so we thread delay so it doesn't needlessy spin the cpu...
        res <- liftIO $ recv s rECV_MAX_BYTES_TO_RECEIVE <* threadDelay 10000
        unless (B.null res) $ yield res 
        go

    rECV_MAX_BYTES_TO_RECEIVE :: Int
    rECV_MAX_BYTES_TO_RECEIVE = 4096

{-| Rougly follows the idea here: https://redis.io/topics/protocol. The specification is as follows.

    * Integers the first byte of the reply is ":"

    * Chars the first byte of the reply is ";"

    * Strings the first byte of the reply is "?", then the length @n@ (as an int), then @n@ characters.

    * Errors the first byte of the reply is "-" 

    * Simple strings the first byte of the reply is "+" followed by a string that cannot contain (CRLF)
        (this is used for sending commands e.g. a close comand)

    * We do not support parsing any other values (unclear with how to do this anyways).

    * Different parts of the protocol are always terminted wth "\r\n" (CRLF)
-}
pVal :: A.Parser Val
pVal = undefined

{-| ":<SOMEINT>\r\n" -}
pSNInt :: A.Parser Int
pSNInt = A.char ':' *> A.signed A.decimal <* A.endOfLine

{-| ";<SOMECHAR>\r\n" -}
pSNChar :: A.Parser Char
pSNChar = A.char ';' *> A.anyChar <* A.endOfLine

{-| "?<LENGTH>\r\n<SOMESTRINGOFLENGTH>\r\n" -}
pSNString :: A.Parser String
pSNString = do 
    _ <- A.char '?' 
    (n :: Int) <- A.decimal 
    _ <- A.endOfLine 
    str <- A.take n 
    _ <- A.endOfLine 
    return $ B.unpack str

pSNCmd :: A.Parser SNInstr
pSNCmd = 
    A.char '+' *> 
    A.choice 
        [ SNGetChar <$ A.string "GETCHAR"
        , SNPutChar <$ A.string "PUTCHAR"

        , SNGetInt <$ A.string "GETINT"
        , SNPutInt <$ A.string "PUTINT"

        , SNGetString <$ A.string "GETSTR"
        , SNPutString <$ A.string "PUTSTR"

        , SNClose <$ A.string "CLOSE"
        ]
    <* A.endOfLine

snInstrToByteString :: 
    SNInstr ->
    ByteString
snInstrToByteString = \case
    SNInt n -> ':' `B.cons` (B.pack (show n) `B.append` eoc)
    SNChar n -> ';' `B.cons` (B.pack [n] `B.append` eoc)
    SNString n -> '?' `B.cons` (B.pack (show $ length n) `B.append` eoc `B.append` B.pack n `B.append` eoc)

    SNGetChar -> '+' `B.cons` ("GETCHAR" `B.append` eoc)
    SNPutChar -> '+' `B.cons` ("PUTCHAR" `B.append` eoc)

    SNGetInt -> '+' `B.cons` ("GETINT" `B.append` eoc)
    SNPutInt -> '+' `B.cons` ("PUTINT" `B.append` eoc)

    SNGetString -> '+' `B.cons` ("GETSTR" `B.append` eoc)
    SNPutString -> '+' `B.cons` ("PUTSTR" `B.append` eoc)

    SNClose -> '+' `B.cons` ("CLOSE" `B.append` eoc)
  where
    eoc = "\r\n"

serviceChToByteString :: 
    ServiceCh ->
    ByteString
serviceChToByteString ch = '=' `B.cons` (B.pack (show (coerce @ServiceCh @Int ch)) `B.append` "\r\n")


{-| Parses a service channel which is given by:
    
    * first char is "=" then followed by an integer (of the channel), and ending with "\r\n" as usual
-}
pServiceCh :: A.Parser ServiceCh
pServiceCh = fmap (coerce @Int @ServiceCh) 
    $  A.char '=' *> A.signed A.decimal <* A.endOfLine


-- * service loop for a local thread
-- | this runs a service for a local thread
serviceThread ::
    TranslationLkup ->
    MplMach MplMachEnv ()
serviceThread chlkup = loop 
  where
    loop = gview equality >>= \env -> do
        sinstr <- liftIO $ atomically $ do 
            chotherqueue <- chlkup ^. otherQueue % to readChMQueue
            peekTQueue chotherqueue >>= \case
                QSHPut sinstr -> readTQueue chotherqueue >> return sinstr
                _ -> retry 
        case sinstr of
            -- int instructions 
            SHGetInt -> do 
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Please enter an int: "
                    let inputloop = fmap readMaybe getLine 
                            >>= \case 
                                Just n ->  return (n :: Int)
                                Nothing ->  inputloop
                    n <- inputloop
                    fetchAndWriteChMQueue 
                        (chlkup ^. activeQueue) 
                        (QPut (VInt n))
                loop
            SHPutInt -> do 
                ~(QPut (VInt n)) <- liftIO $ atomically $ 
                    chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Putting int: "
                    putStrLn $ show n
                loop

            -- char instructions 
            SHGetChar -> do 
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Please enter an char: "
                    let inputloop = getLine 
                            >>= \case 
                                [c] ->  return c
                                _ ->  inputloop
                    n <- inputloop
                    fetchAndWriteChMQueue 
                        (chlkup ^. activeQueue) 
                        (QPut (VChar n))
                loop
            SHPutChar -> do 
                ~(QPut (VChar n)) <- liftIO $ atomically $ 
                    chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    putStrLn "Putting char: "
                    putStrLn [n]
                loop

            -- string instructions 
            SHGetString -> do 
                gview stdLock >>= \mvar -> liftIO $ withMVar mvar $ const $ do
                    str <- getLine
                    fetchAndWriteChMQueue 
                        (chlkup ^. activeQueue) 
                        (QPut (strToVal str))
                loop
            SHPutString -> do
                ~(QPut inp) <- liftIO $ atomically $ 
                    chlkup ^. otherQueue % to (readTQueue <=< readChMQueue)
                gview stdLock >>= \mvar -> liftIO 
                    $ withMVar mvar $ const $ putStrLn $ valToStr inp
                loop




            SHClose -> return ()
