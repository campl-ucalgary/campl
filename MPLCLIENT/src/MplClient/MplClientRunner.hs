module MplClient.MplClientRunner where

import Optics

import MplMach.MplMachServices
import MplClient.Flags
import MplClient.MplClientStack

import Network.Socket
import Network.Socket.ByteString

import Control.Exception
import Control.Monad.IO.Class

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
mplClient sock = undefined

