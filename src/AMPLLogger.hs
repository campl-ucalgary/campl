module AMPLLogger where

import AMPLTypes
import AMPLEnv

import Data.Time
import Data.List
import System.IO
import Control.Monad.Reader
import Control.Concurrent 

import qualified Data.Map as Map
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

{-
    Small / simple thread safe logger for AMPL
-}
data Logger = Logger {
        loggerLock :: MVar ()
        , logTo :: (FilePath, Handle)
    }

initLogger :: 
    FilePath ->     -- ^ directory to create the file..
    String ->       -- ^ name of the file. e.g. foo.ext will become fooXXX.ext
    IO Logger
initLogger filepath filename = do
    (fp, h) <- openTempFile filepath filename
    m <- newMVar ()
    putStrLn ("Logging to file: " ++ fp)
    return $ Logger m (fp, h)

closeLogger :: Logger -> IO ()
closeLogger Logger{ loggerLock = mvar, logTo = lgto } =
    withMVar mvar (const (hClose (snd lgto)))

fileLogger :: 
    Logger -> 
    String ->       -- ^ String to log
    IO ()
fileLogger Logger{ loggerLock = mvar, logTo = lgto } str =
    withMVar mvar (const (hPutStr (snd lgto) str))

logProcess :: HasLog r => Stec -> ReaderT r IO ()
logProcess (s,t,e,c) = do
    env <- ask
    thid <- liftIO myThreadId
    tme <- liftIO getCurrentTime
    liftIO $ getLog env
        (intercalate "\n" 
            [ dashes
            , show tme
            , "Process on: " ++ show thid
            , "Stack:"
            , render (doc s)
            , "Translations:"
            , render (doc t)
            , "Environment:"
            , render (doc e)
            , "Code:"
            , render (doc c)
            , dashes
            ]
        )

logChm :: HasLog r => Chm -> ReaderT r IO ()
logChm chm = do
    env <- ask
    tme <- liftIO getCurrentTime
    liftIO $ getLog env
        (intercalate "\n" $
            [ dashes
            , show tme
            , "Channel Manager: "]
            ++ map show (Map.toList chm)
            ++ [ dashes ]
        )

dashes :: String
dashes = replicate 20 '-'
