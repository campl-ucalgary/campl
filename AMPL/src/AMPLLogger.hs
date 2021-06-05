module AMPLLogger where

import AMPLTypes

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
data AmplLogger = AmplLogger {
        stdLock :: MVar ()                              -- lock for standard input and output
        , fileLogger :: (FilePath, Handle, MVar String) -- for logging to files.. 
    }

-- | intialize the ampl logger (mainly used to create the file..)
initAmplLogger :: 
    -- | directory to create the file..
    FilePath ->     
    -- | name of the file. e.g. foo.ext will become fooXXX.ext
    String ->       
    IO AmplLogger
initAmplLogger filepath filename = do
    (fp, h) <- openTempFile filepath filename
    lglck <- newMVar ()
    m <- newMVar ""
    putStrLn ("Logging to file: " ++ fp)
    return $ AmplLogger lglck (fp, h, m)

closeAmplLoggerWithTimeStamp :: AmplLogger -> IO ()
closeAmplLoggerWithTimeStamp lger@AmplLogger{ stdLock = stdlk, fileLogger = (fp, h, flk) } = do
    tme <- getCurrentTime
    nonRedundantFileAmplLogger lger dashesTimeStampLn (return dashesLn) ""
    withMVar flk (const (hClose h))

-- | Wrapper to log things to stdout
amplLogStdOut :: 
    AmplLogger ->
    String ->
    IO ()
amplLogStdOut AmplLogger{  stdLock = stdlk, fileLogger = (fp, h, flk) } str = 
    withMVar stdlk (const (putStrLn str))

-- | This logs only the different strings....
nonRedundantFileAmplLogger :: 
    AmplLogger -> 
    -- | Before log string
    IO String ->       
    -- | After log string
    IO String ->       
    -- | String to log
    String ->          
    IO ()
nonRedundantFileAmplLogger AmplLogger{  stdLock = stdlk, fileLogger = (fp, h, flk) } before after str =
    modifyMVar flk f
  where
    f s = if str == s
            then return (s, ())
            else do
                b <- before
                a <- after
                hPutStr h (b ++ str ++ a)
                return (str, ())

timeStampLn :: IO String
timeStampLn = (++"\n"). show <$> getCurrentTime

dashesLn :: String
dashesLn = '\n':dashes ++"\n"

dashesTimeStampLn :: IO String
dashesTimeStampLn = (dashesLn++) <$> timeStampLn 

dashes :: String
dashes = replicate 20 '-'
