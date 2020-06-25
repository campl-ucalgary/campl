module AMPLASMMain where

import AMPLAssemble
import AMPLServices
import AMPLTypes

import Data.Stream (Stream)
import qualified Data.Stream as Stream

import Text.PrettyPrint

import Data.List
import System.Random
import System.Exit
import System.Environment
import System.IO

data AmplAssembleConfig = AmplAssembleConfig {
        filetocompile :: String
        , outputfile :: Maybe String
    }

amplAssembleMain :: 
    AmplAssembleConfig ->
    IO ()
amplAssembleMain AmplAssembleConfig{ filetocompile = filename, outputfile = outputfile} = do
    prg <- readFile filename
    servicestate <- defaultServiceState
    let eithercode = amplAssemble servicestate prg
    case eithercode of
        Right mach -> do
            handle <- maybe (pure stdout) (`openFile` ReadWriteMode) outputfile
            -- hPutStr handle $ render $ doc $ mach
            hPutStr handle $ show mach
            hClose handle
        Left errs -> do
            hPutStrLn stderr (pprintAmplAssemblerErrors errs) 
            exitFailure


defaultServiceState :: IO ServiceState
defaultServiceState = do    
    keystream <- defaultKeyStream
    return $ ServiceState {
            keyStream = keystream
            , serviceGlobalChanIdGen = Stream.iterate pred (GlobalChanID 0)
            , internalGlobalChanIdGen = Stream.iterate succ (GlobalChanID 1)
            , terminalNetworkedCommand = (\(Key str) -> "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k " ++ str ++ "  ; read'")
        }

defaultKeyStream :: IO (Stream Key)
defaultKeyStream = do
    stdgen <- newStdGen
    let keys = map (Key . show) $ nub $ (randoms stdgen :: [Word])
    return $ Stream.fromList keys
