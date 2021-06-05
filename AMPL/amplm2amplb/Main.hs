module Main where


main = undefined

{-
import Data.Array
import Data.List
import Data.Function

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Bits

import AMPL
import AMPLSequential 
import AMPLConcurrent
import AMPLTypes
import AMPLServices
import AMPLEnv
import AMPLMach
import AMPLLogger

import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Data.Queue (Queue)
import qualified Data.Queue as Queue


import ServiceConstants
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import System.Environment
import System.IO

import Data.Word
import Data.Int

import Bytecode

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> do
            initmachstate <- read <$> readFile input
            putStr $ "init machine state: "
            putStrLn $ show initmachstate
            printInitMachineStateToBytecodeFile output initmachstate
        _ -> error "Usage: amplm2amplb <input> <output>"
        -}
