module Main where

import AMPL
import AMPLSequential
import AMPLConcurrent
import AMPLTypes
import AMPLServices
import AMPLEnv
import AMPLMach
import AMPLLogger
import Tests

import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Data.Queue (Queue)
import qualified Data.Queue as Queue

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import System.Environment

main :: IO ()
main = undefined
