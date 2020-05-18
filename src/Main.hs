module Main where

import AMPL
import AMPLSequential
import AMPLConcurrent
import AMPLTypes
import AMPLEnv

import Control.Monad.Reader
import Data.Map (Map (..))
import qualified Data.Map as Map

codataTestSequential 
    = [ iConstInt 2
        , iRec [[iConstInt 1, iAccess 0, iAddInt, iRet]         -- 0
                , [iConstInt 100, iAccess 0, iAddInt, iRet]     -- 1
                , [iConstInt 200, iAccess 0, iAddInt, iRet]     -- 2
                ]
        , iDest 2 1
        ]

{-
runProgramWithEnv :: 
    ReaderT AmplEnv IO ( ([Instr], [Val], [Val]), [([Instr],[Val], [Val])]) -> 
    IO ( ([Instr], [Val], [Val]), [([Instr],[Val], [Val])])
runProgramWithEnv reader = 
    amplEnv [] putStrLn Map.empty undefined >>= runReaderT reader 

printSteps :: 
    IO (([Instr], [Val], [Val]), [([Instr],[Val], [Val])]) ->
    IO ()
printSteps a = 
    do 
        (a', as) <- a
        print a'
        mapM_ print as
        -}

main :: IO ()
main = do
    putStrLn "hello world"
