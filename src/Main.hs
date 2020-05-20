module Main where

import AMPL
import AMPLSequential
import AMPLConcurrent
import AMPLTypes
import AMPLEnv
import AMPLMach
import AMPLLogger

import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Data.Queue (Queue)
import qualified Data.Queue as Queue

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception

codataTestSequential 
    = [ iConstInt 2
        , iRec [[iConstInt 1, iAccess 0, iAddInt, iRet]         -- 0
                , [iConstInt 100, iAccess 0, iAddInt, iRet]     -- 1
                , [iConstInt 200, iAccess 0, iAddInt, iRet]     -- 2
                ]
        , iDest 2 1
        ]

execAMPLMachWithDefaultLogger :: 
    ([Instr], [Translation]) ->                         -- ^ Main function
    ([(FunID, (String, [Instr]))]                       -- ^ Function definitions..
    , Map GlobalChanID (Queue QInstr, Queue QInstr)     -- ^ channel manager..
    , Stream Word ) ->                                  -- ^ Channel name generator..
    IO ()
execAMPLMachWithDefaultLogger mainf (fdefs, chm, chmg) = 
    bracket 
        (initLogger "logs" "log.txt")
        closeLogger 
        (\lgger -> execAMPLMach mainf (fdefs, fileLogger lgger, chm, chmg))

test = execAMPLMachWithDefaultLogger 
    (codataTestSequential, [])
    ([], Map.empty, Stream.iterate succ 1)

main :: IO ()
main = do
    putStrLn "hello world"
