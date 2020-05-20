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
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception
import System.Environment

codataMain = 
    [ iConstInt 2
        , iRec [[iConstInt 1, iAccess 0, iAddInt, iRet]         -- 0
                , [iConstInt 100, iAccess 0, iAddInt, iRet]     -- 1
                , [iConstInt 200, iAccess 0, iAddInt, iRet]     -- 2
                ]
        , iDest 2 1
        ]
sequentialCodataTest = execAMPLMachWithDefaultLogger 
    (codataMain, [])
    ([], (Services Set.empty Set.empty, Map.empty, Stream.iterate succ 1))


plugMain = (
    [ iPlug 
        [LocalChanID 3] 
        (
            ([LocalChanID 1], 
                [iConstInt 3, iStore, iAccess 0, iPut (LocalChanID 3)] )
                -- Puts ConstInt 3 on top of the stack
                -- Stores it to the environment
                -- Accesses it so it goes back on the stack
                -- Since 3 is on the top of the stack, using the shared channel, send it off to the channel manager
            , ([LocalChanID 2], 
                [iGet (LocalChanID 3)] )
        )
    ]
    ,   -- translations
        [ (Input,  (LocalChanID 1, GlobalChanID 1))
        , (Input,  (LocalChanID 2, GlobalChanID 2))
        ] 
    )
simplePlugTest = execAMPLMachWithDefaultLogger 
    plugMain
    ([], 
        ( Services Set.empty Set.empty
        , mkChm 
            [ (GlobalChanID 1, emptyQInstrQueues)
            , (GlobalChanID 2, emptyQInstrQueues)
            ]
        , Stream.iterate succ 3)
        )

testchm = 
        mkChm 
            [ (GlobalChanID 1, emptyQInstrQueues)
            , (GlobalChanID 2, emptyQInstrQueues)
            ]



main :: IO ()
main = do
    putStrLn "hello world"
