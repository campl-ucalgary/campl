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

-------------------------
-- Codata test
-------------------------
{-
    Small sequential co data test
-}
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


-------------------------
-- PLUG test
-------------------------
{-
    Plugs two processes together. Sends the number 3 over channel 3...
-}
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
-------------------------
-- parallel or test...
-------------------------
{-
    A parallel or program would look something like this -- (note that since
    services have not been written at the time we implemented this, so
    we just write services as serviceN)

    -- a is channel 1
    -- b is channel 2
    -- c is channel 3

    paralelOrMain:
    plug a b
        p1 | service0 => a,b
        p2 | a,b => service1
    
    proc p1
        | _  => a,b -> do
            -- this part can be rearranged
            put False on a
            put True on b
            close a
            close b
    proc p2
        | a,b => c -> do
            race
                a -> 
                    get va from a
                    if va
                        then
                            put va on c
                            get _ from b
                            close a,b
                            end c
                        else
                            get vb on b
                            put (va || vb) on c
                            close a,b
                            end c
                b -> get vb from b
                    if vb 
                        then
                            put vb on c
                            get _ from a
                            close a,b
                            end c
                        else
                            get va on a
                            put (va || vb) on c
                            close a,b
                            end c 
-}

p1 = [ 
    iConstBool False
    , iPut (LocalChanID 1)  
    , iConstBool True
    , iPut (LocalChanID 2)  
    , iClose (LocalChanID 1)
    , iClose (LocalChanID 2)
    ]
p2 = [ iRace [ (LocalChanID 1, a), (LocalChanID 2, b) ] ]
  where
    a = [
        iGet (LocalChanID 1)
        , iStore
        , iAccess 0
        , iIf [
                iAccess 0
                , iPut (LocalChanID 3)
                , iGet (LocalChanID 2)
                , iStore
                , iAccess 0
                , iClose (LocalChanID 1)
                , iClose (LocalChanID 2) 
                -- end c?
                , iRet
                ] 
              [
                iGet (LocalChanID 2)
                , iStore
                , iAccess 0
                , iAccess 1
                , iOrBool
                , iStore
                , iAccess 0
                , iAccess 0             -- makes the if statement happy
                , iPut (LocalChanID 3)
                , iClose (LocalChanID 1)
                , iClose (LocalChanID 2) 
                -- end c?
                , iRet
                ]
        ]
    b = [
        iGet (LocalChanID 2)
        , iStore
        , iAccess 0
        , iIf [
                iAccess 0
                , iPut (LocalChanID 3)
                , iGet (LocalChanID 1)
                , iStore
                , iAccess 0
                , iClose (LocalChanID 1)
                , iClose (LocalChanID 2) 
                -- end c?
                , iRet
                ] 
              [
                iGet (LocalChanID 1)
                , iStore
                , iAccess 0
                , iAccess 1
                , iOrBool
                , iStore
                , iAccess 0
                , iAccess 0             -- this other access is here to make the if statement and return happy.
                , iPut (LocalChanID 3)
                , iClose (LocalChanID 1)
                , iClose (LocalChanID 2) 
                -- end c?
                , iRet
                ]
        ]

paralelOrMain = (
    [ iPlug 
        [LocalChanID 1, LocalChanID 2 ] 
        (
            ([LocalChanID 0], 
                p1 )
            , ([LocalChanID 3], 
                p2 )
        )
    ]
    ,   -- translations
        [ (Input,  (LocalChanID 0, GlobalChanID 0))
        , (Input,  (LocalChanID 3, GlobalChanID 3))
        ] 
    )
paralelOrExec = execAMPLMachWithDefaultLogger 
    paralelOrMain
    ([], 
        ( Services Set.empty Set.empty
        , mkChm 
            [ (GlobalChanID 0, emptyQInstrQueues)
            , (GlobalChanID 3, emptyQInstrQueues)
            ]
        , Stream.iterate succ 4)
        )


main :: IO ()
main = do
    paralelOrExec
