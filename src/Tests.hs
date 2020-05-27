module Tests where

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
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import System.Environment

import Network.Socket

testAmplTCPServer = "5000"

-------------------------
-- Codata test
-------------------------
{-
    Small sequential co data test
-}
codataMain = 
    [ iConst (VInt 2)
        , iRec [[iConst (VInt 1), iAccess 0, iAddInt, iRet]         -- 0
                , [iConst (VInt 100), iAccess 0, iAddInt, iRet]     -- 1
                , [iConst (VInt 200), iAccess 0, iAddInt, iRet]     -- 2
                ]
        , iDest 2 1
        ]
sequentialCodataTest = do
    svs <- genServicesChmAndStream [] []
    execAmplMachWithDefaults 
        (codataMain, [])
        []
        testAmplTCPServer
        svs

-------------------------
-- Codata test / print to stdout
-------------------------
{-
    Small sequential co data test, but prints the result to std out..
-}
codataMainStdOut = codataMain ++
    [ iHPut (LocalChanID 0) (HCaseIx hCaseIxPut)
     , iPut (LocalChanID 0) 
     , iHPut (LocalChanID 0) (HCaseIx hCaseIxClose)]
sequentialCodataTestStdOut = do
    svs <- genServicesChmAndStream [] [(GlobalChanID 0, (IntService, StdService) )]
    execAmplMachWithDefaults 
        (codataMainStdOut,  [(Input, (LocalChanID 0, GlobalChanID 0))] )
        []
        testAmplTCPServer
        svs

-------------------------
-- basic standard input and output -- asks for an integer and prints it out
-------------------------
stdoutstdinmain = ( 
    [ iHPut (LocalChanID 0) (HCaseIx hCaseIxGet)
    , iGet (LocalChanID 0)
    , iStore
    , iAccess 0
    , iHPut (LocalChanID 0) (HCaseIx hCaseIxPut)
    , iPut (LocalChanID 0)
    , iHPut (LocalChanID 0) (HCaseIx hCaseIxClose)
    ]
    , [(Output, (LocalChanID 0, GlobalChanID 0))] ) 

stdoutstdinTest = do
    svs <- genServicesChmAndStream [] [(GlobalChanID 0, (IntService, StdService) )]
    execAmplMachWithDefaults 
        stdoutstdinmain
        []
        testAmplTCPServer
        svs

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
                [iConst (VInt 3), iStore, iAccess 0, iPut (LocalChanID 3)] )
                -- Puts ConstInt 3 on top of the stack
                -- Stores it to the environment
                -- Accesses it so it goes back on the stack
                -- Since 3 is on the top of the stack, using the shared channel, send it off to the channel manager
            , ([LocalChanID 2], 
                [ iGet (LocalChanID 3) ] 
                )
        )
    ]
    ,   -- translations
        [ (Input,  (LocalChanID 1, GlobalChanID 1))
        , (Input,  (LocalChanID 2, GlobalChanID 2))
        ] 
    )
simplePlugTest = do
    svs <- genServicesChmAndStream [GlobalChanID 1, GlobalChanID 2] []
    execAmplMachWithDefaults 
        plugMain
        []
        testAmplTCPServer
        svs

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
    iConst (VBool False)
    , iPut (LocalChanID 1)  
    , iConst (VBool True)
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
paralelOrExec = do
    svs <- genServicesChmAndStream [GlobalChanID 0, GlobalChanID 3] []
    execAmplMachWithDefaults 
        paralelOrMain
        []
        testAmplTCPServer
        svs

-------------------------
-- Get an integer from an external service and print on stdout
-------------------------
serviceGetIntTermAndPrintToStdOut = (
    [ iHPut (LocalChanID (-1)) (HCaseIx hCaseIxGet)
    , iGet (LocalChanID (-1))
    , iStore
    , iAccess 0
    , iHPut (LocalChanID (-1)) (HCaseIx hCaseIxClose)

    , iHPut (LocalChanID 0) (HCaseIx hCaseIxPut)
    , iPut (LocalChanID 0)
    , iHPut (LocalChanID 0) (HCaseIx hCaseIxClose)
    ],
    [(Output, (LocalChanID (-1), GlobalChanID (-1))), (Input, (LocalChanID 0, GlobalChanID 0))])
serviceGetIntTermAndPrintToStdOutTest = do
    svs <- genServicesChmAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (IntService, TerminalNetworkedService "xterm -e ' /home/jared/.local/bin/amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID 0, (IntService, StdService) )
                    ]
    execAmplMachWithDefaults 
        serviceGetIntTermAndPrintToStdOut
        []
        testAmplTCPServer
        svs

-------------------------
-- Get an integer from stdin and print it on an extern service..
-------------------------
getStdIntToServiceOut = (
    [ iHPut (LocalChanID 0) (HCaseIx hCaseIxGet)
    , iGet (LocalChanID 0)
    , iStore
    , iAccess 0
    , iHPut (LocalChanID 0) (HCaseIx hCaseIxClose)

    , iHPut (LocalChanID (-1)) (HCaseIx hCaseIxPut)
    , iPut (LocalChanID (-1))
    , iHPut (LocalChanID (-1)) (HCaseIx hCaseIxClose)
    ],
    [(Output, (LocalChanID (-1), GlobalChanID (-1))), (Input, (LocalChanID 0, GlobalChanID 0))])
getStdIntToServiceOutTest = do
    svs <- genServicesChmAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (IntService, TerminalNetworkedService "xterm -e ' /home/jared/.local/bin/amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID 0, (IntService, StdService) )
                    ]
    execAmplMachWithDefaults 
        getStdIntToServiceOut
        []
        testAmplTCPServer
        svs

-------------------------
-- Parallel or with services...
-------------------------
{-
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
parallelServiceOr = (
    [ iRace [(LocalChanID (-1), r1), (LocalChanID (-2), r2)]
    ], 

    [ (Input, (LocalChanID 0, GlobalChanID 0))
    , (Output, (a, GlobalChanID (-1)))
    , (Input, (b, GlobalChanID (-2)))
    ]
    )
  where
    r1 = [
        -- iGet a
        iHPut a (HCaseIx hCaseIxGet)
        , iGet a
        , iStore
        , iAccess 0
        , iConst (VChar 'y')
        , iEqChar
        , iIf [
                iAccess 0
                , iConst (VChar 'y')
                , iHPut a (HCaseIx hCaseIxPut)
                , iPut c
                , iHPut b (HCaseIx hCaseIxGet)
                , iGet b
                , iStore
                , iAccess 0
                , iClose a
                , iClose b 
                , iClose c
                , iRet
                ] 
              [
                iHPut b (HCaseIx hCaseIxGet)
                , iGet b
                , iStore
                , iAccess 0

                , iConst (VChar 'y')
                , iEqChar
                , iIf
                    [ iConst (VChar 'y')
                    , iHPut a (HCaseIx hCaseIxPut)
                    , iPut c
                    , iAccess 0
                    , iRet
                    ]
                    [ iConst (VChar 'n')
                    , iHPut a (HCaseIx hCaseIxPut)
                    , iPut c
                    , iAccess 0
                    , iRet
                    ]

                , iAccess 0             -- makes the if statement happy
                , iPut c
                , iClose a
                , iClose b 
                , iClose c
                , iRet
                ]
        ]
    r2 = [
        -- iGet a
        iHPut b (HCaseIx hCaseIxGet)
        , iGet b
        , iStore
        , iAccess 0
        , iConst (VChar 'y')
        , iEqChar
        , iIf [
                iAccess 0
                , iConst (VChar 'y')
                , iHPut b (HCaseIx hCaseIxPut)
                , iPut c
                , iHPut a (HCaseIx hCaseIxGet)
                , iGet a
                , iStore
                , iAccess 0
                , iClose b
                , iClose a 
                , iClose c
                , iRet
                ] 
              [
                iHPut a (HCaseIx hCaseIxGet)
                , iGet a
                , iStore
                , iAccess 0

                , iConst (VChar 'y')
                , iEqChar
                , iIf
                    [ iConst (VChar 'y')
                    , iHPut b (HCaseIx hCaseIxPut)
                    , iPut c
                    , iAccess 0
                    , iRet
                    ]
                    [ iConst (VChar 'n')
                    , iHPut b (HCaseIx hCaseIxPut)
                    , iPut c
                    , iAccess 0
                    , iRet
                    ]

                , iAccess 0             -- makes the if statement happy
                , iPut c
                , iClose a
                , iClose b 
                , iClose c
                , iRet
                ]
        ]

    a = LocalChanID (-1)
    b = LocalChanID (-2)
    c = LocalChanID 0

parallelServiceOrTest = do
    svs <- genServicesChmAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (CharService, TerminalNetworkedService "xterm -e '/home/jared/.local/bin/amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID (-2), (CharService, TerminalNetworkedService "xterm -e '/home/jared/.local/bin/amplc -hn 127.0.0.1 -p 5000 -k c-2  ; read'"  (Key "c-2")))
                    , (GlobalChanID 0, (CharService, StdService) )
                    ]
    execAmplMachWithDefaults 
        parallelServiceOr
        []
        testAmplTCPServer
        svs


