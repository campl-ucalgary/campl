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
-- Run example....
-------------------------
{-
Example using run with services to print the output...
This will open a service, get a character from the service and
 - if the character is 't', it will print 't' on the stdout
 - otherwise, it will print 'f' on the stdout
Note that this uses a bool to communciate between the two internal channels..

runExampleMain:
plug a b
    p1 | service1 => a
    p2 | a => service0

-- service1 is some external char service...
-- service0 is std stdout

function chartfToBool(a :: char)
    if a == 't' 
        then return True
        else return False

function boolToChartf(a :: Bool)
    if a 
        then return 't'
        else return 'f'

proc p1run
    | service1  => a -> do
        -- this part can be rearranged
        hput CharGet on service1
        get x on servce1
        let x = boolToChartf(x)
        put x on a
        close service1
        close a
proc p2run
    | a => service0 -> do
        get x on a
        let x = boolToChartf(x)
        hput CharPut on service0
        put x on service0
        close a
        close service0 
-}

getStdCharAndRunToServiceOutMain = (
    [ iPlug 
        [LocalChanID 1] 
        ( ([LocalChanID (-1)], p1run)
        , ([LocalChanID 0], p2run)
        )
    ],
    [(Output, (LocalChanID (-1), GlobalChanID (-1))), (Input, (LocalChanID 0, GlobalChanID 0))])

p1run = [
      iHPut (LocalChanID (-1)) (HCaseIx hCaseIxGet)
    , iGet (LocalChanID (-1))
    , iStore
    , iAccess 0
    , iCall (FunID 0) 1
    , iStore
    , iAccess 0
    , iPut (LocalChanID 1)
    , iHPut (LocalChanID (-1)) (HCaseIx hCaseIxClose)
    , iClose (LocalChanID 1)
    ]
p2run = [
      iGet (LocalChanID 1)
    , iStore
    , iAccess 0
    , iCall (FunID 1) 1
    , iStore
    , iAccess 0
    , iHPut (LocalChanID 0) (HCaseIx hCaseIxPut)
    , iPut (LocalChanID 0)
    , iHPut (LocalChanID 0) (HCaseIx hCaseIxClose)
    , iClose (LocalChanID 1)
    ]

chartfToBool = [
    iAccess 0             
    , iConst (VChar 't')    -- put a 't' on the stack...
    , iEqChar
    , iStore                  -- 
    , iAccess 0
    , iRet
    ]

boolToChartf = [
    iAccess 0            
    , iIf 
        [ iConst (VChar 't')
        , iRet
        ]
        [ iConst (VChar 'f')
        , iRet
        ]
    , iRet
    ]

getStdCharAndRunToServiceOutFunDefs = [ (FunID 0, ("chartfToBool", chartfToBool)), (FunID 1, ("boolToChartf",boolToChartf)) ]

getStdCharAndRunToServiceOutTest = do
    svs <- genServicesChmAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (CharService, TerminalNetworkedService "xterm -e ' /home/jared/.local/bin/amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID 0, (CharService, StdService) )
                    ]
    execAmplMachWithDefaults 
        getStdCharAndRunToServiceOutMain    
        getStdCharAndRunToServiceOutFunDefs
        testAmplTCPServer
        svs



-------------------------
-- Parallel or with services...
-------------------------
{-

    -- service1 and service2 are external char services..
    -- service0 is stdout

    parallelOrServiceMain:
    plug a, b
        p1ServiceOr | service1, service2 => a, b
        p2ServiceOr | a,b => service0
    -- actually wasn't too sure about how to write this out....

    proc p1ServiceOr
        | service1, service2  => a,b -> do
            plug []
                p11ServiceOr | service1 => a
                p12ServiceOr | service2 => b
                -- Type mismatch?

    proc p11ServiceOr
        | service1 => a -> do
            hPut GetChar service1
            get x on service1
            let x = chartfToBool(x)
            put x on a
            close a
            hPut CloseService service1
    proc p12ServiceOr
        | service2 => b -> do
            hPut GetChar service2
            get x on service2
            let x = chartfToBool(x)
            put x on b
            close b
            hPut CloseService service2

    proc p2ServiceOr
        | a,b => service0 -> do
            race
                a -> 
                    get va from a
                    if va
                        then
                            put va on service0
                            get _ from b
                            close a,b
                            end service0
                        else
                            get vb on b
                            put (va || vb) on service0
                            close a,b
                            end service0
                b -> get vb from b
                    if vb 
                        then
                            put vb on service0
                            get _ from a
                            close a,b
                            end service0
                        else
                            get va on a
                            put (va || vb) on service0
                            close a,b
                            end service0 
-}

parallelOrServiceMain = 
    (
        [
            iPlug [LocalChanID aPOr, LocalChanID bPOr] 
                (
                    ([LocalChanID service1POr, LocalChanID service2POr], p1ServiceOr)
                    , ([LocalChanID service0POr], p2ServiceOr)
                )
            
        ]
    ,
    [ (Output, (LocalChanID service0POr, GlobalChanID service0POr)) 
    , (Input, (LocalChanID service1POr, GlobalChanID service1POr)) 
    , (Input, (LocalChanID service2POr, GlobalChanID service2POr)) 
    ]
    )

{-
proc p1ServiceOr
        | service1, service2  => a,b -> do
            plug []
                p11ServiceOr | service1 => a
                p12ServiceOr | service2 => b
                -- Type mismatch?
-}
p1ServiceOr = 
    [ 
        iPlug []
        (([LocalChanID service1POr, LocalChanID aPOr], p11ServiceOr)
        ,([LocalChanID service2POr, LocalChanID bPOr], p12ServiceOr))
    ]

{-
    proc p11ServiceOr
        | service1 => a -> do
            hPut GetChar service1
            get x on service1
            let x = chartfToBool(x)
            put x on a
            close a
            hPut CloseService service1
    proc p12ServiceOr
        | service2 => b -> do
            hPut GetChar service2
            get x on service2
            let x = chartfToBool(x)
            put x on b
            close b
            hPut CloseService service2
-}

p11ServiceOr = 
    [ iHPut (LocalChanID service1POr) (HCaseIx hCaseIxGet)
    , iGet (LocalChanID service1POr)
    , iCall (FunID 0) 1 
    , iPut (LocalChanID aPOr)
    , iClose (LocalChanID aPOr)
    , iHPut (LocalChanID service1POr) (HCaseIx hCaseIxClose)
    ]

p12ServiceOr = 
    [ iHPut (LocalChanID service2POr) (HCaseIx hCaseIxGet)
    , iGet (LocalChanID service2POr)
    , iCall (FunID 0) 1 
    , iPut (LocalChanID bPOr)
    , iClose (LocalChanID bPOr)
    , iHPut (LocalChanID service2POr) (HCaseIx hCaseIxClose)
    ]

{-
proc p2ServiceOr
    | a,b => service0 -> do
        race
            a -> 
                get va from a
                if va
                    then
                        put va on service0
                        get _ from b
                        close a,b
                        end service0
                    else
                        get vb on b
                        put (va || vb) on service0
                        close a,b
                        end service0
            b -> get vb from b
                if vb 
                    then
                        put vb on service0
                        get _ from a
                        close a,b
                        end service0
                    else
                        get va on a
                        put (va || vb) on service0
                        close a,b
                        end service0 
-}

p2ServiceOr = 
    [ iRace [(LocalChanID aPOr, racea), (LocalChanID bPOr, raceb) ]
    ]
  where
    racea = 
        [ iGet (LocalChanID aPOr)
        , iStore
        , iAccess 0
        , iIf
            [ iAccess 0
            , iCall (FunID 1) 1
            , iStore
            , iAccess 0
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxPut)
            , iPut (LocalChanID service0POr)
            , iGet (LocalChanID bPOr)
            , iClose (LocalChanID aPOr)
            , iClose (LocalChanID bPOr)
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxClose)
            , iRet
            ]
            [ iAccess 0
            , iGet (LocalChanID bPOr)
            , iStore
            , iAccess 0
            , iCall (FunID 1) 1
            , iStore
            , iAccess 0
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxPut)
            , iPut (LocalChanID service0POr)
            , iClose (LocalChanID aPOr)
            , iClose (LocalChanID bPOr)
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxClose)
            , iRet
            ]
        ]
    raceb = 
        [ iGet (LocalChanID bPOr)
        , iStore
        , iAccess 0
        , iIf
            [ iAccess 0
            , iCall (FunID 1) 1
            , iStore
            , iAccess 0
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxPut)
            , iPut (LocalChanID service0POr)
            , iGet (LocalChanID aPOr)
            , iClose (LocalChanID bPOr)
            , iClose (LocalChanID aPOr)
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxClose)
            , iRet
            ]
            [ iAccess 0
            , iGet (LocalChanID aPOr)
            , iStore
            , iAccess 0
            , iCall (FunID 1) 1
            , iStore
            , iAccess 0
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxPut)
            , iPut (LocalChanID service0POr)
            , iClose (LocalChanID bPOr)
            , iClose (LocalChanID aPOr)
            , iHPut (LocalChanID service0POr) (HCaseIx hCaseIxClose)
            , iRet
            ]
        ]


service1POr = -1
service2POr = -2
service0POr = 0

aPOr = 1
bPOr = 2


parallelServiceOrTest = do
    svs <- genServicesChmAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (CharService, TerminalNetworkedService "xterm -e '/home/jared/.local/bin/amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID (-2), (CharService, TerminalNetworkedService "xterm -e '/home/jared/.local/bin/amplc -hn 127.0.0.1 -p 5000 -k c-2  ; read'"  (Key "c-2")))
                    , (GlobalChanID 0, (CharService, StdService) )
                    ]
    execAmplMachWithDefaults 
        parallelOrServiceMain
        getStdCharAndRunToServiceOutFunDefs         -- use the same function definitions from above..
        testAmplTCPServer
        svs


