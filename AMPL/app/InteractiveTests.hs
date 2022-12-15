module InteractiveTests where

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
{-

{-
The general structure for writing the program is as follows...
main = do
    svs <- genServicesChannelManagerAndStream 
                    [ {- Non service channels -} ] 
                    [ {- Service channels -} ]
    execAmplMachWithDefaults 
        {- (main function instructions, translations) -}
        [ {- (function index, function name, function definition) -} ]
        {- Port to run TCP server on e.g. 5000 -}
        svs
-}

-------------------------
-- Codata test
-------------------------
{-
    Small sequential co data test
    See logs to see what it is doing...
-}
codataMain = 
    [ iConst (VInt 2)
        , iRec [[iConst (VInt 1), iAccess 0, iAddInt, iRet]         -- 0
                , [iConst (VInt 100), iAccess 0, iAddInt, iRet]     -- 1
                , [iConst (VInt 200), iAccess 0, iAddInt, iRet]     -- 2
                ]
        , iDest (ConsIx 2) 1
        ]
sequentialCodataTest = do
    svs <- genServicesChannelManagerAndStream [] []
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
    svs <- genServicesChannelManagerAndStream [] [(GlobalChanID 0, (IntService, StdService) )]
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
    svs <- genServicesChannelManagerAndStream [] [(GlobalChanID 0, (IntService, StdService) )]
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
    svs <- genServicesChannelManagerAndStream [GlobalChanID 1, GlobalChanID 2] []
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
                , iAccess 0             -- this other access is here to make the if and return happy.
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
    svs <- genServicesChannelManagerAndStream [GlobalChanID 0, GlobalChanID 3] []
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
    svs <- genServicesChannelManagerAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (IntService, TerminalNetworkedService "xterm -e ' amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
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
    svs <- genServicesChannelManagerAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (IntService, TerminalNetworkedService "xterm -e ' amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID 0, (IntService, StdService) )
                    ]
    execAmplMachWithDefaults 
        getStdIntToServiceOut
        []
        testAmplTCPServer
        svs

-------------------------
-- Call example....
-------------------------
{-
Example using call with services to print the output...
This will open a service, get a character from the service and
 - if the character is 't', it will print 't' on the stdout
 - otherwise, it will print 'f' on the stdout
Note that this uses a bool to communciate between the two internal channels..

callExampleMain:
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

proc p1call
    | service1  => a -> do
        -- this part can be rearranged
        hput CharGet on service1
        get x on servce1
        let x = boolToChartf(x)
        put x on a
        close service1
        close a
proc p2call
    | a => service0 -> do
        get x on a
        let x = boolToChartf(x)
        hput CharPut on service0
        put x on service0
        close a
        close service0 
-}

getStdCharAndcallToServiceOutMain = (
    [ iPlug 
        [LocalChanID 1] 
        ( ([LocalChanID (-1)], p1call)
        , ([LocalChanID 0], p2call)
        )
    ],
    [(Output, (LocalChanID (-1), GlobalChanID (-1))), (Input, (LocalChanID 0, GlobalChanID 0))])

p1call = [
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
p2call = [
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
    , iEq
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

getStdCharAndcallToServiceOutFunDefs = [ (FunID 0, ({-"chartfToBool",-} chartfToBool)), (FunID 1, ({-"boolToChartf",-}boolToChartf)) ]

getStdCharAndcallToServiceOutTest = do
    svs <- genServicesChannelManagerAndStream 
                    [] 
                    [ 
                      (GlobalChanID (-1), (CharService, TerminalNetworkedService "xterm -e ' amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID 0, (CharService, StdService) )
                    ]
    execAmplMachWithDefaults 
        getStdCharAndcallToServiceOutMain    
        getStdCharAndcallToServiceOutFunDefs
        testAmplTCPServer
        svs



-------------------------
-- BAD Parallel or with services...
-------------------------
{-
    -- Note that this parallel or is bad! What this gets compiled down to
    -- does not type check! see the next example for a valid parallel or...
    -- When I wrote this, I forgot about the fork and split instruction.

    -- service1 and service2 are external char services..
    -- service0 is stdout

    parallelOrServiceMain:
    plug a, b
        p1ServiceOr | service1, service2 => a, b
        p2ServiceOr | a,b => service0
    -- actually wasn't too sure about how to write this out....
    -- Ideally, we want something like:
    -- plug a,b
    --     p1ServiceOr | service1 => a
    --     p2ServiceOr | service2 => b
    --     p3ServiceOr | a,b => service0
    -- But, plug only takes 2 arguments! So we have to do 
    -- some weird work around to get this to work..
        

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

parallelOrServiceTest = do
    svs <- genServicesChannelManagerAndStream 
                    [] 
                    [ 
                      (GlobalChanID service1POr, (CharService, TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID service2POr, (CharService, TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k c-2  ; read'"  (Key "c-2")))
                    , (GlobalChanID service0POr, (CharService, StdService) )
                    ]
    execAmplMachWithDefaults 
        parallelOrServiceMain
        getStdCharAndcallToServiceOutFunDefs         -- use the same function definitions from above..
        testAmplTCPServer
        svs

-------------------------
-- Parallel or with services and fork and split
-------------------------
{-
    -- This is a proper implememtation of a parallel or. using
    -- fork and split.. It reuses many of the functions in the
    -- previous improper implementation.

    -- service1 and service2 are external char services..
    -- service0 is stdout

    parallelOrServiceMain':
    plug s
        p1ServiceOr' | service1, service2 => s
        p2ServiceOr' | s => service0
        
    proc p1ServiceOr'
        | service1,service2 => s -> do
        -- this type should be tensor or par?
            fork s as
                a with service1 -> do
                    hPut GetChar service1
                    get x on service1
                    let x = chartfToBool(x)
                    put x on a
                    close a
                    hPut CloseService service1
                b with service2 -> do
                    hPut GetChar service2
                    get x on service2
                    let x = chartfToBool(x)
                    put x on b
                    close b
                    hPut CloseService service2

    proc p2ServiceOr'
        | s => service0 -> do
            split s into a b
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


{-
parallelOrServiceMain':
plug s
    p1ServiceOr' | service1, service2 => s
    p2ServiceOr' | s => service0
-}
parallelOrServiceMain' =
    (
        [ iPlug [LocalChanID sPOr]
            ( ([LocalChanID service1POr, LocalChanID service2POr], p1ServiceOr')
            , ([LocalChanID sPOr, LocalChanID service0POr], p2ServiceOr'))
        ]
    ,
        [ (Output, (LocalChanID service0POr, GlobalChanID service0POr)) 
        , (Input, (LocalChanID service1POr, GlobalChanID service1POr)) 
        , (Input, (LocalChanID service2POr, GlobalChanID service2POr)) 
        ]
    )

{-
proc p1ServiceOr'
    | service1,service2 => s -> do
        fork s as
            a with [service1] -> do
                hPut GetChar service1
                get x on service1
                let x = chartfToBool(x)
                put x on a
                close a
                hPut CloseService service1
            b with [service2] -> do
                hPut GetChar service2
                get x on service2
                let x = chartfToBool(x)
                put x on b
                close b
                hPut CloseService service2
-}
p1ServiceOr' = 
    [ iFork (LocalChanID sPOr) 
        (
            (LocalChanID aPOr, [LocalChanID service1POr], p11ServiceOr)
            , (LocalChanID bPOr, [LocalChanID service2POr], p12ServiceOr)
        )

    ]

p2ServiceOr' = iSplit (LocalChanID sPOr) (LocalChanID aPOr, LocalChanID bPOr) : p2ServiceOr

sPOr = 3

parallelOrServiceTest' = do
    svs <- genServicesChannelManagerAndStream 
                    [] 
                    [ 
                      (GlobalChanID service1POr, (CharService, TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k c-1  ; read'"  (Key "c-1")))
                    , (GlobalChanID service2POr, (CharService, TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k c-2  ; read'"  (Key "c-2")))
                    , (GlobalChanID service0POr, (CharService, StdService) )
                    ]
    execAmplMachWithDefaults 
        parallelOrServiceMain'
        getStdCharAndcallToServiceOutFunDefs
        testAmplTCPServer
        svs

-------------------------
-- Repeatedly ask for a number, and add it to the previous number and print the result. 
-- Mainly used to test recursively calling iRun.
-- Enter 0 to exit
-------------------------
{-
    -- service0 is StdInt
    repeatedlyAskAndAddMain:
        hPut GetChar service0
        get x on service0
        if x == 0
            then hPut Close service0
            else repeatedlyAskAndAddMain (| => service0)
-}
repeatedlyAskAndAddMain = 
    ( iConst (VInt 0) : iStore : repeatedlyAskAndAddFun
    , [(Input, (LocalChanID 0, GlobalChanID 0))]
    )
repeatedlyAskAndAddFun = 
        [ iHPut (LocalChanID 0) (HCaseIx hCaseIxGet)
        , iGet (LocalChanID 0)
        , iStore
        , iAccess 0
        , iConst (VInt 0)
        , iEq
        , iIf 
            [ iAccess 0, iHPut (LocalChanID 0) (HCaseIx hCaseIxClose), iRet ]
            [ iHPut (LocalChanID 0) (HCaseIx hCaseIxPut)
            , iAccess 0
            , iAccess 1
            , iAddInt
            , iStore
            , iAccess 0
            , iPut (LocalChanID 0)
            , iAccess 0
            , iRun [(Input, (LocalChanID 0, LocalChanID 0))] (FunID 0) 1
            ]
        ]

repeatedlyAskAndAddTest = do
    svs <- genServicesChannelManagerAndStream 
                    [] 
                    [(GlobalChanID service0POr, (IntService, StdService) )]
    execAmplMachWithDefaults 
        repeatedlyAskAndAddMain
        [(FunID 0, ({-"repeatedlyAskAndAddFun",-} repeatedlyAskAndAddFun))]
        testAmplTCPServer
        svs


-------------------------
-- Ticket booking
-------------------------
{-
    -- equivalent program looks something like this...

    ticketNum = 0

    ticketBookingMain:
    plug s
        ticketClients | service1, service2, service3 => s
        ticketServer  ticketNum | s => service0

    proc ticketClients  | service1, service2, service3 => s -> do
        fork s as 
            a with service1 -> do
                bookClient (| service1 => a)
            s' with [service2, service3] -> do
                fork s' as 
                    b with service2 -> do
                        bookClient (| service2 => b)
                    c with service3 -> do
                        bookClient (| service3 => c)

    proc bookClient
        | serviceN => n -> do
            -- any input gives a ticket (but in the future, change it
            -- so that we have 0 for no tickets, 1 for a ticket
            hPut GetInt serviceN
            get x on serviceN

            -- Send it back to the server..
            put x on n

            -- Get the ticket number and put it on the service..
            get x on n
            hPut putInt serviceN
            put x on n

            -- recurse indefinetly
            bookClient (| service0POr => n)

            -- close n
            -- hPut CloseService serviceN

    proc ticketServer
        n | s => service0 -> do
            split s into a s'
            split s' into b c

            tickerServerHelper(n | a,b,c => service0)

    proc tickerServerHelper 
        n | a,b,c => service0 -> do
            hPut PutInt service0
            put n on service0
            race
                a -> ticketServerHelper'(n | a,b,c -> service0)
                b -> ticketServerHelper'(n | b,a,c -> service0)
                c -> ticketServerHelper'(n | c,a,b -> service0)

    proc ticketServerHelper'
        n | s,t,q => service0 -> do 
            get _ from s
            put n on s
            hPut IntPut on service0
            let n' = n + 1
            put n' on service0
            tickerServerHelper(n' | s,t,q => service0)
-}

ticketBookService0 = 0
ticketBookService1 = -1
ticketBookService2 = -2
ticketBookService3 = -3
ticketBookChS = 1
ticketBookChS' = 2
ticketBookA = 3
ticketBookB = 4
ticketBookC = 5

ticketNum = 0

ticketBookingMain =
    ( 
        [ iPlug [LocalChanID ticketBookChS]
            ( 
                ( 
                    [ LocalChanID ticketBookService1
                    , LocalChanID ticketBookService2
                    , LocalChanID ticketBookService3 ]
                    , ticketClients
                )
                ,
                ( 
                    [ LocalChanID ticketBookService0 ]
                    , 
                    [ iConst (VInt ticketNum)
                    , iStore
                    , iAccess 0
                    , iRun 
                        [ (Output, (LocalChanID ticketBookService0, LocalChanID ticketBookService0)) 
                        , (Input, (LocalChanID ticketBookChS, LocalChanID ticketBookChS)) 
                        ] 
                        ticketServerFunId 
                        1 
                    ]
                )
            )
        ]
    , 
        [ (Input, (LocalChanID ticketBookService1, GlobalChanID ticketBookService1))
        , (Input, (LocalChanID ticketBookService2, GlobalChanID ticketBookService2))
        , (Input, (LocalChanID ticketBookService3, GlobalChanID ticketBookService3))
        , (Output, (LocalChanID ticketBookService0, GlobalChanID ticketBookService0))
        ]
    )

-- we inline this function..
ticketClients = 
    [ iFork (LocalChanID ticketBookChS) 
        (
            ( LocalChanID ticketBookA
            , [ LocalChanID ticketBookService1 ]
            , [ iRun 
                    [ (Input, (LocalChanID ticketServiceN, LocalChanID ticketBookService1))
                    , (Output, (LocalChanID ticketn, LocalChanID ticketBookA)) 
                    ] 
                    bookClientFunId 
                    0
              ]
            )
        , ( LocalChanID ticketBookChS'
          , [ LocalChanID ticketBookService2, LocalChanID ticketBookService3 ]
          , [ iFork (LocalChanID ticketBookChS')
                (
                        ( LocalChanID ticketBookB
                        , [ LocalChanID ticketBookService2 ]
                        , [ iRun 
                            [ (Input, (LocalChanID ticketServiceN, LocalChanID ticketBookService2))
                            , (Output, (LocalChanID ticketn, LocalChanID ticketBookB)) 
                            ] 
                            bookClientFunId
                            0
                        ]
                        )
                    ,
                        ( LocalChanID ticketBookC
                        , [ LocalChanID ticketBookService3 ]
                        , [ iRun 
                            [ (Input, (LocalChanID ticketServiceN, LocalChanID ticketBookService3))
                            , (Output, (LocalChanID ticketn, LocalChanID ticketBookC)) 
                            ] 
                            bookClientFunId
                            0
                        ]
                        )
                )
            ]
          )
        )
    ]

ticketServiceN = 1
ticketn = 2
bookClient =
    [ iHPut (LocalChanID ticketServiceN) (HCaseIx hCaseIxGet)
    , iGet (LocalChanID ticketServiceN)
    , iStore
    , iAccess 0
    , iPut (LocalChanID ticketn)
    , iGet (LocalChanID ticketn)
    , iStore
    , iAccess 0
    , iHPut (LocalChanID ticketServiceN) (HCaseIx hCaseIxPut)
    , iPut (LocalChanID ticketServiceN)
    , iRun 
        [ (Input, (LocalChanID ticketServiceN, LocalChanID ticketServiceN)) 
        , (Output, (LocalChanID ticketn, LocalChanID ticketn))
        ]
        bookClientFunId
        0
    ]
bookClientFunId = FunID 1

ticketServer = 
    [ iSplit (LocalChanID ticketBookChS) (LocalChanID ticketBookA, LocalChanID ticketBookChS')
    , iSplit (LocalChanID ticketBookChS') (LocalChanID ticketBookB, LocalChanID ticketBookC)
    , iAccess 0
    , iRun 
        [ (Input, (LocalChanID ticketBookChS, LocalChanID ticketBookA))
        , (Input, (LocalChanID ticketBookChT, LocalChanID ticketBookB))
        , (Input, (LocalChanID ticketBookChQ, LocalChanID ticketBookC))
        , (Output, (LocalChanID ticketBookService0, LocalChanID ticketBookService0))
        ]
        ticketServerHelperFunId 
        1
    ]
ticketServerFunId = FunID 0

ticketServerHelper = 
    [ iHPut (LocalChanID ticketBookService0) (HCaseIx hCaseIxPut)
    , iAccess 0
    , iPut (LocalChanID ticketBookService0)
    , iRace 
        [ (LocalChanID ticketBookChS
            , [ iAccess 0
              , iRun  
                    [ (Input, (LocalChanID ticketBookChS, LocalChanID ticketBookChS))
                    , (Input, (LocalChanID ticketBookChT, LocalChanID ticketBookChT))
                    , (Input, (LocalChanID ticketBookChQ, LocalChanID ticketBookChQ))
                    ,  (Output, (LocalChanID ticketBookService0, LocalChanID ticketBookService0))]
                    ticketServerHelperId'
                    1
              ]
            )
        , (LocalChanID ticketBookChT
            , [ iAccess 0
              , iRun  
                    [ (Input, (LocalChanID ticketBookChS, LocalChanID ticketBookChT))
                    , (Input, (LocalChanID ticketBookChT, LocalChanID ticketBookChS))
                    , (Input, (LocalChanID ticketBookChQ, LocalChanID ticketBookChQ))
                    ,  (Output, (LocalChanID ticketBookService0, LocalChanID ticketBookService0))]
                    ticketServerHelperId'
                    1
              ]
            )
        , (LocalChanID ticketBookChQ
            , [ iAccess 0
              , iRun  
                    [ (Input, (LocalChanID ticketBookChS, LocalChanID ticketBookChQ))
                    , (Input, (LocalChanID ticketBookChT, LocalChanID ticketBookChS))
                    , (Input, (LocalChanID ticketBookChQ, LocalChanID ticketBookChT))
                    ,  (Output, (LocalChanID ticketBookService0, LocalChanID ticketBookService0))]
                    ticketServerHelperId'
                    1
              ]
            )

        ]
    ]
ticketServerHelperFunId = FunID 2

ticketBookChT = 8
ticketBookChQ = 9

ticketServerHelper' = 
    [ iGet (LocalChanID ticketBookChS)
    , iAccess 0
    , iPut (LocalChanID ticketBookChS)
    , iAccess 0
    , iConst (VInt 1)
    , iAddInt
    , iStore
    , iAccess 0
    , iRun
        [ (Input, (LocalChanID ticketBookChS, LocalChanID ticketBookChS))
        , (Input, (LocalChanID ticketBookChT, LocalChanID ticketBookChT))
        , (Input, (LocalChanID ticketBookChQ, LocalChanID ticketBookChQ))
        , (Output, (LocalChanID ticketBookService0, LocalChanID ticketBookService0))
        ]
        ticketServerHelperFunId
        1
    ]
ticketServerHelperId' = FunID 3

ticketBookingTest = do
    svs <- genServicesChannelManagerAndStream 
                    [] 
                    [ (GlobalChanID ticketBookService0, (IntService, StdService) )
                    , (GlobalChanID ticketBookService1, (IntService, TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k c1  ; read'"  (Key "c1")))
                    , (GlobalChanID ticketBookService2, (IntService, TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k c2  ; read'"  (Key "c2")))
                    , (GlobalChanID ticketBookService3, (IntService, TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k c3  ; read'"  (Key "c3")))
                    ]
    execAmplMachWithDefaults 
        ticketBookingMain
        [ (bookClientFunId, ({-"bookClient",-} bookClient))
        , (ticketServerFunId, ({-"ticketServer",-} ticketServer))
        , (ticketServerHelperFunId, ({- "ticketServerHelper",-} ticketServerHelper))
        , (ticketServerHelperId', ({-"ticketServerHelper'",-} ticketServerHelper'))
        ]
        testAmplTCPServer
        svs

-------------------------
-- Id test...
-------------------------
-}
