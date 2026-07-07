
include Prelude

protocol Echo => S =
    EchoGet :: Get( [Char] | Put( [Char] | S)) => S 
    EchoCls :: TopBot => S


proc client = id | ch => terminal -> do

    hcase ch of 
        EchoGet -> do

            hput StringTerminalPut on terminal
            put "Client " ++ id ++ ", please enter message:" on terminal
    
            hput StringTerminalGet on terminal
            get msg on terminal

            put "msg" ++ id ++ ": " ++ msg on ch
            get _ on ch

            client( id | ch => terminal)

        EchoCls -> do
            close ch

            hput StringTerminalPut on terminal
            put "Press enter to close." on terminal

            hput StringTerminalGet on terminal
            get _ on terminal

            hput StringTerminalClose on terminal
            halt terminal


proc server_det = | console => winner, loser -> do
    on winner do 
        get msg1
        put msg1
        hput EchoCls
        close

    hput ConsolePut on console
    put msg1 on console

    on loser do 
        get msg2
        put msg2
        hput EchoCls
        close

    hput ConsolePut on console
    put msg2 on console

    hput ConsoleClose on console
    halt console


proc otherclient = id | => ch, terminal -> do

    hput StringTerminalPut on terminal
    put "Press enter to close." on terminal

    hput StringTerminalGet on terminal
    get _ on terminal

    hput StringTerminalClose on terminal
    close terminal

    put "done" on ch
    halt ch

proc server = | console => ch1, ch2 -> do
    -- hput EchoGet on ch1
    -- hput EchoGet on ch2
    race
        ch1 -> server_det( | console => ch1, ch2)
        ch2 -> server_det( | console => ch2, ch1)


proc server2 = | console, ch3 => ch1, ch2 -> do
    hput EchoGet on ch1
    hput EchoGet on ch2
    race
        ch1 -> do
            get _ on ch3
            close ch3
            server_det( | console => ch1, ch2)
        ch2 -> do
            get _ on ch3
            close ch3
            server_det( | console => ch2, ch1)
        ch3 -> do
            get _ on ch3
            close ch3
            server ( | console => ch1, ch2)




proc run =
    -- | console => terminal1, terminal2 -> do
    | console => terminal1, terminal2, terminal3 -> do
        plug 
            -- server ( | console => ch1, ch2)
            server2 ( | console, ch3 => ch1, ch2)
            client ( "1" | ch1 => terminal1)
            client ( "2" | ch2 => terminal2)
            otherclient ("3" | => ch3, terminal3)

        