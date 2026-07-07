
include Prelude

protocol Echo => S =
    EchoGet :: Get( [Char] | Put( [Char] | S)) => S 
    EchoCls :: TopBot => S

coprotocol S => CoEcho =
    Echo :: S => Get( [Char] | Put( [Char] | S))
    -- NoEcho :: S => Get( [Char] | S)
    CoEchoCls :: S => TopBot


proc client = id | ch => terminal -> do

    hput StringTerminalPut on terminal
    put "Client " ++ id ++ ", please enter message:" on terminal

    hput StringTerminalGet on terminal
    get msg on terminal

    hput Echo on ch
    put "msg" ++ id ++ ": " ++ msg on ch
    get _ on ch

    hput CoEchoCls on ch
    close ch

    hput StringTerminalPut on terminal
    put "Press enter to close." on terminal

    hput StringTerminalGet on terminal
    get _ on terminal

    hput StringTerminalClose on terminal
    halt terminal


proc serve_single = | console => ch -> do
    hcase ch of
        Echo -> do
            on ch do 
                get msg
                put msg
            hput ConsolePut on console
            put msg on console
            serve_single( | console => ch)
        CoEchoCls -> do
            close ch
            hput ConsoleClose on console
            halt console    


proc server_det = | console => winner, loser -> do
    hcase winner of
        Echo -> do            
            -- could put another race here.,,
            on winner do 
                get msg1
                put msg1
            hput ConsolePut on console
            put msg1 on console
            
            race
                winner -> server_det( | console => winner, loser)
                loser -> server_det( | console => loser, winner)
        
        CoEchoCls -> do
            close winner
            serve_single( | console => loser)


proc server = | console => ch1, ch2 -> do
    -- since we are racing, ch1, ch2 must have type Get (because they are output pol.)
    -- they cannot be coprotocols even though the first thing server det is get a handle from the channel.
    -- the error we are getting is a "type check / semantic error: Match failure" so let's go look at the type checker!
    race
        ch1 -> server_det( | console => ch1, ch2)
        ch2 -> server_det( | console => ch2, ch1)


proc run =
    | console => terminal1, terminal2 -> do
        plug 
            server ( | console => ch1, ch2)
            client ( "1" | ch1 => terminal1)
            client ( "2" | ch2 => terminal2)

        