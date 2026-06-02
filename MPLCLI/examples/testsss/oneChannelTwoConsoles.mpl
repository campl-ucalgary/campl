protocol StringTerminal => S =
    StringTerminalGet :: Get ([ Char ]| S ) => S
    StringTerminalPut :: Put ([ Char ]| S ) => S
    StringTerminalClose :: TopBot => S


coprotocol S => Console =
    ConsolePut :: S => Get ([ Char ]| S )
    ConsoleGet :: S => Put ([ Char ]| S )
    ConsoleClose :: S => TopBot
    ConsoleStringTerminal :: S => S (*) Neg ( StringTerminal )
    
    
    
proc helloWorld1 :: | Console => TopBot =
    | console => ch -> do
        close ch
        hput ConsolePut on console 
        put "Hello from process 1" on console
        hput ConsoleClose on console
        halt console
        
        
proc helloWorld2 :: | TopBot, Console => =
    | ch, console => -> do
        close ch
        hput ConsolePut on console
        put "Hello from process 2" on console
        hput ConsoleClose on console
        halt console


proc run :: | Console(*)Console => = 
    | consoles => -> do
        split consoles into console1, console2
        plug
            helloWorld1( | console1 => z)
            helloWorld2( | z, console2 =>)