

-- want more than one console error
-- the inferred (from the hello world type signature) 
-- type Console doesn't have the same
-- abstract syntax tree node type as the type inferred with no user provided types
-- or when the user has provided all types. idk why, but we are handling that
-- properly? now. idk if we should change the type inference part to make it the same type

-- coprotocol S => Console = 
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot     

protocol SendMsgs => S =
    SendMsg :: Put([Char]|S) => S 
    Close :: TopBot => S

-- user provides type here
proc helloworld :: | Console => =
    | ch => -> on ch do
        hput ConsolePut
        put "hi"
        hput ConsoleClose
        halt
        
-- no type signature given here
-- then the type of console is stored as a _TypeWithNoArgs
-- instead of a _TypeConcWithArgs. idk why
proc run =
    | console, console2 => -> plug
        ch, console => -> do
            close ch 
            helloworld( | console => )
        console2 => ch -> do
            close ch 
            helloworld( | console2 => )

-- anyway we are now correctly getting the error
-- mpl: assembler error:
-- Main run process has more than one service channel of type Console: console at line 31 and column 7, console2 at line 31 and column 16