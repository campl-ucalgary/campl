protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    -- we would have to fork for that... thats the idea
    ConsoleStringTerminal :: S => S (+) Neg(StringTerminal)

coprotocol S => ListP(| M) =
    ListP :: S => M (*)  S
    NilP :: S => TopBot


proc root  :: | Console => ListP( | StringTerminal) = 
    | console => listp -> do
        hput ConsoleStringTerminal on console
        plug 
            => strterm -> do
                hput StringTerminalPut on strterm
                put "banan" on strterm

                hput StringTerminalClose on strterm
                halt strterm
                
            console, strterm => listp -> do
                fork console as 
                    nconsole -> root( | nconsole => listp )
                    negstrterm -> negstrterm |=| neg strterm

proc nilp :: | ListP ( | M ) =>  = 
    | listch => -> nilp( | listch => ) 

proc run :: | Console => =
    | console => -> plug
        root( | console => listp)
        nilp( | listp => )
