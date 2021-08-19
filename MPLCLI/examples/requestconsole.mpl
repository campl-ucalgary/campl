{- This is impossible. Just by design of the language, I was trolling the whole
time.. No wonder why this was so difficult. 

The idea was to use a string terminal (which is forked by the console) to 
ask whether we should continue (and spawn another terminal) or stop altogether..

But this is simply impossible by the nature of the fork command.. they are disjoint..
there is no way they can communicate with each other.
-} 
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

{-
proc runner :: | Console => = 
    | console => -> 
        plug 
            => z -> 
                fork z as 
                    strterm -> do
                        hput StringTerminalGet on strterm
                        get inp on strterm

                        hput StringTerminalClose on strterm
                        halt strterm
                    negconsole -> do
                        plug 
                            => zz , negconsole -> do
                                negconsole |=| neg zz
                            zz => -> do
                                hput ConsoleClose on zz
                                halt zz
            z, console => -> do
                split z into strterm, negconsole
                hput ConsoleStringTerminal on console
                fork console as 
                    nconsole -> negconsole |=| neg nconsole
                    negstrterm -> negstrterm |=| neg strterm
-}

proc runner :: | Console => StringTerminal = 
    | console => strterm -> do
        hput StringTerminalGet on strterm
        get inp on strterm

        hput StringTerminalClose on strterm
        close strterm

        case inp of
            ":close" -> do
                hput ConsoleClose on console
                halt console

            inp -> do
                hput ConsolePut on console
                put inp on console


                plug 
                    => z -> 
                        plug 
                            fk => zz,z -> fork z as 
                                negconsole -> negconsole |=| neg zz
                                nstrterm ->  nstrterm |=| fk
                            => fk
                            zz => fk -> do
                                runner( | zz => fk)
                    
                    z,console => -> do
                        split z into negconsole, nstrterm
                        hput ConsoleStringTerminal on console
                        fork console as
                            nconsole -> negconsole |=| neg nconsole
                            negstrterm -> negstrterm |=| neg nstrterm

proc run = 
    | console => strterm -> 
        runner( | console => strterm)

