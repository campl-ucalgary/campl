protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    -- we would have to fork for that... thats the idea
    ConsoleStringTerminal :: S => S (+) Neg(StringTerminal)

coprotocol S => ListP( | M) =
    ConsP :: S => M (*) S
    NilP :: S => M 


proc generator :: | Console, StringTerminal (*) (StringTerminal (*) TopBot) =>  = 
    | console, gen => -> do
        split gen into gstrterm0, gstrterm1andgtb

        hput ConsoleStringTerminal on console
        fork console as
            nconsole -> do
                split gstrterm1andgtb into gstrterm1,gtb

                hput ConsoleStringTerminal on nconsole
                fork nconsole as
                    nnconsole -> do
                        close gtb 

                        hput ConsoleClose on nnconsole
                        halt nnconsole

                    negstrterm1 -> negstrterm1 |=| neg gstrterm1

            negstrterm0 -> negstrterm0 |=| neg gstrterm0

proc unravelGenerator :: |  =>  StringTerminal (*) (StringTerminal (*) TopBot) = 
    | => gen -> fork gen as 
        gstrterm0 -> do
            hput StringTerminalGet on gstrterm0
            get inp on gstrterm0

            hput StringTerminalPut on gstrterm0
            put inp on gstrterm0

            hput StringTerminalClose on gstrterm0
            halt gstrterm0

        gstrterm1andgtb -> fork gstrterm1andgtb as
            gstrterm1 -> do
                hput StringTerminalGet on gstrterm1
                get inp on gstrterm1

                hput StringTerminalPut on gstrterm1
                put inp on gstrterm1

                hput StringTerminalClose on gstrterm1
                halt gstrterm1
                
            gtb -> halt gtb
            

proc run :: | Console => = 
    | console => -> plug
        generator( | console, gen => )
        unravelGenerator( | => gen )

{-
proc run :: | Console => = 
    | console => ->
        plug 
            => strterm -> do
                hput StringTerminalGet on strterm
                get inp on strterm

                hput StringTerminalPut on strterm
                put inp on strterm

                hput StringTerminalClose on strterm
                halt strterm

            strterm, console => -> do
                hput ConsoleStringTerminal on console
                fork console as
                    nconsole -> do
                        hput ConsoleClose on nconsole
                        halt nconsole
                    negstrterm -> negstrterm |=| neg strterm 
-}
