protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    -- we would have to fork for that... thats the idea
    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

coprotocol S => ListP( | M) =
    ConsP :: S => M (+) S
    NilP :: S => M

proc generateN :: Int | Console, ListP( | StringTerminal ) => StringTerminal = 
    0 | console, gen => strterm -> do
        hput ConsoleClose on console
        close console

        hput NilP on gen
        strterm |=| gen

    n | console, gen => strterm -> do
        hput ConsoleStringTerminal on console
        split console into nconsole, negstrterm

        plug 
            negstrterm, nstrterm => -> negstrterm |=| neg nstrterm 

            nconsole, gen => nstrterm, strterm -> do
                hput ConsP on gen 
                fork gen as
                    gstrterm -> nstrterm |=|  gstrterm
                    ngen -> generateN( n - 1 | nconsole, ngen => strterm )

proc unravelGenerator :: | => ListP (| StringTerminal) = 
    | => gen -> hcase gen of 
        ConsP -> do
            split gen into gstrterm, ngen

            -- if we did not plug, we would have to wait for some input,then
            -- new terminals would be spawned, so we do a dummy plug essentially
            plug 
                z => gstrterm -> do
                    close z

                    hput StringTerminalGet on gstrterm
                    get inp on gstrterm

                    hput StringTerminalPut on gstrterm
                    put inp on gstrterm

                    hput StringTerminalClose on gstrterm
                    halt gstrterm
                => z, ngen -> do
                    close z

                    unravelGenerator( | => ngen )

        NilP -> do
            hput StringTerminalGet on gen
            get inp on gen

            hput StringTerminalPut on gen
            put inp on gen

            hput StringTerminalClose on gen
            halt gen


proc run :: | Console => StringTerminal = 
    | console => strterm -> plug
        generateN( 6 | console, gen => strterm )
        unravelGenerator( | => gen )
