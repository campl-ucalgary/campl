{- UH NOTHING -}
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

coprotocol S => CoGetter( A | ) =
    CoGetterGet :: S => Get(A | S)
    CoGetterClose :: S => TopBot

proc generateN :: Int | Console, ListP( | StringTerminal ) => StringTerminal = 
    0 | console, gen => strterm -> do
        hput ConsoleClose on console
        close console

        hput NilP on gen
        strterm |=| gen

    n | console, gen => strterm -> do
        hput ConsP on gen 
        split gen into gstrterm, ngen

        hput ConsoleStringTerminal on console
        fork console as
            nconsole -> generateN( n - 1 | nconsole, ngen => strterm )
            negstrterm -> negstrterm |=| neg gstrterm

{-
proc unravelGenerator :: |  CoGetter ( [Char] | ) => ListP (| StringTerminal) = 
    | putter => gen -> hcase gen of 
        ConsP -> plug 
            putter => z -> fork z as
                nputter -> nputter |=| putter
                nstrterm -> 
            z => -> do
                split z into nputter, nstrterm
                fork gen as
                    strterm -> strterm |=| nstrterm
                    ngen -> unravelGenerator( | nputter => ngen ) 

        NilP -> do
            hput StringTerminalGet on gen
            get inp on gen

            hput CoGetterGet on putter
            put inp on putter

            hput CoGetterClose on putter
            close putter

            hput StringTerminalClose on gen
            halt gen
-}


{-
proc run :: | Console => StringTerminal = 
    | console => strterm -> plug
        generateN( 6 | console, gen => strterm )
        unravelGenerator( | => gen )
-}
