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

protocol InfGet(A| ) => S =
    InfGet :: Get(A | S) => S

proc strTermInfGetter :: | InfGet([Char]|) => StringTerminal = 
    | infgetter => strterm -> hcase infgetter of InfGet -> do
        hput StringTerminalGet on strterm
        get inp on strterm

        put inp on infgetter

        strTermInfGetter( | infgetter => strterm)
        

coprotocol S => ListP( | M)  =
    ConsP :: S => M (+) S
    NilP :: S => M
        
defn 
    proc node :: | InfGet([Char] | ) => InfGet([Char]| ), InfGet([Char]| ) = 
         | up => ch0, ch1 -> do
            hput InfGet on ch0 
            hput InfGet on ch1 

            race
                ch0 -> nodeLoop( | up => ch0, ch1)
                ch1 -> nodeLoop( | up => ch1, ch0)

    proc nodeLoop :: | InfGet([Char]| ) => Get([Char]| InfGet([Char] |)), Get([Char]| InfGet([Char] |)) =
        | up => winner, loser -> do
            get winnerinp on winner 

            hcase up of InfGet -> do
                put winnerinp on up

                plug 
                    node( | up => winner, z)
                    z => loser -> do
                        get loserinp on loser
                        hcase z of
                            InfGet -> do
                                put loserinp on z
                                loser |=| z


proc root :: | Console => InfGet([Char] | ) = 
    | console => infget -> do
        hput InfGet on infget
        get inp on infget

        hput ConsolePut on console
        put inp on console

        root( | console =>  infget)


proc nilp :: | ListP(| InfGet([Char]|)) => StringTerminal =
    | up => strterm -> do
        hput NilP on up
        strTermInfGetter( | up => strterm)

proc consp :: | ListP(| InfGet([Char]|)) => StringTerminal,ListP(| InfGet([Char]|)) =
    | up => strterm, tail -> do
        hput ConsP on up
        fork up as 
            l -> strTermInfGetter( | l => strterm)
            r -> r |=| tail

defn
    proc messageBoard :: | Console => ListP(| InfGet([Char]|)) = 
        | console => listp -> 
            plug
                root ( | console => infget )
                unravel( | infget => listp )
where
    proc unravel :: | InfGet([Char] | ) => ListP(| InfGet([Char]|)) = 
        | infget => listp -> do
            hcase listp of 
                ConsP -> do
                    split listp into l, r
                    plug 
                        infget => l,z -> node( | infget => l,z)
                        z => r -> unravel( | z => r)
                    
                NilP -> infget |=| listp



proc run :: | Console => StringTerminal, StringTerminal, StringTerminal =
    | console => strterm0, strterm1, strterm2 -> do
        plug 
            messageBoard( | console => listp0) 
            consp( | listp0 => strterm0, listp1 )
            consp( | listp1 => strterm1, listp2 )
            nilp(  | listp2 => strterm2)
