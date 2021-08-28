protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

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
                        hcase z of InfGet -> do
                            put loserinp on z
                            loser |=| z

proc nilp :: | ListP(| M) => M =
    | up => m -> do
        hput NilP on up
        up |=| m

proc consp :: | ListP(| M) => M,ListP(| M) =
    | up => m, tail -> do
        hput ConsP on up
        fork up as 
            l -> l |=| m
            r -> r |=| tail

defn 
    proc messageBoard :: | Console => ListP(| InfGet([Char]|)) = 
        | console => listp -> plug
            root ( | console => infget )
            unravelToMessageBoard( | infget => listp )

    proc root :: | Console => InfGet([Char] | ) = 
        | console => infget -> do
            hput InfGet on infget
            get inp on infget

            case inp of
                ":new-friend" -> do
                    hput ConsoleStringTerminal on console

                    split console into nconsole, negstrterm

                    plug 
                        negstrterm, strterm => -> negstrterm |=| neg strterm

                        messageBoard( | nconsole => nlistp)

                        consp( | nlistp => h, nlistp0)
                        strTermInfGetter( | h => strterm ) 

                        nilp( | nlistp0 => infget)

                _ -> do
                    hput ConsolePut on console
                    put inp on console

                    root( | console =>  infget)
where
    proc unravelToMessageBoard :: | InfGet([Char] | ) => ListP(| InfGet([Char]|)) = 
        | infget => listp -> do
            hcase listp of 
                ConsP -> do
                    split listp into l, r
                    plug 
                        infget => l,z -> node( | infget => l,z)
                        z => r -> unravelToMessageBoard( | z => r)
                    
                NilP -> infget |=| listp


proc run :: | Console => StringTerminal =
    | console => strterm -> plug 
        messageBoard( | console => listp) 

        nilp( | listp => m)
        strTermInfGetter( | m => strterm)
