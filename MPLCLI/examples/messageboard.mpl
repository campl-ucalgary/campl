protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut   :: S => Get( [Char] | S) 
    ConsoleGet   :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

protocol InfGet(A| ) => S =
    InfGet :: Get(A | S) => S

defn 
    proc getInps :: | InfGet([Char]| ) (+) InfGet([Char] | ) => StringTerminal, StringTerminal =
        | ch => _strterm0, _strterm1 -> do
            fork ch as 
                ch0 with _strterm0 -> go( | ch0 => _strterm0)
                ch1 with _strterm1 -> go( | ch1 => _strterm1)
where
    proc go ::  | InfGet([Char] | ) => StringTerminal =
        | ch => _strterm -> do
            hput StringTerminalGet on _strterm
            get inp on _strterm

            hcase ch of 
                InfGet -> do
                    put inp on ch
                    go(| ch => _strterm)

defn 
    proc messageBoard :: | Console => InfGet([Char]| ), InfGet([Char]| ) = 
         | _console => ch0, ch1 -> do
            hput InfGet on ch0 
            hput InfGet on ch1 

            race
                ch0 -> messageBoardLoop( | _console => ch0, ch1)
                ch1 -> messageBoardLoop( | _console => ch1, ch0)

    proc messageBoardLoop :: | Console => Get([Char]| InfGet([Char] |)), Get([Char]| InfGet([Char] |)) =
        | _console => winner, loser -> do
            get winnerinp on winner 

            hput ConsolePut on _console
            put winnerinp on _console

            plug 
                messageBoard( | _console => winner, z)
                z => loser -> do
                    get loserinp on loser
                    hcase z of
                        InfGet -> do
                            put loserinp on z
                            loser |=| z

proc run :: | Console => StringTerminal, StringTerminal = 
    | _console => _strterm0, _strterm1 -> do
        plug 
            getInps( | ch => _strterm0, _strterm1)
            _console => ch -> do
                split ch into ch0, ch1
                messageBoard( | _console => ch0, ch1)
