protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut   :: S => Get( [Char] | S) 
    ConsoleGet   :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

protocol
    InfGet(A| ) => S =
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
    proc manageInps :: | Console => InfGet( [Char] | ) (+) InfGet( [Char] | ) = 
        | _console => ch -> do
            split ch into ch0,ch1

            hput InfGet on ch0
            get inp0 on ch0

            hput InfGet on ch1
            get inp1 on ch1


            hput ConsolePut on _console
            put inp0 on _console

            hput ConsolePut on _console
            put inp1 on _console

            manageInps( | _console => ch)
where
    proc go :: | Console => InfGet( [Char] | ) =
        _console

proc run = 
    | _console => _strterm0, _strterm1 -> do 
        plug 
            getInps( | ch => _strterm0, _strterm1 )
            manageInps( | _console => ch   )
