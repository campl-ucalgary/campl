{-
True
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun showBool :: Bool -> [Char] = 
    True -> "True"
    False -> "False"


fun or :: Bool, Bool -> Bool = 
    False,False -> False
    _,_ -> True


proc bools :: Bool | Get( Bool| TopBot ) =>  =
    a | ch => -> do 
        put a on ch
        halt ch


-- | gets the bool inputs..
proc getInputs :: | Get(Bool| TopBot) (+) Get(Bool| TopBot) =>  =
    | ch => -> do
        fork ch as 
            ch0 -> bools( True | ch0 => )
            ch1 -> bools( False | ch1 => )
    


-- | actually runs the parallel or.. 
proc por :: | Console  => Get(Bool| TopBot) (+) Get(Bool| TopBot) =
    | _console => ch -> do  
        split ch into ch0,ch1
        race
            ch0 -> do
                get a on ch0
                get b on ch1

                hput ConsolePut on _console
                put showBool(or(a,b)) on _console

                hput ConsoleClose on _console
                close _console

                close ch0
                halt ch1

            ch1 -> do
                get a on ch1
                get b on ch0

                hput ConsolePut on _console
                put showBool(or(a,b)) on _console

                hput ConsoleClose on _console
                close _console

                close ch0
                halt ch1


proc run =
    | _console => ->  do
        plug
            getInputs( | ch => )
            por( | _console => ch )
