-- Expected: Ask for int input, print it out on an external int terminal
-- then on the same external int channel, ask for an int and print it on the
-- console

%constructors : {
    Bool = { False 0 ; True 0 } ;
}

%functions : {
    internalBoolToBool( internalBool ) = { 
            if internalBool
                then { Bool.True ; ret }
                else { Bool.False ; ret } ;
            ret 
        } ;

    chartfToBool( x ) = { 
            if internalBool
                then { Bool.True ; ret }
                else { Bool.False ; ret } ;
            ret 
        } ;
}

%cohandles : {
    ConsoleChar = { ConsoleGet ; ConsolePut ; ConsoleClose } ;
}

%processes : {
    p1serviceor ( | charterm1, charterm2 => s ) = { 
            fork s as {
                s0 with charterm1 : {
                    hput charterm1 ConsoleChar.ConsoleGet
                    get x on charterm1
                    x := 
                } ;
                s1 with charterm2 : {
                }
            };

        } ;
}


%handles : {
    CharTerm = { CharGet ; CharPut ; CharClose } ;
}

%run ( | console => charterm1, charterm2 ) : {

    plug s as {
        with [] : { -- COMMANDS} ;
        with [] : { -- COMMANDS} ;
    } ;
}
