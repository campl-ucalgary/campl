-- Expected: opens up two terminals and ors the inputs
-- 't' is true and 'f' is false

%constructors : {
    Bool = { False 0 ; True 0 } ;
}

%functions : {
    chartfToBool( x ) = { 
            cChar 't' ;
            load x ;
            internalBool := eq ;
            if internalBool
                then { Bool.True ; ret }
                else { Bool.False ; ret } ;
            ret 
        } ;

    orfun(a , b ) = {
        case a of {
            Bool.True : { Bool.True ; ret ;} ;
            Bool.False : { 
                case b of {
                    Bool.True : { Bool.True ; ret } ;
                    Bool.False : { Bool.False ; ret } ;
                };
                ret ;
            };
        };
        ret ;
    } ;

    boolToChar( x ) = { 
        case x of {
            Bool.True : { cChar 't' ; ret ;} ;
            Bool.False : { cChar 'f' ; ret ;};
        } ;
        ret ;
    } ;
}

%cohandles : {
    ConsoleChar = { ConsoleGet ; ConsolePut ; ConsoleClose } ;
}

%handles : {
    CharTerm = { CharGet ; CharPut ; CharClose } ;
}

%processes : {
    p1serviceor ( |  => charterm1, charterm2, s ) = { 
            fork s as {
                s1 with charterm1 : {
                    hput CharTerm.CharGet on charterm1  ;
                    get x on charterm1 ;
                    nx := call chartfToBool ( x ) ;
                    put nx on s1 ;
                    close s1 ;
                    hput CharTerm.CharClose on charterm1 
                } ;
                s2 with charterm2 : {
                    hput CharTerm.CharGet on charterm2  ;
                    get x on charterm2 ;
                    nx := call chartfToBool( x ) ;
                    put nx on s2 ;
                    close s2 ;
                    hput CharTerm.CharClose on charterm2 
                }
            };
        } ;
    
    p2serviceor ( | s, console => ) = {
        split s into s1 s2 ;
        race { 
            s1 -> { 
                get va on s1 ;
                case va of {
                    Bool.True : {
                            charva := call boolToChar (va) ;

                            hput ConsoleChar.ConsolePut on console  ;
                            put charva on console  ;

                            get unused on s2 ;
                            close s1 ;
                            close s2 ;
                            hput ConsoleChar.ConsoleClose on console  ;
                        } ;
                    Bool.False  : {
                            get vb on s2 ;
                            nvb := call orfun (va, vb) ;
                            charnvb := call boolToChar(nvb) ;

                            hput ConsoleChar.ConsolePut on console  ;
                            put charnvb on console ;

                            close s1;
                            close s2;
                            hput ConsoleChar.ConsoleClose on console  ;
                    };
                } ;
            };
            s2 -> { 
                get va on s2 ;
                case va of {
                    Bool.True : {
                            charva := call boolToChar (va) ;

                            hput ConsoleChar.ConsolePut on console  ;
                            put charva on console  ;

                            get unused on s1 ;
                            close s2 ;
                            close s1 ;
                            hput ConsoleChar.ConsoleClose on console  ;
                        } ;
                    Bool.False  : {
                            get vb on s1 ;
                            nvb := call orfun (va, vb) ;
                            charnvb := call boolToChar(nvb) ;

                            hput ConsoleChar.ConsolePut on console  ;
                            put charnvb on console ;

                            close s2;
                            close s1;
                            hput ConsoleChar.ConsoleClose on console  ;
                    };
                } ;
            };

        }; 
    }
}


%run ( | console => charterm1, charterm2 ) : {
    plug s as {
        with [charterm1,charterm2] : { run p1serviceor ( | => charterm1, charterm2, s ) } ;
        with [console] : { run p2serviceor (| s, console => ) } 
    } ;
}

