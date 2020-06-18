%handles : { 
    Mem = { Get ; Put ; Close } ;
        -- get: get a new value and assigns it to the memory
        -- put: put the current value on the channel
        -- close: closes
    IntTerm = { Get ; Put ; Close } ;
}

%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%constructors : {
    List = { Cons 2 ; Nil 0 } ;
    Bool = { False 0 ; True 0 } ;
}


%destructors : {
    ListFold = { ConsFold 2 ; NilFold 0 } ;
}

%functions : {
    lnil() = { List.Nil ; ret ; } ;

    lcons(s, ss) = { List.Cons(s, ss) ; ret ; } ;
    listfold(folder, list) = {
        case list of {
            List.Nil : {
                ListFold.NilFold folder ;
                ret ;
            } ;
            List.Cons(s, ss) : {
                acc := call listfold(folder, ss) ;
                ListFold.ConsFold(s, acc) folder ;
                ret ;
            } ;
        } ;
        ret ;
    } ;

    boolor(a , b ) = {
        case a of {
            Bool.True : { Bool.True ; ret ; } ; 
            Bool.False : { 
                case b of {
                    Bool.True : { Bool.True ; ret ; } ;
                    Bool.False : { Bool.False ; ret ; } ;
                };
                ret ;
            };
        };
        ret ;
    } ;

    internalbooltoBool(a) = {
        if a 
            then { Bool.True ; ret ;} 
            else { Bool.False ; ret ; } ;
        ret ;
    } ;

    intinintlist(s,list) = {
        inlistfold := 
            rec of {
                ListFold.ConsFold(a, acc) : {
                    load a ;
                    load s ;
                    eq ;
                    store res; 
                    res := call internalbooltoBool(res);
                    call boolor(res, acc) ;
                    ret ;
                } ;
                ListFold.NilFold() : {
                    Bool.False ;
                    ret ;
                } ;
            } ; 

        call listfold(inlistfold, list) ;
        ret ; 
    } ;

}

%processes : {
    memory(x | mem => ) = {
        hcase mem of {
            Mem.Get : { 
                put x on mem ;
                run memory(x | mem => ) ;
            };
            Mem.Put : { 
                get y on mem ;
                run memory(y | mem => ) ;
            } ;
            Mem.Close : { close mem ; } ;
        };
    } ;

    p1( | => p, intterm1) = {
        split p into mm nmpp ;
        hput Mem.Get on mm ;
        get y on mm ;

        hput IntTerm.Put on intterm1 ;
        put y on intterm1 ;

        hput IntTerm.Get on intterm1 ;
        get x on intterm1 ;

        hput Mem.Put on mm ;
        put x on mm ;

        fork nmpp as {
            nm with mm : { mm == nm ; } ;
            -- nm with mm : { nm == mm ; } ;
                -- neg
            pp with intterm1 : { run p1(| => pp, intterm1) }
        } ;
    };

    p2(| p => intterm2,mem ) = {
        hput Mem.Get on mem ;
        get y on mem ;

        hput IntTerm.Put on intterm2 ;
        put y on intterm2 ;

        hput IntTerm.Get on intterm2 ;
        get x on intterm2 ;

        hput Mem.Put on mem ;
        put x on mem ;

        fork p as {
            mm with mem : { mm == mem ; } ;
            nmpp with intterm2 : {
                split nmpp into nm pp ;
                plug z as {
                    with [ pp, intterm2 ] : { run p2( | pp => intterm2, z) ; } ;
                    with [ nm ] : { nm == z } 
                    -- with [ nm ] : { z == nm } 
                        -- neg
                };
            } 
        } ;
    };


    -- p2( | => intterm2, mem) = {
    --     hput Mem.Get on mem ;
    --     get y on mem ;

    --     hput IntTerm.Put on intterm2 ;
    --     put y on intterm2 ;

    --     hput IntTerm.Get on intterm2 ;
    --     get x on intterm2 ;

    --     hput Mem.Put on mem ;
    --     put x on mem ;
    -- } ;
}

%run ( | => intterm1, intterm2  ) : {
    plug p as {
        with [intterm1] : { run p1(| => p, intterm1) ; } ;
        with [intterm2] : {
            plug mem as {
                with [p, intterm2] : { run p2(| p => intterm2, mem) ; } ;
                with [] : { val := cInt 10 ; run memory( val | mem => ) ; } 
            } ;
        }
    };

    -- plug mem as {
    --     with [intterm2] : { run p2(| => intterm2, mem) ; } ;
    --     with [] : { val := cInt 10 ; run memory( val | mem => ) ; } 
    -- } ;
}
