-- Expected: folds over [200,100] and outputs to Console
%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%constructors : {
    List = { Nil 0 ; Cons 2} ;
}

%functions : {
    listSum(list) = {
        case list of {
            List.Nil : { 
                cInt 0 ; 
                ret ; 
            } ;
            List.Cons ( var1, var2 ): { 
                load var1 ;
                call listSum (var2);
                add ;
                ret ;
            } ;
        };
        ret
    }
}

%run ( | console => ) : {
    n := List.Nil ;
    a0 := cInt 100;
    l0 := List.Cons(a0,n);

    a1 := cInt 200;
    finallist := List.Cons(a1,l0);

    sum := call listSum(finallist) ;

    hput console ConsoleInt.Put ;
    put sum on console ;

    hput console ConsoleInt.Close ;
    close console ;
}
