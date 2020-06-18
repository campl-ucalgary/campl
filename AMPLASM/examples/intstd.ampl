-- Expected: ask for int input, then output the same int
%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%run ( | console => intterm ) : {
    hput ConsoleInt.Get on console  ;
    get val on console ;
    hput ConsoleInt.Put on console  ;
    put val on console ;
    hput ConsoleInt.Close on console  ;

    close console ;
}
