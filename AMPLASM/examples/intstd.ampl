-- Expected: ask for int input, then output the same int
%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%run ( | console => intterm ) : {
    hput console ConsoleInt.Get ;
    get val on console ;
    hput console ConsoleInt.Put ;
    put val on console ;
    hput console ConsoleInt.Close ;

    close console ;
}
