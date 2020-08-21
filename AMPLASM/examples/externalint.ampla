-- Expected: Ask for int input, print it out on an external int terminal
-- then on the same external int channel, ask for an int and print it on the
-- console
%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%handles : {
    Intterm = { Get ; Put ; Close } ;
}

%run ( | console => intterm ) : {
    hput ConsoleInt.Get on console  ;
    get val on console ;

    hput Intterm.Put on intterm  ;
    put val on intterm ;

    hput Intterm.Get on intterm ;
    get vval on intterm ;

    hput ConsoleInt.Put on console  ;
    put vval on console ;

    hput ConsoleInt.Close on console ;
    hput Intterm.Close on intterm ;

    close console ;
    close intterm ;
}
