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
    hput console ConsoleInt.Get ;
    get val on console ;

    hput intterm Intterm.Put ;
    put val on intterm ;

    hput intterm Intterm.Get;
    get vval on intterm ;

    hput console ConsoleInt.Put ;
    put vval on console ;

    hput console ConsoleInt.Close;
    hput intterm Intterm.Close;

    close console ;
    close intterm ;
}
