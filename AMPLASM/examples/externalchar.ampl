-- Expected: ask for char input, then output the same char on an external service
%cohandles : {
    ConsoleChar = { Get ; Put ; Close } ;
}

%handles : {
    CharTerm = { Get ; Put ; Close } ;
}

%run ( | cconsole => charterm ) : {
    hput cconsole ConsoleChar.Get ;
    get val on cconsole ;

    hput charterm CharTerm.Put ;
    put val on charterm ;
    hput charterm CharTerm.Close ;

    hput cconsole ConsoleChar.Close ;

    close cconsole ;
}
