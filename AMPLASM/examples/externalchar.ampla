-- Expected: ask for char input, then output the same char on an external service
%cohandles : {
    ConsoleChar = { Get ; Put ; Close } ;
}

%handles : {
    CharTerm = { Get ; Put ; Close } ;
}

%run ( | cconsole => charterm ) : {
    hput ConsoleChar.Get on cconsole  ;
    get val on cconsole ;

    hput CharTerm.Put on charterm  ;
    put val on charterm ;
    hput CharTerm.Close on charterm  ;

    hput ConsoleChar.Close on cconsole  ;

    close cconsole ;
}
