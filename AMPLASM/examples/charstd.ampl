-- Expected: ask for char input, then output the same char
%cohandles : {
    ConsoleChar = { Get ; Put ; Close } ;
}

%run ( | cconsole => intterm ) : {
    hput ConsoleChar.Get on cconsole  ;
    get val on cconsole ;
    hput ConsoleChar.Put on cconsole  ;
    put val on cconsole ;
    hput ConsoleChar.Close on cconsole  ;

    close cconsole ;
}
