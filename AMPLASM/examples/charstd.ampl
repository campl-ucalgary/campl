-- Expected: ask for char input, then output the same char
%cohandles : {
    ConsoleChar = { Get ; Put ; Close } ;
}

%run ( | cconsole => intterm ) : {
    hput cconsole ConsoleChar.Get ;
    get val on cconsole ;
    hput cconsole ConsoleChar.Put ;
    put val on cconsole ;
    hput cconsole ConsoleChar.Close ;

    close cconsole ;
}
