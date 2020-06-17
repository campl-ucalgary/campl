-- addds 1 to 100 with higher order data and prints it to console..
%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%destructors : {
    HigherOrder = { App 3 } ;
}

%run (| console => ) : {
    arg0 := cInt 0 ;
    arg1 := cInt 0 ;
    arg2 := cInt 100;

    rec of {
        HigherOrder.App ( var1, var2, var3 ) : { cInt 1 ; load var3 ;  add ; ret } ;
    } ;
    store higherordercodata ;

    HigherOrder.App(arg0, arg1, arg2) higherordercodata ;

    store val ;

    hput console ConsoleInt.Put ;
    put val on console ;
    hput console ConsoleInt.Close ;
}


