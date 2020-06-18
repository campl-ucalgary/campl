%constructors : {
}

%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%handles : {
    IntTerm = { Get ; Put ; Close } ;
}

%functions : {
    succ(n) = {
        cInt 1 ;
        load n ;
        add ;
        ret ;
    } ;
}

%processes : {
    ticketClients( | => intterm1, intterm2, intterm3, intterm4, s) = {
        fork s as {
            s1 with intterm1, intterm2 : {
                fork s1 as {
                    s11 with intterm1 : { run bookClient(| => intterm1, s11 ) ; } ;
                    s12 with intterm2 : {  run bookClient(| => intterm2, s12 ) ; } 
                } ;
            } ;
            s2 with intterm3, intterm4 : {
                fork s2 as {
                    s21 with intterm3 : { run bookClient(| => intterm3, s21 ) ; } ;
                    s22 with intterm4 : {  run bookClient(| => intterm4, s22 ) ; } 
                } ;
            } 
        } ;

    } ;

    bookClient(| => service, s) = {
        hput IntTerm.Get on service  ;
        get x on service ;

        -- send int request to server
        put x on s ;

        -- get ticket number from server
        get ticketNum on s;

        -- print
        hput IntTerm.Put on service  ;
        put ticketNum on service ;

        run bookClient (| => service, s) ;

    } ;

    ticketServer( ticketNum | console, s => ) = {
        split s into s1 s2 ;
        split s1 into s11 s12 ;
        split s2 into s21 s22 ;

        run ticketServerHelper(ticketNum | console,s11,s12,s21,s22 => ) ;
    } ;

    ticketServerHelper(ticketNum | console,s11,s12,s21,s22 => ) = {
        hput ConsoleInt.Put on console  ;
        put ticketNum on console ;
        race {
            s11 -> { run ticketServerHelperHelper(ticketNum | console, s11,s12,s21,s22 => ) } ;
            s12 -> { run ticketServerHelperHelper(ticketNum | console, s12,s11,s21,s22 => ) } ;
            s21 -> { run ticketServerHelperHelper(ticketNum | console, s21,s12,s11,s22 => ) } ;
            s22 -> { run ticketServerHelperHelper(ticketNum | console, s22,s12,s21,s11 => ) } ;
        } ;
    } ;

    ticketServerHelperHelper(ticketNum | console,a,b,c,d => ) = {
        -- get the unused value (any int means "I want a ticket")
        get unused on a ;
        -- Return the ticket number to that guy)
        put ticketNum on a ;

        ticketNum := call succ(ticketNum) ;

        run ticketServerHelper(ticketNum | console,a,b,c,d =>) ;
    } ;
}

%run ( | console => intterm1, intterm2, intterm3, intterm4  ) : {
    ticketNum := cInt 0 ;

    plug s as {
        with [ intterm1, intterm2, intterm3, intterm4 ] : { 
                run ticketClients ( | => intterm1, intterm2, intterm3, intterm4, s ) 
            } ;
        with [console] : { 
                run ticketServer ( ticketNum | console, s => ) 
            } 
    } ;
}
