%cohandles : {
    Console = { Get ; Put ; Close } ;
}

%handles : {
    IntTerm = { Get ; Put ; Close } ;
}

%processes : {
    p1(| console => w1) = {
        hput Console.Get on console ;
        get val on console ;

        put val on w1 ;
        get val2 on w1 ;

        hput Console.Put on console ;
        put val2 on console ;

        close w1 ;

        hput Console.Close on console ;
        close console ;
    } ;

    p2( | w1 => w2 ) = {   
        w1 == w2 ;
    };

    p3(| w2 => i1) = {
        get val on w2;

        hput IntTerm.Put on i1;
        put val on i1;

        hput IntTerm.Get on i1 ;
        get val2 on i1;
        put val2 on w2;
        
        hput IntTerm.Close on i1 ;

        close i1 ;
        close w2 ;
    };
}

%run ( | console => intterm ) : {
    
    plug w2 as {
        with [console] : { 
                plug w1 as {
                    with [console] :
                        { run p1( | console => w1 ) ; } ;
                    with [w2] :
                        { run p2 ( | w1 => w2 ) ;  }
                } ;
            } ;
        with [intterm] : { run p3 (| w2 => intterm); } 
    } ;
}

--coprotocol CP => Console (A) =
--    GetIntC   :: CP => Put (A|CP)  
--    PutIntC   :: CP => Get (A|CP) 
--    CloseC    :: CP => TopBot  
--
--protocol IntTerm (A) => P =
--    GetInt   :: Get (A|P) => P 
--    PutInt   :: Put (A|P) => P
--    Close    :: TopBot    => P  
--
--
--proc p1 = 
--  | console => w1 -> do 
--       hput GetIntC on console
--       get val on console
--       put val on w1
--       get val2 on w1
--       hput PutIntC on console
--       put val2 on console 
--       close w1
--       hput CloseC on console
--       halt console
--
--proc p2 =
--  | w1 => w2 -> do 
--       w2 |=| w1 
--
--proc  p3 =
--    | w2 => i1 -> do 
--       get val on w2 
--       hput PutInt on i1 
--       put val on i1 
--       hput GetInt on i1 
--       get val2 on i1 
--       put val2 on w2
--       hput Close on i1 
--       close i1 
--       halt w2  
--
--
--run console => intTerm1 -> do 
--    plug 
--      p1 ( | console => w1)
--      p2 ( | w1 => w2)
--      p3 ( | w2 => intTerm1) 
--
--
