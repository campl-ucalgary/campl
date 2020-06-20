%handles : { 
    Mem = { Get ; Put ; Close } ;
        -- get: get a new value and assigns it to the memory
        -- put: put the current value on the channel
        -- close: closes
    IntTerm = { Get ; Put ; Close } ;
    Passer = { Pass } ;
}


%processes : {
    memory(x | mem => ) = {
        hcase mem of {
            Mem.Get : { 
                put x on mem ;
                run memory(x | mem => ) ;
            };
            Mem.Put : { 
                get y on mem ;
                run memory(y | mem => ) ;
            } ;
            Mem.Close : { close mem ; } ;
        };
    } ;

    p1( | => p, intterm1) = {
        hput Passer.Pass on p ;

        split p into mm nmpp ;
        hput Mem.Get on mm ;
        get y on mm ;

        hput IntTerm.Put on intterm1 ;
        put y on intterm1 ;

        hput IntTerm.Get on intterm1 ;
        get x on intterm1 ;

        hput Mem.Put on mm ;
        put x on mm ;

        fork nmpp as {
            nm with mm : { nm == mm ; } ;
            -- nm with mm : { nm == neg(mm) ; } ;
            pp with intterm1 : { run p1(| => pp, intterm1) }
        } ;
    };

    p2(| p => intterm2,mem ) = {
        hcase p of {
            Passer.Pass : {
                hput Mem.Get on mem ;
                get y on mem ;

                hput IntTerm.Put on intterm2 ;
                put y on intterm2 ;

                hput IntTerm.Get on intterm2 ;
                get x on intterm2 ;

                hput Mem.Put on mem ;
                put x on mem ;

                fork p as {
                    mm with mem : { mm == mem ; } ;
                    nmpp with intterm2 : {
                        split nmpp into nm pp ;
                        plug z as {
                            with [ pp, intterm2 ] : { run p2( | pp => intterm2, z) ; } ;
                            with [ nm ] : { z == nm } 
                            -- with [ nm ] : { z == neg(nm) } 
                        };
                    } 
                } ;
            } ;
        } ; 
    };

}

%run ( | => intterm1, intterm2  ) : {
    plug mem as {
        with [intterm1, intterm2] : {
                plug ch as {
                    with [intterm1] : { run p1(| => ch, intterm1) ; } ;
                    with [intterm2,mem] : { run p2(| ch => intterm2, mem) ; }
                } ;
            } ;
        with [] : { val := cInt 10 ; run memory(val | mem =>) ; }  
    } ;

}

-- %handles : { 
--     Mem = { Put ; Get ; Close } ;
--         -- get: get a new value and assigns it to the memory
--         -- put: put the current value on the channel
--         -- close: closes
--     IntTerm = { Get ; Put ; Close } ;
--     Passer = { Pass } ;
-- }
-- 
-- 
-- %processes : {
--     memory(x | ch => ) = {
--         hcase ch of {
--             Mem.Put : { 
--                 get y on ch ;
--                 run memory(y | ch => ) ;
--             };
--             Mem.Get : { 
--                 get x on ch ;
--                 run memory(x | ch => ) ;
--             } ;
--             Mem.Close : { close ch ; } ;
--         };
--     } ;
-- 
--     p1( | => p, intterm1) = {
--         hput Passer.Pass on p ;
-- 
--         split p into mm nmpp ;
-- 
--         hput Mem.Get on mm ;
--         get y on mm ;
-- 
--         hput IntTerm.Put on intterm1 ;
--         put y on intterm1 ;
-- 
--         hput IntTerm.Get on intterm1 ;
--         get x on intterm1 ;
-- 
--         hput Mem.Put on mm ;
--         put x on mm ;
-- 
--         fork nmpp as {
--             nm with mm : { nm == mm ; } ;
--             -- nm with mm : { nm == neg(mm) ; } ;
--             pp with intterm1 : { run p1(| => pp, intterm1) }
--         } ;
--     };
-- 
--     p2(| p => intterm2,mem ) = {
--         hcase p of {
--             Passer.Pass : {
--                 hput Mem.Get on mem ;
--                 get y on mem ;
-- 
--                 hput IntTerm.Put on intterm2 ;
--                 put y on intterm2 ;
--                 hput IntTerm.Get on intterm2 ;
--                 get x on intterm2 ;
-- 
--                 hput Mem.Put on mem ;
--                 put x on mem ;
-- 
--                 fork p as {
--                     mm with mem : { mm == mem ; } ;
--                     nmpp with intterm2 : {
--                         split nmpp into nm pp ;
--                         plug z as {
--                             with [ pp, intterm2 ] : { run p2( | pp => intterm2, z) ; } ;
--                             with [ nm ] : { z == nm } 
--                             -- with [ nm ] : { z == neg(nm) } 
--                         };
--                     } 
--                 } ;
--             } ;
--         } ; 
--     }; 
-- }
-- 
-- %run ( | => intterm1, intterm2  ) : {
--     plug p as {
--         with [intterm1] : { run p1(| => p, intterm1) ; } ;
--         with [intterm2] : {
--             plug mem as {
--                 with [p, intterm2] : { run p2(| p => intterm2, mem) ; } ;
--                 with [] : { val := cInt 10 ; run memory( val | mem => ) ; } 
--             } ;
--         }
--     };
-- }
