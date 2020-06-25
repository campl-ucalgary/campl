defn of
    data Hashbrowns -> C = 
        MakeHashbrowns :: Int -> C
        MakeHashbrownsWithSalad :: Salad -> C

where 
    data Salad -> C = 
        MakeSalad :: Int -> C
    fun potato = 
        -> 2
{-
-- error: potato has been defined multiple times
-- but potato is in a let binding and these should not over lap...
fun potato = 
    -> 2
-}

{-
-- it compiles with this even though it should not
-- Salad should be local
fun pomelo :: Salad -> Int =
    _ -> 4
-}

{-
-- another bug
-- Expected 1 got 0 even though there are clearly 0
-- arguments
fun orange :: -> Int = 
    -> orange( )

-- parse error
fun orange :: -> Int = 
    -> orange()
-}


protocol Passer(A) => P =
    Pass :: A (+) (Neg(A) (*) P) => P

{-
proc p2 :: | Passer(MEM) => IntTerm, MEM = 
    | p => intterm, mem -> do
        hcase p of
            Pass -> do
                fork p as
                    mm -> mm |=| mem
                    nmpp -> do
                                split nmpp into nm,pp
                                plug 
                                    p2(|pp=>intterm, z)
                                    z |=| neg nm
-}



{-
-- should not compile! 
-- A is a free variable
data Cherry -> C =
    MakeCherry :: A -> C

-- Arity mismatch with datatype
fun cherryMaker :: Cherry(Int,Int,Int,Char,Int) -> Int =
    MakeCherry(a) -> a

-- I think the type variables for a data type do not 
-- do anything for instance, the following will also compile
data Cherry(A,B,C) -> C =
    MakeCherry :: A -> C

-- Arity mismatch with datatype
fun cherryMaker :: Cherry -> Int =
    MakeCherry(a) -> a
-}


run console => intTerm1,intTerm2,intTerm3 -> do 
    close console
    close intTerm1
    close intTerm2
    halt intTerm3

