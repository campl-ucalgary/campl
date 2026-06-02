
proc p :: Int | A => A =
    n | x => y -> x |=| y

proc q :: Store( Int | A => A) | A => A =
    p | x => y -> use(p)( 5 | x => y) 

proc run :: | A => A =
    | ch0 => ch1 -> q(store(p) | ch0 => ch1)