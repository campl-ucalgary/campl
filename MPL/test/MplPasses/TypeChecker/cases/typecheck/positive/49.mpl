
fun v1 :: (A,A) -> A=
    (a,b) -> a
    (a,b) -> b

fun v2 :: (A,A,A) -> A =
    (a,b,c) -> a
    (a,b,c) -> b
    (a,b,c) -> c

fun v3 :: (A,A,A) -> (A,A,A) =
    (a,b,c) -> (a,a,a)
    (a,b,c) -> (b,b,b)
    (a,b,c) -> (a,b,c)
