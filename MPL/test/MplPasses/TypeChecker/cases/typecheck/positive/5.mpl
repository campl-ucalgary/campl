data List(A) -> S =
    Cons :: A,S -> S
    Nil ::   -> S

fun v5 :: A,List(A) -> List(A) =
    a,b -> Cons(a,b)
