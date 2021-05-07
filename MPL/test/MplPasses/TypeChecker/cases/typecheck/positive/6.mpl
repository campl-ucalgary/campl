data List(A) -> S =
    Cons :: A,S -> S
    Nil ::   -> S

fun v6 :: List(A), List(List(A)) -> List(List(A)) =
    a,b -> Cons(a,b)
    Cons(a,b),c -> Nil
