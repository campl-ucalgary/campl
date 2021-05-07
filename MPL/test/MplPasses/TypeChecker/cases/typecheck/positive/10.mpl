codata S -> Fun(A,B) =
    App :: A, S -> B

fun myConst :: A -> Fun(B,B)  =
    a -> (App := b -> b)
