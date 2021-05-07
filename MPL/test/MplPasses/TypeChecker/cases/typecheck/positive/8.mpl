data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun v8 :: A -> Nat() =
    a -> Succ(v8(a))
