data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun v4 :: Nat() -> Nat() =
    a -> Succ(a)
