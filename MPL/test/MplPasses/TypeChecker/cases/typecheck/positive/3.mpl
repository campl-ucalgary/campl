data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun v3 :: Nat() -> Nat() =
    Succ(a) -> case a of
        b -> b
