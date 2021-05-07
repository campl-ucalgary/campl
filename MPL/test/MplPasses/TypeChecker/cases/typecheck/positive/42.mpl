data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun foldtest :: Nat -> Nat =
    a -> fold a of
        Succ : b -> Succ(b)
        Zero : -> Zero
