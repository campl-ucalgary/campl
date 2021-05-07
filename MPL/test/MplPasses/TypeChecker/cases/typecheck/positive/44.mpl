data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data Wrapper(A) -> S =
    Wrapper :: A -> S

data Unit -> S =
    Unit :: -> S

codata S -> Tuple(A,B,C) =
    P1 :: S -> A
    P2 :: S -> B
    P3 :: S -> C

fun unfoldtest :: Nat -> Tuple(Nat, Wrapper(Nat), Unit) =
    a -> unfold a of
        Succ(b) of
            P1 : -> b
            P2 : -> Wrapper(b)
            P3 : -> Unit
