data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data NegNat -> S =
    Pred :: S -> S
    NZero ::   -> S

codata S -> Tuple(A,B) =
    P1 :: S -> A
    P2 :: S -> B

codata 
    S -> Zig(A,B) =
        HeadA :: S -> A
        TailA :: S -> T
    and 
    T -> Zag(A,B) =
        HeadB :: T -> B
        TailB :: T -> S
        
fun v45 :: -> Zig(NegNat(), Nat())=
    -> unfold (P1 := -> NZero, P2 := -> Zero) of
        (P1 := n, P2 := i) of
            HeadA : -> n
            TailA : -> (P1 := -> i, P2 := -> Pred(n))
        (P1 := i, P2 := n) of
            HeadB : -> i
            TailB : -> (P1 := -> n, P2 := -> Succ(i))
