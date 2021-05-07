data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data 
    Rose(A) -> S =
        Branches :: A, T -> S
    and
    Forest(A) -> T =
        ForestCons :: S,T -> T
        ForestNil ::      -> T

data Wrapper(A) -> S =
    Wrapper :: A -> S

fun foldtest  =
    a -> fold a of
        Branches : v, Wrapper(rst) -> rst
        ForestCons : a,Wrapper(b) -> 
            Wrapper(
                    fold a of
                        Succ : a -> Succ(a)
                        Zero : -> b
                )
        ForestNil : -> Wrapper(Zero)
