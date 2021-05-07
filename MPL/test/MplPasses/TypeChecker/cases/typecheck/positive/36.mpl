data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

proc v36 :: Nat | TopBot => =
    a |  b => -> 
        case a of
            Succ(a) -> do
                halt b
            Zero -> do
                halt b
