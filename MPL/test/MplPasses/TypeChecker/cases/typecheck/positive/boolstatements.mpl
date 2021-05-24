fun myif0  =
    a,b,c ->  if a then b else c

fun myif1 :: Bool,A,A -> A  =
    a,b,c ->  if a then b else c

fun myswitch :: Bool, A, Bool -> A=
    a,b,c ->  switch 
        a -> b
        c -> b

