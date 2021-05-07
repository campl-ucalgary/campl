codata S -> Tuple(A,B) =
    P0 :: S -> A
    P1 :: S -> B

fun prj0 :: Tuple(A,B) -> A =
    (P0 := a, P1 := b) -> a
fun prj1 :: Tuple(A,B) -> B =
    (P0 := a, P1 := b) -> b
