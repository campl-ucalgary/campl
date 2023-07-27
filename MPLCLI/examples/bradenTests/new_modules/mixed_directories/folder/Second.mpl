
fun f :: Int -> [Char] =
    n -> if n < 1 then [] else '+':f(n - 1)