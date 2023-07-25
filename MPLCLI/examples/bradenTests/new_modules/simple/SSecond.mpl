
fun (++) :: [Char],[Char] -> [Char] =
    [],c -> c
    b:bs,c -> b:(bs ++ c)

fun f :: Int -> [Char] =
    n -> if n < 1 then [] else '+':f(n - 1)