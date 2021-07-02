
-- some eq 
fun testing :: Int -> Bool =
    a -> a == 2

fun testing :: Int -> Bool =
    a -> 2 == a

fun testing :: Int -> Bool =
    a -> 2 == a


-- some eq 
fun testing :: Int -> Bool =
    a -> a /= 2

fun testing :: Int -> Bool =
    a -> 2 /= a

fun testing :: Int -> Bool =
    a -> 2 /= a


-- some inequalities
fun testing :: Int -> Bool =
    a -> a <= 2

fun testing :: Int -> Bool =
    a -> a < 2

fun testing :: Int -> Bool =
    a -> a > 2

fun testing :: Int -> Bool =
    a -> a >= 2

-- some lists
fun testing :: A, [A] -> [A] =
    h,rst -> h : rst

fun testing :: (), [()] -> [()] =
    h,rst -> () : rst

fun testing :: () -> () =
    () -> ()

fun testing :: [A] -> A =
    a:b -> a

fun testing :: [A] -> [A] =
    a:b -> b

fun testing :: [A] -> A =
    [a] -> a

fun testing :: [A] -> () =
    [] -> ()

fun testing :: [Char] -> () =
    "a" -> ()

fun testing :: [Char] -> [Char] =
    "a" -> "b"

fun testing :: [Char] -> [Char] =
    ['a',b] -> ['a', b]

