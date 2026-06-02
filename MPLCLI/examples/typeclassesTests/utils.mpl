data Maybe(A) -> S =
    Just :: A -> S
    Nothing :: -> S

codata S -> Fun(A,B) = 
    App :: A,S -> B

codata S -> Int_Show_Dict =
    ShowInt :: S -> Fun(Int, [Char])

codata S -> Int_Parse_Dict =
    ParseInt :: S -> Fun([Char], Maybe(Int))

defn 
    fun parseInt :: [Char] -> Maybe(Int) =
        [] -> Nothing
        str -> convert(Just(0), str)

where
    fun charToDigit :: Char -> Maybe(Int) =
        '0' -> Just(0)
        '1' -> Just(1)
        '2' -> Just(2)
        '3' -> Just(3)
        '4' -> Just(4)
        '5' -> Just(5)
        '6' -> Just(6)
        '7' -> Just(7)
        '8' -> Just(8)
        '9' -> Just(9)
        _   -> Nothing


    fun step :: Maybe(Int),Char -> Maybe(Int) =
        Nothing, _ -> Nothing
        Just(n), c -> case charToDigit(c) of
            Just(nn) -> Just(n * 10 + nn)
            Nothing -> Nothing

    fun convert :: Maybe(Int),[Char] -> Maybe(Int) =
        Nothing, _ -> Nothing
        res, [] -> res
        res, s:ss -> convert(step(res,s), ss)


defn
    fun showInt :: Int -> [Char] =
        0 -> "0"
        n -> convert(n, [])
where
    fun intToDigit :: Int -> Char =
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        _ -> '#'

    fun divqr :: Int, Int -> (Int, Int) =
        a, b -> if a < b
            then (0, a)
            else case divqr(a - b, b) of
                (q, r) ->  (q + 1, r)

    fun convert :: Int, [Char] -> [Char] =
        0, acc -> acc
        n, acc -> case divqr(n, 10) of
            (q, r) -> convert(q, intToDigit(r):acc)

fun int_show_dict :: -> Int_Show_Dict =
    -> (ShowInt := -> (App := i -> showInt(i)))

fun int_parse_dict :: -> Int_Parse_Dict =
    -> (ParseInt := -> (App := s -> parseInt(s)))