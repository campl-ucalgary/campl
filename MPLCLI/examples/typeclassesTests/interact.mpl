protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

data Maybe(A) -> S =
    Just :: A -> S
    Nothing :: -> S

protocol InOut(A, B | ) => S =
    InOut :: Get(A | Put(B | S)) => S

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

-- class Interact T where
-- pinteract :: | T => Console

-- instance <Show B, Parse A> => Interact InOut(A, B | ) where
--      proc pinteract :: <Show B, Parse A> => | InOut(A, B | ) => Console =
--          | ch => strterm -> ...

-- translation of pinteract
proc pinteract :: Fun(B, [Char]), Fun([Char], Maybe(Int)) | InOut(Int, B | ) => StringTerminal =
    showB, parseA | ch => strterm -> do
        hput StringTerminalPut on strterm
        put "enter an integer" on strterm
        hcase ch of
            InOut -> do
                hput StringTerminalPut on strterm
                put "enter an integer" on strterm
                hput StringTerminalGet on strterm
                get input on strterm
                case App(input, parseA) of
                    Just(a_val) -> do
                        put a_val on ch
                        get b_val on ch
                        hput StringTerminalPut on strterm
                        put App(b_val, showB) on strterm
                        pinteract( showB, parseA | ch => strterm)
                    Nothing -> do
                        hput StringTerminalPut on strterm
                        put "error parsing the input" on strterm
                        put 1 on ch
                        get _ on ch
                        pinteract( showB, parseA | ch => strterm)



-- type of the dictionary (package) that would be generated for "instance Interact InOut"
-- codata S -> InOut_Interact_Dict(A,B)  =
--        Pinteract :: S -> (InOut(A, B | ) => Console)

-- the actual dictionary (package) that would be generated for "instance Interact InOut"
fun int_show_dict :: -> Int_Show_Dict =
    -> (ShowInt := -> (App := i -> showInt(i)))

fun int_parse_dict :: -> Int_Parse_Dict =
    -> (ParseInt := -> (App := s -> parseInt(s)))

proc test :: Int | Console => InOut(Int, Int |) =
    val | console => ch -> do
        hput ConsolePut on console
        put "waiting for input" on console
        hput InOut on ch
        get inp on ch
        hput ConsolePut on console
        put "got input" on console
        hput ConsolePut on console
        put showInt(inp) on console
        put val on ch
        test(val | console => ch)

proc run :: | Console => StringTerminal =
    | console => strterm ->
        plug
            test(8123 | console => a)
            pinteract(ShowInt(int_show_dict), ParseInt(int_parse_dict)| a => strterm)
