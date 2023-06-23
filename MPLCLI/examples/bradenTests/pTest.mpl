protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S






-- Takes two positive ints (a/b), returns their quotient and remainder.
-- Works by repeated subtraction
fun div :: Int,Int -> (Int,Int) =
    a,b -> if a < b
        then (0,a)
        else case div(a-b,b) of
            (q,r) -> (q+1,r)

fun timesTen :: Int -> Int =
    a -> a * 10

-- Generates a list [...,100,10,1] where the first item is slightly smaller than 'a'
-- fun tensL :: Int,[Int] -> [Int] =
    -- a,b:bs -> if timesTen(b) > a then b:bs else tensL(a,timesTen(b):b:bs)
    -- _,_ -> []

fun tensL :: Int,[Int] -> [Int] =
    a,b:bs -> if a > b then [1] else [1]
    _,[] -> []


proc run =
    | => sTerm0 -> do
        hput StringTerminalPut on sTerm0
        put "Enter a positive integer:" on sTerm0
        
        hput StringTerminalGet on sTerm0
        get numStr on sTerm0
        
        hput StringTerminalPut on sTerm0
        put "hi" on sTerm0
        
        hput StringTerminalGet on sTerm0
        get _ on sTerm0
        
        hput StringTerminalClose on sTerm0
        halt sTerm0