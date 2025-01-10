coprotocol S => Console = 
    ConsoleGet :: S => Put([Char] | S) 
    ConsolePut :: S => Get([Char] | S) 
    ConsoleClose :: S => TopBot

protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S


data Maybe(A) -> C =
    Just :: A -> C 
    Nothing :: -> C


data SF(A) -> C =
    SS :: A -> C 
    FF :: -> C

codata C->Mymealy(A, B) = 
    Step :: A,C -> (B, C)


fun strlen :: [A] -> Int = 
    [] -> 0
    s:ss -> (1 + (strlen (ss)))
   
    
fun mypower :: Int, Int -> Int =
    x,0 -> 1
    x,1 -> x
    x,n -> (x * ( mypower (x, (n + (-1)))))



fun char2int :: Char -> Maybe(Int) = 
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
     -- in the use cases of this program, this will never happen!
    _ -> Nothing




fun str2int :: [Char] -> Maybe(Int) = 
    [] -> Just(0)
    s:ss -> case char2int(s) of
            Just(sb) -> case str2int(ss) of 
                        Just(d) -> Just (((sb) * (mypower(10,strlen (ss)))) + d)
                        _ -> Nothing
            _ -> Nothing


codata Z -> Fun (A, B) =
	Fn :: A, Z -> B

protocol 
    Transaction ( | ) => Z =
                    GetNum :: Put (Int|(Get (Mymealy(Int,[Int]) | Z))) => Z
                    Leave :: TopBot => Z 




fun divqr :: Int, Int -> (Int, Int) =
	a, b -> if a < b
	    then (0, a)
	    else case divqr(a - b, b) of
		(q, r) ->  (q + 1, r)

defn
    fun showInt :: Int -> [Char] =
        0 -> ['0']
        n -> go(n, [])	
where
    -- this does integer division for a,b as input
    -- and outputs (q,r) where q is the integer division and r 
    -- is the remainder.

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
        -- in the use cases of this program, this will never happen!
        _ -> '#'

    fun go :: Int, [Char] -> [Char] =
        0, acc -> acc
        n, acc -> case divqr(n, 10) of
            (q, r) -> go(q, intToDigit(r):acc)





fun land :: Bool, Bool -> Bool =
    True, True -> True
    _,_ -> False

fun append :: [A],[A] -> [A] =
    [],ts -> ts
    s:ss,ts -> s : append(ss,ts)

fun concat :: [[A]] -> [A] =
	[] -> []
	s:ss -> append(s, concat(ss))

fun stringEqual :: [Char], [Char] -> Bool =
    [],[] -> True
    x:xs,y:ys -> land (x == y, stringEqual (xs,ys))
    _,_ -> False

fun unfoldmealy :: Fun((A,C),(B,C)), C -> Mymealy(A,B) = 
    f, c -> (Step := a -> case (Fn ((a,c), f)) of
                            (b,cp ) -> (b, (unfoldmealy(f,cp)))
            )
   


fun crlist :: Int, Int -> [Int] = 
    a, b -> if (a < b) 
                then []
                else (b: crlist(a,(b+1))) 
            

fun inffun :: Int, Int -> ([Int], Int) = 
    a, b -> (crlist(b, a), a+b)

fun infmeal ::  -> Mymealy (Int, [Int]) = 
    -> unfoldmealy ((Fn:=(a,b) -> inffun (a,b)),0)


fun printlist :: [Int] -> [Char] = 
    [] -> " ]"
    (x:xs) -> append(append(showInt(x)," "), printlist(xs))  

fun fst :: (A,B) -> A =
    (a, b) -> a 

fun snd :: (A,B) -> B =
    (a, b) -> b



proc client :: | => StringTerminal, Transaction =
    | => strterm, chA -> do
        hput StringTerminalPut on strterm
        put "please enter a number" on strterm
        hput StringTerminalGet on strterm
        get num on strterm
        case str2int(num) of 
            Just (numchar) -> do hput GetNum on chA  
                                 put numchar on chA
                                 get mealmach on chA
                                 hput StringTerminalPut on strterm
                                 put append("[ ",printlist(fst(Step(numchar, mealmach)))) on strterm
                                 hput StringTerminalPut on strterm
                                 put "Do you want to continue?" on strterm
                                 hput StringTerminalGet on strterm
                                 get ans on strterm
                                 if (stringEqual (ans, "yes"))
                                    then do 
                                        client(| => strterm, chA)
                                    else do 
                                        hput StringTerminalClose on strterm
                                        close strterm
                                        hput Leave on chA
                                        halt chA
            _ ->  do 
                    client(| => strterm, chA)
        


proc server :: Mymealy (Int,[Int]) | Transaction => StringTerminal =
    mealmach | chA => strterm -> hcase chA of
        GetNum -> do
            get num on chA
            put mealmach on chA
            server ( snd(Step(num, mealmach))| chA => strterm)
        Leave -> do
            hput StringTerminalPut on strterm
            put "Ok! Bye" on strterm
            hput StringTerminalClose on strterm
            close strterm
            halt chA



proc run :: | => StringTerminal =
          | => strtermC -> plug
                 client( | => strtermC, ch)
                 server( infmeal() | ch => strtermC)

