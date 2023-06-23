protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

-- Append two lists
fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

-- Turns a string into an int. The second parameter prepends digits to the result.
fun stoiH :: Int,[Char] -> Int =
    d,'0':r -> stoiH((d*10)+0,r)
    d,'1':r -> stoiH((d*10)+1,r)
    d,'2':r -> stoiH((d*10)+2,r)
    d,'3':r -> stoiH((d*10)+3,r)
    d,'4':r -> stoiH((d*10)+4,r)
    d,'5':r -> stoiH((d*10)+5,r)
    d,'6':r -> stoiH((d*10)+6,r)
    d,'7':r -> stoiH((d*10)+7,r)
    d,'8':r -> stoiH((d*10)+8,r)
    d,'9':r -> stoiH((d*10)+9,r)
    d,_ -> d -- base case. Unrecognized characters or end-of-string. Either way, finish.


-- String to int.
fun stoi :: [Char] -> Int =
    a -> stoiH(0,a)

defn
    -- returns the square root, rounded down.
    fun sqrt :: Int -> Int =
        0 -> 0
        1 -> 1 -- For convenience; not necessary.
        a -> sqrtH1(a,1,3,1)

where
    -- Square root helper 2: exact search
    -- Takes:
    --  the number,
    --  a perfect square,
    --  the increment to reach the next square
    --  the sqrt of the perfect square
    fun sqrtH2 :: Int,Int,Int,Int -> Int =
        a,b,c,d -> switch
            a < b + c -> d -- We found the sqrt, and rounded down
            a == b + c -> d -- We found an exact sqrt
            True -> sqrtH2(a,b+c,c+2,d+1) -- d++, then try again.
    
    -- Square root helper 1: fast search
    -- Takes:
    --  the number,
    --  a perfect square,
    --  the increment to reach the next square
    --  the sqrt of the perfect square
    fun sqrtH1 :: Int,Int,Int,Int -> Int =
        a,b,c,d -> switch
            a < b * 4 -> sqrtH2(a,b,c,d) -- d cannot double again
            True -> sqrtH1(a,b*4,(c+c) - 1,d+d) -- double the value of d
    
    


-- Takes two positive ints (a/b), returns their quotient and remainder.
-- Works by repeated subtraction. (New and improved: now with bit-shifts!)
fun div :: Int,Int -> (Int,Int) =
    a,b -> switch
        a < b -> (0,a) -- base case: only remainder left
        b * 2 < a -> case div(a,b+b) of -- Recursive case: double the divisor, and solve.
            (q,r) -> case div(r,b) of
                (q2,r2) -> ((q * 2) + q2, r2)
        True -> case div(a-b,b) of -- Recursive case: repeated subtraction.
            (q,r) -> (q+1,r)

-- Generates a list [...,100,10,1] where the first item is slightly smaller than 'a'
fun tensL :: Int,[Int] -> [Int] =
    a,b:bs -> if a < b*10 then b:bs else tensL(a,(b*10):b:bs)
    _,_ -> [] -- Should never happen

-- int to character
fun itoc :: Int -> Char =
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
    _ -> '?'

-- Takes a positive integer, a list of ints, and returns a string.
-- The list contains [1000,100,10,1] (or something of that format, up to the size of the int.)
fun itosH :: Int,[Int] -> [Char] =
    a,b:bs -> case div(a,b) of
        (q,r) -> itoc(q):itosH(r,bs)
    a,[] -> [] -- The base case.

-- int to string
fun itos :: Int -> [Char] =
    a -> itosH(a,tensL(a,[1]))

-- checks if the second number is a factor of the first.
fun isFactor :: Int,Int -> Bool =
    a,b -> switch
        a < b -> False -- b is too large
        a == b -> False -- a number is not its own factor, for our purposes.
        b == 0 -> False -- 0 is a factor of nothing.
        True -> case div(a,b) of
            (q,0) -> True
            (q,r) -> False

-- Checks the small factors
fun smallFactor :: Int -> Int =
    a -> switch
        isFactor(a,2) -> 2
        isFactor(a,3) -> 3
        True -> 0

defn
    -- Calculates the expected number of threads, equal to log_10(a)
    fun logCheck :: Int -> Int =
        a -> logCH(a,1,1)

where
    fun logCH :: Int,Int,Int -> Int =
        a,b,c -> switch
            b < a -> logCH(a,b*10,c+1)
            True -> c

-- Race, propagating the first non-zero value upward.
proc racer :: | Put(Int|TopBot),Put(Int|TopBot) => Put(Int|TopBot) =
    | a,b => c -> do
        race
            a -> do
                get valA on a
                close a
                if valA == 0
                    then do
                        get valB on b
                        close b
                        put valB on c
                        halt c
                    else do
                        put valA on c
                        close c
                        get _ on b
                        halt b
            b -> do
                get valB on b
                close b
                if valB == 0
                    then do
                        get valA on a
                        close a
                        put valA on c
                        halt c
                    else do
                        put valB on c
                        close c
                        get _ on a
                        halt a

fun floorDiv :: Int,Int -> Int =
    a,b -> case div(a,b) of
        (q,_) -> q

fun findFactor :: Int,Int,Int,Int -> Int =
    v,s,e,i -> switch
        e < s -> 0 -- Reached the end, no factor
        True -> case div(v,s - 1) of -- check s-1
            (_,r) -> if r == 0
                then s - 1 -- Factor found
                else case div(v,s+1) of -- check s+1
                    (_,r2) -> if r2 == 0
                        then s + 1 -- Factor found
                        else findFactor(v,s+i,e,i) -- no factor found; iterate.

-- searches for factors, or splits if there are too many to search.
-- Takes val,start,end,interval, and thread count. Returns a factor, or 0 if none is found.
proc solver :: Int,Int,Int,Int,Int | => Put(Int|TopBot) =
    val,start,end,interval,tCount | => res -> do
        if 1 < tCount
            then do
                plug -- split and recurse
                    racer(| a,b => res)
                    solver(val,start,end,interval+interval,floorDiv(tCount,2) | => a)
                    solver(val,start+interval,end,interval+interval,tCount-floorDiv(tCount,2) | => b)
            else do -- Solve and return result
                put findFactor(val,start,end,interval) on res
                halt res

-- sets up the tree, and handles simple cases (factors 2 or 3)
proc resolver :: Int | => Put(Int|TopBot) =
    val | => res -> do
        if 0 < smallFactor(val)
            then do -- small factor found.
                put smallFactor(val) on res
                halt res
            else do
                solver( val,6,sqrt(val),6,logCheck(val) | => res)

-- A process that writes the result to the console
proc outputter :: | Put(Int|TopBot) => StringTerminal =
    | result => sTerm0 -> do
        get res on result
        close result
        
        if 0 < res
            then do
                hput StringTerminalPut on sTerm0
                put "Number is not prime. Found factor:" on sTerm0
                
                hput StringTerminalPut on sTerm0
                put itos(res) on sTerm0
                
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0
            else do
                hput StringTerminalPut on sTerm0
                put "Number is prime" on sTerm0
        
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0

proc run =
    | => sTerm0 -> do
        hput StringTerminalPut on sTerm0
        put "Enter a positive integer:" on sTerm0
        
        hput StringTerminalGet on sTerm0
        get numStr on sTerm0
        
        if stoi(numStr) < 2
            then do
                hput StringTerminalPut on sTerm0
                put "Number is too small; not prime" on sTerm0
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0
            else do
        
                hput StringTerminalPut on sTerm0
                put "Expected thread count:" on sTerm0
                
                hput StringTerminalPut on sTerm0
                put itos(logCheck(stoi(numStr))) on sTerm0
                
                plug -- Resolver -> outputter -> console
                    resolver(stoi(numStr) | => res)
                    outputter( | res => sTerm0)