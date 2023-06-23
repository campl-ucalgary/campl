-- An advanced prime-number checker. Creates ceil(log(input)) threads, organized in a balanced tree.
-- Each thread checks a subset of the numbers. 
-- Each worker has a 'secretary', which handles interactions with the rest of the system.

protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get([Char]|S)
    ConsoleGet :: S => Put([Char]|S)
    ConsoleClose :: S => TopBot
    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

-- Append two lists
fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)



defn
    -- String to int.
    fun stoi :: [Char] -> Int =
        a -> stoiH(0,a)
where
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

fun floorDiv :: Int,Int -> Int =
    a,b -> case div(a,b) of
        (q,_) -> q

-- -------------------------------------------------------------------------------
-- Built-in functions above

-- A tree. use '-1' to say 'None'
data Tree(A) -> T =
    Leaf :: A -> T
    Node :: A,T,T -> T

protocol Find => F =
    Listen :: Get(Bool|F) => F -- Check for termination. True=done.
    Speak :: Put(Int|Put(Tree(Int)|TopBot)) => F -- Yup, done.

protocol ToBoss => G =
    Await :: Get(Int|G) => G -- Waits for a worker to have something to say.
    Conclude :: TopBot => G -- Tells a worker to quit.

-- like int-to-string, but converts -1 to 'None'
fun itos2 :: Int -> [Char] =
    -1 -> "None"
    n -> itos(n)

-- Adds the requested indentation to a string.
fun indent :: [Char],Int -> [Char] =
    s,0 -> s
    s,n -> ' ':' ':' ':' ':indent(s,n - 1)

fun printTree :: Tree(Int),Int ->[Char] =
    Node(i,t1,t2),ind -> append(append(indent(itos2(i),ind),'\n':printTree(t1,ind+1)),'\n':printTree(t2,ind+1))
    Leaf(i),ind -> indent(itos2(i),ind)

defn
    -- Calculates the expected number of threads, equal to log_10(a)
    fun logCheck :: Int -> Int =
        a -> logCH(a,1,1)

where
    fun logCH :: Int,Int,Int -> Int =
        a,b,c -> switch
            b < a -> logCH(a,b*10,c+1)
            True -> c

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
    proc aug =
        | a,b => c -> do
            hcase b of
                Listen -> do
                    hput Listen on c
                    get mess on c
                    put mess on b
                    aug(|a,b=>c)
                Speak -> do
                    get r on b
                    hput Speak on c
                    put r on c
                    get t1 on a
                    get t2 on b
                    close a
                    close b
                    put Node(r,t1,t2) on c
                    halt c
    
    -- For when the value is already known
    proc aug2 =
        v | a,b => c -> do
            hcase b of
                Listen -> do
                    put True on b
                    aug2(v|a,b=>c)
                Speak -> do
                    get _ on b
                    get t1 on a
                    get t2 on b
                    close a
                    close b
                    put Node(v,t1,t2) on c
                    halt c
    
    proc joiner =
        | a,b => c -> do
            hcase a of
                Listen -> do
                    hcase b of
                        Listen -> do -- Both listen. Send request to root.
                            hput Listen on c
                            get mess on c
                            put mess on b
                            put mess on a
                            joiner(|a,b=>c)
                        Speak -> do -- b speaks. Thus, an answer may have been found.
                            get v on b
                            if v == 0
                                then do
                                    put False on a
                                    aug(|b,a=>c)
                                else do
                                    hput Speak on c
                                    put v on c
                                    put True on a
                                    aug2(v|b,a=>c)
                Speak -> do
                    hcase b of
                        Listen -> do -- a speaks. Thus, an answer may have been found.
                            get v on a
                            if v == 0
                                then do
                                    put False on b
                                    aug(|a,b=>c)
                                else do
                                    hput Speak on c
                                    put v on c
                                    put True on b
                                    aug2(v|a,b=>c)
                        Speak -> do
                            get v on a
                            get v2 on b
                            hput Speak on c
                            if v2 < v
                                then do
                                    put v on c
                                    get t1 on a
                                    get t2 on b
                                    put Node(v,t1,t2) on c
                                    close a
                                    close b
                                    halt c
                                else do
                                    put v2 on c
                                    get t1 on a
                                    get t2 on b
                                    put Node(v2,t1,t2) on c
                                    close a
                                    close b
                                    halt c
    -- The secretary. Controls two chanels, and ensures both parties can talk whenever they need.
    proc secr =
        | => w,res -> do
            race
                w -> do -- The worker speaks
                    secrW(|=>w,res)
                res -> do
                    secrR(|=>w,res)
    
    proc secrR =
        | => w, res -> do
            get mess on res
            if mess
                then do -- Answer found; close with non-answer.
                    get _ on w
                    hput Conclude on w
                    close w
                    hput Speak on res
                    put -1 on res
                    put Leaf(-1) on res
                    halt res
                else do -- No answer found. Listen to worker next.
                    hput Listen on res
                    secrW(|=>w,res)
    
    -- secretary, but waiting for worker message.
    proc secrW =
        | => w, res -> do
            get mess on w
            if -1 < mess
                then do -- Found an answer
                    get _ on res
                    hput Speak on res
                    put mess on res
                    put Leaf(mess) on res
                    hput Conclude on w
                    close w
                    halt res
                else do -- no answer found. Reset.
                    hput Await on w
                    secrR(| => w,res)
    
    -- Sets up
    proc secrH =
        | => w,res -> do
            hput Listen on res
            hput Await on w
            secr(|=>w,res)
    
    
    proc worker =
        val,start,end,interval | res => -> do
            hcase res of
                Conclude -> do
                    halt res
                Await -> do
                    if end < start
                        then do
                            put 0 on res
                            worker(val,start,end,interval|res=>) -- repeat as-is. We are done.
                        else do
                            if isFactor(val,start - 1)
                                then do
                                    put (start - 1) on res
                                    worker(val,start,end,interval|res=>) -- repeat as-is. We are done.
                                else do
                                    if isFactor(val, start + 1)
                                        then do
                                            put (start + 1) on res
                                            worker(val,start,end,interval|res=>) -- repeat as-is. We are done.
                                        else do
                                            put -1 on res
                                            worker(val,start+interval,end,interval|res=>) -- repeat as-is. We are done.

proc splitter :: Int,Int,Int,Int,Int | => Find(|) =
    val,start,end,interval,tCount | => res -> do
        if 1 < tCount
            then do
                plug -- split and recurse
                    joiner(| ca,cb => res)
                    splitter(val,start,end,interval+interval,floorDiv(tCount,2) | => ca)
                    splitter(val,start+interval,end,interval+interval,tCount-floorDiv(tCount,2) | => cb)
            else do
                plug
                    secrH(| => s,res)
                    worker(val,start,end,interval | s =>)

proc resolver =
    val | => res -> do
        if 0 < smallFactor(val)
            then do -- small factor found.
                hput Speak on res
                put smallFactor(val) on res
                put Leaf(smallFactor(val)) on res
                halt res
            else do
                splitter( val,6,sqrt(val),6,logCheck(val) | => res)
        

-- Waits for an answer, which it propagates to the outputter.
proc tester :: | Find(|) => Put(Int|Put(Tree(Int)|TopBot)) =
    | a => c -> do
        hcase a of
            Listen -> do
                put False on a
                tester(|a=>c)
            Speak -> do
                get r on a
                put r on c
                get t on a
                put t on c
                close a
                halt c

-- A process that writes the result to the console. Same as before
proc outputter :: | Put(Int|Put(Tree(Int)|TopBot)) => StringTerminal =
    | result => sTerm0 -> do
        get res on result
        
        if 0 < res
            then do
                hput StringTerminalPut on sTerm0
                put "Number is not prime. Found factor:" on sTerm0
                
                hput StringTerminalPut on sTerm0
                put itos(res) on sTerm0
                
                hput StringTerminalPut on sTerm0
                put "Process tree:\n" on sTerm0
                
                get resTree on result
                close result
                hput StringTerminalPut on sTerm0
                put printTree(resTree,0) on sTerm0
                
                hput StringTerminalPut on sTerm0
                put "\n\nPress enter to exit" on sTerm0
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0
            else do
                hput StringTerminalPut on sTerm0
                put "Number is prime" on sTerm0
                
                hput StringTerminalPut on sTerm0
                put "Process tree:\n" on sTerm0
                
                get resTree on result
                close result
                hput StringTerminalPut on sTerm0
                put printTree(resTree,0) on sTerm0
                
                
                hput StringTerminalPut on sTerm0
                put "\n\nPress enter to exit" on sTerm0
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
                    resolver(stoi(numStr) | => resT)
                    tester(| resT => res)
                    outputter( | res => sTerm0)