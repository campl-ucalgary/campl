-- This is an enriched version of primeFinder.mpl.
-- It still does everything the regular primeFinder does, as well as printing a tree.
-- The nodes of the tree represent racers, and display 'racer:someInt', saying what result they propagated (not who won the race)
-- The leaves are workers, and display the result they generated.



protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S


-- A tree. Every node has linked data.
data Tree(A) -> U =
    Leaf :: A -> U
    Node :: A,U,U -> U
    LeafNone :: -> U
    NodeNone :: U,U -> U

-- A coprotocol for either requesting a factor or saying 'a factor was already found'
coprotocol Mssg => T =
    GetFactor :: T => Put(Int | T)
    GetTree :: T => Put(Tree(Int) | TopBot)


-- Append two lists
fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)


-- Adds the requested indentation to a string.
fun indent :: [Char],Int -> [Char] =
    s,0 -> s
    s,n -> ' ':' ':' ':' ':indent(s,n - 1)

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
        b + b < a -> case div(a,b+b) of -- Recursive case: double the divisor, and solve.
            (q,r) -> case div(r,b) of
                (q2,r2) -> (q + q + q2, r2)
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

-- Prints a tree with a given level of indentation.
fun printTree :: Tree(Int),Int ->[Char] =
    Node(i,t1,t2),ind -> append(append(indent(itos(i),ind),'\n':printTree(t1,ind+1)),'\n':printTree(t2,ind+1))
    Leaf(i),ind -> indent(itos(i),ind)
    NodeNone(t1,t2),ind -> append(append(indent("None",ind),'\n':printTree(t1,ind+1)),'\n':printTree(t2,ind+1))
    LeafNone(),ind -> indent("None",ind)

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


-- A process that represents a fully-solved subtree.
-- It simply feeds its results out until it is told to terminate.
-- proc tCap :: Int,Tree(Int) | => T =
proc tCap =
    a,b | => c -> do
        hcase c of
            GetFactor -> do -- send int and recurse
                put a on c
                tCap(a,b|=>c)
            GetTree -> do -- send tree and halt.
                put b on c
                halt c


defn
    -- Calculates the expected number of threads, equal to log_10(a)
    fun logCheck :: Int -> Int =
        a -> logCH(a,1,1)
    
where
    fun logCH :: Int,Int,Int -> Int =
        a,b,c -> switch
            b < a -> logCH(a,b*10,c+1)
            True -> c


defn

    -- Race, propagating the first non-zero value upward.
    -- Now handles early termination: the forward channel may choose to call for an early termination.
    -- The 'propFlag' ensures that 'are we there yet' messages are only propagated every second time.
    -- proc racer :: Bool | Put(Int|T),Put(Int|T) => Put(Int|T) =
    proc racer =
        propFlag | a,b => c -> do
            race
                a -> do
                    racerFound(propFlag|a,b => c)
                b -> do
                    racerFound(propFlag|b,a => c)
    
    -- The racer, where the first channel has a message ready to recieve.
    -- proc racerFound :: Bool | Put(Int|T),Put(Int|T) => Put(Int|T) =
    proc racerFound =
        propFlag | a,b, => c -> do
            -- 'a' has a message ready on it.
            get m on a
            case m of
                -1 -> do -- propagate 'are we there yet' message
                    if propFlag
                        then do
                            put -1 on c
                            racerCase(|a,b=>c) -- handle the response
                        else do
                            hput GetFactor on a
                            racer(True|a,b=>c) -- Recursively call.
                0 -> do
                    hput GetTree on a
                    get t on a
                    close a
                    augmenter(t|b=>c) -- Create a psuedo-channel, which augments any 'GetTree' response
                q -> do -- A result was recieved.
                    put q on c
                    -- Set up a tCap, since a terminal result was recieved.
                    hput GetTree on a
                    get _ on b
                    hput GetTree on b
                    get t1 on a
                    get t2 on b
                    close a
                    close b
                    tCap(q,Node(q,t1,t2)|=>c)
    
    -- determines how to reset the racer, based on what the 'out' channel asks of it.
    -- proc racerCase :: | T,Put(Int|T) => T =
    proc racerCase =
        | a,b => c -> do
            hcase c of
                GetFactor -> do -- reset
                    hput GetFactor on a
                    racer(False|a,b=>c) -- we can infer 'false' from where it gets called.
                GetTree -> do -- Early termination request.
                    -- Tell both subtrees to quit
                    hput GetTree on a
                    get _ on b -- Throw the int result away
                    hput GetTree on b
                    -- Get the process tree from each.
                    get t1 on a
                    get t2 on b
                    close a
                    close b
                    -- Send the process tree upward.
                    put NodeNone(t1,t2) on c
                    halt c
    
    -- Takes a process tree. Acts like a channel for the purposes of GetFactor,
    -- But augments any GetTree result with the given subtree.
    -- proc augmenter :: Tree(Int) | Put(Int|T) => Put(Int|T) =
    proc augmenter =
        t1 | a => c -> do
            get m on a
            put m on c
            hcase c of
                GetFactor -> do
                    hput GetFactor on a
                    augmenter(t1|a=>c)
                GetTree -> do
                    hput GetTree on a
                    get t2 on a
                    close a
                    case m of -- Detect early termination vs regular termination.
                        -1 -> do
                            put NodeNone(t1,t2) on c
                            halt c
                        n -> do
                            put Node(n,t1,t2) on c
                            halt c




fun floorDiv :: Int,Int -> Int =
    a,b -> case div(a,b) of
        (q,_) -> q

-- Checks the current position for a factor.
-- 0 means no factor found.
fun findFactorStep :: Int,Int -> Int =
    v,s -> switch
        isFactor(v,s - 1) -> s - 1
        isFactor(v,s + 1) -> s + 1
        True              -> 0

-- A helper process. Takes an int result, and generates a tCap.
proc solverH :: Int | => T =
-- proc solverH =
    val | => res -> do
        tCap(val,Leaf(val)|=>res)

-- searches for factors, or splits if there are too many to search.
-- Takes val,start,end,interval. Returns a factor, or 0 if none is found.
-- proc solver :: Int,Int,Int,Int | => Put(Int|T) =
proc solver =
    val,start,end,interval| => res -> do
        if end < start
            then do -- Done searching
                put 0 on res
                solverH(0|=>res)
            else do
                case findFactorStep(val,start) of
                    0 -> do
                        put -1 on res -- Are we there yet?
                        hcase res of
                            GetFactor -> do -- Try the next possibility
                                solver(val,start+interval,end,interval|=>res)
                            GetTree -> do -- Early termination
                                put LeafNone on res
                                halt res
                    n -> do
                        put n on res
                        solverH(n|=>res) -- Create tree cap

-- Splits the task into multiple threads, generating the process tree as it goes.
-- Passes the problem to 'solver' once it is done.
-- Takes val,start,end,interval, and thread count.
-- proc splitter :: Int,Int,Int,Int,Int | => Put(Int|T) =
proc splitter =
    val,start,end,interval,tCount | => res -> do
        if 1 < tCount
            then do
                plug -- split and recurse
                    racer(False| a,b => res)
                    splitter(val,start,end,interval+interval,floorDiv(tCount,2) | => a)
                    splitter(val,start+interval,end,interval+interval,tCount-floorDiv(tCount,2) | => b)
            else do -- Solve and return result. (passes to a helper, to send result followed by leaf(result))
                solver(val,start,end,interval| => res)

-- sets up the tree, and handles simple cases (factors 2 or 3)
-- proc resolver :: Int | => Put(Int|T) =
proc resolver =
    val | => res -> do
        case smallFactor(val) of
            0 -> splitter( val,6,sqrt(val),6,logCheck(val) | => res)
            n -> do -- small factor found.
                put n on res
                solverH(n|=>res)

-- A process that writes the result to the console
-- proc outputter :: | Put(Int|T) => StringTerminal =
proc outputter =
    | result => sTerm0 -> do
        get res on result
        
        
        case res of
            -1 -> do
                hput GetFactor on result
                outputter(|result => sTerm0)
            0 -> do
                hput StringTerminalPut on sTerm0
                put "Number is prime" on sTerm0
                
                hput StringTerminalPut on sTerm0
                put "Process tree:\n" on sTerm0
                
                hput GetTree on result
                get resTree on result
                close result
                hput StringTerminalPut on sTerm0
                put printTree(resTree,0) on sTerm0
        
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0
            n -> do
                hput StringTerminalPut on sTerm0
                put "Number is not prime. Found factor:" on sTerm0
                
                hput StringTerminalPut on sTerm0
                put itos(res) on sTerm0
                
                hput StringTerminalPut on sTerm0
                put "Process tree:\n" on sTerm0
                
                hput GetTree on result
                get resTree on result
                close result
                hput StringTerminalPut on sTerm0
                put printTree(resTree,0) on sTerm0
                
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