protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get([Char]|S)
    ConsoleGet :: S => Put([Char]|S)
    ConsoleClose :: S => TopBot
    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

-- A database query.
coprotocol S => Query =
    NewQ :: S => Put((Int,[Char],[Char])|Get(Int|S))
    NoQ :: S => TopBot

-- Append two lists
fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

-- string equality
fun strEq :: [Char], [Char] -> Bool =
    q:qs,r:rs -> case (q == r) of
                    False -> False
                    True -> strEq(qs,rs)
    "","" -> True
    _,_ -> False

-- Remove a string from a list of strings.
fun lsRem :: [Char], [[Char]] -> [[Char]] =
    _,[] -> []
    a,b:bs -> case (strEq(a,b)) of
        True -> bs
        False -> b:(lsRem(a,bs))

-- Reads the next word (including its trailing space) from a string.
-- Returns the word and remaining string
fun genVar :: [Char] -> ([Char],[Char]) =
    ""       -> ("","")
    ' ':rest -> ("",rest)
    a:rest   ->
        case genVar(rest) of
            (t,rest2) -> (a:t,rest2)
    _        -> ("","")

-- turns a space-separated list into a proper list of strings
fun toStrList :: [Char] -> [[Char]] =
    "" -> []
    ' ':w -> toStrList(w) -- Remove extra spaces.
    s -> case genVar(s) of
        (w,ws) -> w:toStrList(ws)

-- Pairs each item with a zero.
fun pairUp :: [[Char]] -> [([Char],Int)] =
    [] -> []
    b:bs -> (b,0):pairUp(bs)

-- Takes two positive ints (a/b), returns their quotient and remainder.
-- Works by repeated subtraction
fun div :: Int,Int -> (Int,Int) =
    a,b -> if a < b
        then (0,a)
        else case div(a-b,b) of
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

-- prints the election results.
fun printRes :: [([Char],Int)] -> [Char] =
    [] -> ""
    (name,score):rest -> append(name,':':append(itos(score),'\n':printRes(rest)))

-- Turns an error code into an error message
fun printErr :: Int -> [Char] =
    -1 -> "Voting has closed. Please wait 4 years, then try again."
    1 -> "Name not found."
    2 -> "The given name is busy voting at another terminal."
    3 -> "Vote already cast!"
    4 -> "Candidate not found. Vote cancelled."
    _ -> "Unknown error"

-- Checks if a name is in a list
fun find :: [Char],[[Char]] -> Bool =
    _,[] -> False
    a,b:bs -> if strEq(a,b) then True else find(a,bs)

-- Checks if a candidate is valid.
fun validateVote:: [Char],[([Char],Int)] -> Bool =
    _,[] -> False -- no candidate found
    a,(b,_):bs -> if strEq(a,b) then True else validateVote(a,bs)

-- Finds the voter name, if possible.
fun voterSearch :: [Char],[[Char]],[[Char]] -> Int =
    a,b,c -> switch
        find(a,b) -> 2
        find(a,c) -> 3
        True -> 1

-- Casts a vote. Assumes the vote is valid.
fun castVote :: [Char],[([Char],Int)] -> [([Char],Int)] =
    _,[] -> [] -- Should not happen.
    a,(b,s):bs -> switch
        strEq(a,b) -> (b,s+1):bs
        True -> (b,s):castVote(a,bs)

-- Closes a frontEnd, then quits.
proc closer =
    | res => -> do
        get _ on res
        put -1 on res
        hput NoQ on res
        halt res

-- The front-end for the voting machines
-- proc frontEnd :: | => StringTerminal,Put ((Int, [Char], [T1310]) | Get (Int | Query (|))) =
proc frontEnd =
    | => t1,a1 -> do
        hput StringTerminalPut on t1
        put "Enter voter name:" on t1
        hput StringTerminalGet on t1
        get voter on t1
        
        put (1,voter,[]) on a1
        get vResp on a1
        if vResp == 0
            then do -- proceed to the next step.
                hcase a1 of
                    NoQ -> do -- Finish.
                        hput StringTerminalClose on t1
                        close t1
                        halt a1
                    NewQ -> do
                        
                        hput StringTerminalPut on t1
                        put "Enter candidate, or enter nothing to cancel:" on t1
                        hput StringTerminalGet on t1
                        get candidate on t1
                        
                        put (2,voter,candidate) on a1
                        get cResp on a1
                        
                        if cResp == 0
                            then do
                                hput StringTerminalPut on t1
                                put "Vote cast." on t1
                                hcase a1 of -- Either restart or close.
                                    NoQ -> do -- Finish.
                                        hput StringTerminalClose on t1
                                        close t1
                                        halt a1
                                    NewQ -> frontEnd(|=>t1,a1)
                            else do
                                hput StringTerminalPut on t1
                                put printErr(cResp) on t1
                                hcase a1 of -- Either restart or close.
                                    NoQ -> do -- Finish.
                                        hput StringTerminalClose on t1
                                        close t1
                                        halt a1
                                    NewQ -> frontEnd(|=>t1,a1)
                
            else do
                hput StringTerminalPut on t1
                put printErr(vResp) on t1
                hcase a1 of -- Either restart or close.
                    NoQ -> do -- Finish.
                        hput StringTerminalClose on t1
                        close t1
                        halt a1
                    NewQ -> frontEnd(|=>t1,a1)
        

proc racer2 =
    | a,b => -> do
        race
            a -> do
                get _ on a
                put -1 on a
                hput NoQ on a
                close a
                closer(|b=>)
            b -> do
                get _ on b
                put -1 on b
                hput NoQ on b
                close b
                closer(|a=>)

proc racer3 =
    | a,b,c => res -> do
        race
            a -> do
                get m on a
                put m on res
                get m2 on res
                put m2 on a
                hcase res of
                    NoQ -> do
                        close res
                        hput NoQ on a
                        close a
                        racer2(|b,c => )
                    NewQ -> do
                        hput NewQ on a
                        racer3(|a,b,c => res)
            b -> do
                get m on b
                put m on res
                get m2 on res
                put m2 on b
                hcase res of
                    NoQ -> do
                        close res
                        hput NoQ on b
                        close b
                        racer2(|a,c => )
                    NewQ -> do
                        hput NewQ on b
                        racer3(|a,b,c => res)
            c -> do
                get m on c
                put m on res
                get m2 on res
                put m2 on c
                hcase res of
                    NoQ -> do
                        close res
                        hput NoQ on c
                        close c
                        racer2(|a,b => )
                    NewQ -> do
                        hput NewQ on c
                        racer3(|a,b,c => res)


-- First param is list of candidates,
-- Second param is list of free voters,
-- Third param is list of locked voters,
-- Fourth param is list of voted voters.
-- 'setup' is admin queries, 'res' is voting queries
proc database =
    candList,vtList,lkList,dnList | setup,res => -> do
        race
            setup -> do
                get message on setup
                if strEq(message,"")
                    then do
                        hput ConsolePut on setup
                        put "Shutting down..." on setup
                        hput ConsoleClose on setup
                        close setup
                        closer(|res => ) -- replaces the database with one that rejects 3 consecutive queries, closing each front-end.
                    else do
                        hput ConsolePut on setup
                        put "results so far:" on setup
                        hput ConsolePut on setup
                        put printRes(candList) on setup
                        hput ConsolePut on setup
                        put "Enter 'r' to see results, or nothing to quit:" on setup
                        hput ConsoleGet on setup -- to wait for more input
                        database(candList,vtList,lkList,dnList| setup,res =>) -- call self.
            res -> do
                get message on res
                case message of
                    (1,voter,_) -> do -- Lock a voter
                        if find(voter,vtList)
                            then do
                                put 0 on res -- found
                                hput NewQ on res -- To reset it
                                database(candList,lsRem(voter,vtList),voter:lkList,dnList | setup,res =>) -- Update database.
                            else do
                                put voterSearch(voter,lkList,dnList) on res -- either 2,3,4
                                hput NewQ on res -- To reset it
                                database(candList,vtList,lkList,dnList | setup,res =>) -- reset
                    (2,voter,candidate) -> do -- Issue a vote, or fail.
                        if validateVote(candidate,candList)
                            then do
                                put 0 on res-- Vote is good.
                                hput NewQ on res -- To reset it
                                database(castVote(candidate,candList),vtList,lsRem(voter,lkList),voter:dnList | setup,res =>) -- cast the vote
                            else do
                                put 4 on res -- Cannot find candidate. Vote cancelled.
                                hput NewQ on res -- to reset it
                                database(candList,voter:vtList,lsRem(voter,lkList),dnList | setup,res =>) -- release the lock
                    _ -> do -- should never happen.
                        put 1 on res
                        hput NewQ on res
                        database(candList,vtList,lkList,dnList | setup,res => )


proc run =
    | setup => t1,t2,t3 -> do
        
        -- Set up the database
        -- Candidates
        hput ConsolePut on setup
        put "Enter list of candidates, separated by spaces:" on setup
        hput ConsoleGet on setup
        get candList on setup
        -- Voters
        hput ConsolePut on setup
        put "Enter list of voters, separated by spaces:" on setup
        hput ConsoleGet on setup
        get vtList on setup
        
        hput ConsolePut on setup
        put "Enter 'r' to see results, or nothing to quit:" on setup
        
        hput ConsoleGet on setup -- So that database can race the channels easily.
        
        -- Set up the main architecture.
        plug
            database(pairUp(toStrList(candList)),toStrList(vtList),[],[] | setup, res =>)
            racer3(| a1,a2,a3 => res)
            frontEnd(| => t1,a1)
            frontEnd(| => t2,a2)
            frontEnd(| => t3,a3)