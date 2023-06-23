protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

-- A quantified boolean expression.
data Equation(A) -> S =
    EqAll :: A,S -> S -- For all
    EqExs :: A,S -> S -- There exists
    EqVar :: A -> S   -- Variable
    EqAnd :: S,S -> S -- And
    EqOrr :: S,S -> S -- Or
    EqNot :: S -> S   -- Not

-- Append two lists
fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

fun printList :: [[Char]] -> [Char] =
    [] -> ""
    b:bs -> append(b,',':' ':printList(bs))

fun lsEq :: [Char], [Char] -> Bool =
    q:qs,r:rs -> case (q == r) of
                    False -> False
                    True -> lsEq(qs,rs)
    "","" -> True
    _,_ -> False

fun isNull :: [A] -> Bool =
    [] -> True
    _ -> False

-- Remove an item from a list
fun lsRem :: [Char], [[Char]] -> [[Char]] =
    _,[] -> []
    a,b:bs -> case (lsEq(a,b)) of
        True -> bs
        False -> b:(lsRem(a,bs))

-- Generates the union of two sets
fun union :: [[Char]], [[Char]] -> [[Char]] =
    [],a -> a
    b:bs,a -> b:union(bs,lsRem(b,a))

-- Prints an equation nicely. Cannot use genEq on the result.
fun printEq :: Equation([Char]) -> [Char] =
    EqVar(a)     -> a
    EqAnd(t1,t2) -> append("(",append(printEq(t1),append(" && ",append(printEq(t2),")"))))
    EqOrr(t1,t2) -> append("(",append(printEq(t1),append(" || ",append(printEq(t2),")"))))
    EqNot(t)     -> append("!",printEq(t))
    EqAll(v,t)   -> append(append("ForAll ",v),'.':' ':printEq(t))
    EqExs(v,t)   -> append(append("Exists ",v),'.':' ':printEq(t))

-- Reads the next word (including its trailing space) from a string.
-- Returns the word and remaining string
fun genVar :: [Char] -> ([Char],[Char]) =
    ""       -> ("","")
    ' ':rest -> ("",rest)
    a:rest   ->
        case genVar(rest) of
            (t,rest2) -> (a:t,rest2)
    _        -> ("","")

-- Generates an equation from a string.
-- Returns the equation and any remaining unparsed string.
fun genEq :: [Char] -> (Equation([Char]),[Char]) =
    '!':(' ':rest) ->
        case genEq(rest) of
            (t,rest2) -> (EqNot(t),rest2)
    'A':(' ':rest) ->
        case genVar(rest) of
            (v,rest2) -> 
                case genEq(rest2) of
                    (t,rest3) -> (EqAll(v,t),rest3)
    'E':(' ':rest) ->
        case genVar(rest) of
            (v,rest2) -> 
                case genEq(rest2) of
                    (t,rest3) -> (EqExs(v,t),rest3)
    '&':(' ':rest) ->
        case genEq(rest) of
            (t1,rest2) -> 
                case genEq(rest2) of
                    (t2,rest3) -> (EqAnd(t1,t2),rest3)
    '|':(' ':rest) ->
        case genEq(rest) of
            (t1,rest2) -> 
                case genEq(rest2) of
                    (t2,rest3) -> (EqOrr(t1,t2),rest3)
    -- Note: Variables are a catch-all. They are anything that ends in a space or ends a string,
    -- And isn't caught by any previous category. Because of this, every input will parse, even if nonsensically.
    a ->
        case genVar(a) of
            (var,rest) -> (EqVar(var),rest)

-- Generates an equation from a string safely
fun genEqFin :: [Char] -> Equation([Char]) =
    a -> case genEq(a) of
        (t,_) -> t

fun freeVars :: Equation([Char]) -> [[Char]] =
    EqVar(a)    -> [a]
    EqAll(a,t)  -> lsRem(a,freeVars(t))
    EqExs(a,t)  -> lsRem(a,freeVars(t))
    EqAnd(t1,t2)-> union(freeVars(t1),freeVars(t2))
    EqOrr(t1,t2)-> union(freeVars(t1),freeVars(t2))
    EqNot(t)-> freeVars(t)

-- Enforces that no further bindings can be found inside the equation
fun noBindings :: Equation([Char]) -> Bool =
    EqAll(_,_) -> False
    EqExs(_,_) -> False
    EqVar(_) -> True
    EqAnd(t1,t2) -> case noBindings(t1) of
        False -> False
        True -> noBindings(t2)
    EqOrr(t1,t2) -> case noBindings(t1) of
        False -> False
        True -> noBindings(t2)
    EqNot(t1) -> noBindings(t1)

-- Enforces that all bindings are before the equation.
-- 'True' means valid.
fun lateBindings :: Equation([Char]) -> Bool =
    EqAll(_,t)  -> lateBindings(t)
    EqExs(_,t)  -> lateBindings(t)
    a -> noBindings(a) -- No bindings after this point.

fun lNot :: Bool -> Bool =
    False -> True
    True -> False

proc andWaiter :: | Put(Bool|TopBot),Put(Bool|TopBot) => Put(Bool|TopBot) =
    | a,b => c -> do
        race
            a -> do
                get valA on a
                close a
                if valA
                    then do
                        get valB on b
                        close b
                        put valB on c
                        halt c
                    else do
                        put False on c
                        close c
                        get _ on b
                        halt b
            b -> do
                get valB on b
                close b
                if valB
                    then do
                        get valA on a
                        close a
                        put valA on c
                        halt c
                    else do
                        put False on c
                        close c
                        get _ on a
                        halt a

proc orWaiter :: | Put(Bool|TopBot),Put(Bool|TopBot) => Put(Bool|TopBot) =
    | a,b => c -> do
        race
            a -> do
                get valA on a
                close a
                if valA
                    then do -- short-circuit
                        put True on c
                        close c
                        get _ on b
                        halt b
                    else do
                        get valB on b
                        close b
                        put valB on c
                        halt c
            b -> do
                get valB on b
                close b
                if valB
                    then do -- short-circuit
                        put True on c
                        close c
                        get _ on a
                        halt a
                    else do
                        get valA on a
                        close a
                        put valA on c
                        halt c

-- Looks up a variable in a table.
fun lookup :: [([Char],Bool)],[Char] -> Bool =
    [],_ -> False -- error case
    (b,r):bs,n -> case lsEq(b,n) of
        True -> r
        False -> lookup(bs,n)

-- Evaluates an equation in a context
fun eqEval :: [([Char],Bool)],Equation([Char]) -> Bool =
    c,EqVar(n) -> lookup(c,n)
    c,EqAnd(t1,t2) -> case eqEval(c,t1) of
        True -> eqEval(c,t2)
        False -> False
    c,EqOrr(t1,t2) -> case eqEval(c,t1) of
        False -> eqEval(c,t2)
        True -> True
    c,EqNot(t1) -> lNot(eqEval(c,t1))
    _,_ -> False -- Anything else is an error

fun isEqAll :: Equation([Char]) -> Bool =
    EqAll(_,_) -> True
    _ -> False

fun isEqExs :: Equation([Char]) -> Bool =
    EqExs(_,_) -> True
    _ -> False

fun eqSub :: Equation([Char]) -> Equation([Char]) =
    EqAll(_,t) -> t
    EqExs(_,t) -> t
    _ -> EqVar("") -- This should never happen.

fun eqVarName :: Equation([Char]) -> [Char] =
    EqAll(v,_) -> v
    EqExs(v,_) -> v
    _ -> "" -- This should never happen.

defn
    -- A process that takes a context and subtree, and solves it.
    -- Checks for 'or', and splits if needed
    proc solver2 :: [([Char],Bool)],Equation([Char]) | => Put(Bool|TopBot) =
        context,myEq | => res -> do
            if isEqExs(myEq)
                then do
                    plug
                        orWaiter(| a,b => res)
                        solver((eqVarName(myEq),True ):context,eqSub(myEq) | => a)
                        solver((eqVarName(myEq),False):context,eqSub(myEq) | => b)
                else do
                    put eqEval(context,myEq) on res
                    halt res

    -- A process that takes a context and subtree, and solves it.
    -- Checks for 'and', and splits if needed, otherwise passes it to solver2
    proc solver :: [([Char],Bool)],Equation([Char]) | => Put(Bool|TopBot) =
        context,myEq | => res -> do
            if isEqAll(myEq)
                then do
                    plug
                        andWaiter(| a,b => res)
                        solver((eqVarName(myEq),True ):context,eqSub(myEq) | => a)
                        solver((eqVarName(myEq),False):context,eqSub(myEq) | => b)
                else do
                    solver2(context,myEq | => res) -- pass to next 'if'

-- A process that writes the result to the console
proc outputter :: | Put(Bool|TopBot) => StringTerminal =
    | result => sTerm0 -> do
        get res on result
        close result
        
        if res
            then do
                hput StringTerminalPut on sTerm0
                put "Claim holds" on sTerm0
                
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0
            else do
                hput StringTerminalPut on sTerm0
                put "Claim does not hold" on sTerm0
        
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0

proc validateLateBinding :: [Char] | => StringTerminal =
    eqStr | => sTerm0 -> do
        if lateBindings(genEqFin(eqStr))
            then do -- good
                -- Write if input is true.
                plug
                    solver([],genEqFin(eqStr) | => result)
                    outputter(| result => sTerm0)
            else do -- fail
                hput StringTerminalPut on sTerm0
                put "Error, please make sure all 'for all' and 'there exists' declarations are before the equation!" on sTerm0
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0

proc run =
    | => sTerm0 -> do
        hput StringTerminalPut on sTerm0
        put "Enter boolean expression, in prefix (polish) notation; no brackets:\n    !   not\n    &   and\n    |   or" on sTerm0
        hput StringTerminalPut on sTerm0
        put "    A   for all\n    E   there exists" on sTerm0
        
        hput StringTerminalPut on sTerm0
        put "Eg:\n-----------------------------------\n  A b E c E a ! & a | b c\n-----------------------------------\n\n" on sTerm0
        
        hput StringTerminalGet on sTerm0
        get eqStr on sTerm0
        
        hput StringTerminalPut on sTerm0
        put "\nInterpretation:" on sTerm0
        
        hput StringTerminalPut on sTerm0
        put printEq(genEqFin(eqStr)) on sTerm0
        
        if isNull(freeVars(genEqFin(eqStr)))
            then do -- good
                validateLateBinding(eqStr | => sTerm0)
            else do -- error
                hput StringTerminalPut on sTerm0
                put append("Error, unbound variables: ",printList(freeVars(genEqFin(eqStr)))) on sTerm0
                
                hput StringTerminalGet on sTerm0
                get _ on sTerm0
                hput StringTerminalClose on sTerm0
                halt sTerm0
        
        
        
