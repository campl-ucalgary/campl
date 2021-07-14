protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

fun concat :: [[A]] -> [A] =
    [] -> []
    s:ss -> append(s, concat(ss))

defn
    fun intersperse :: A,[A] -> [A] =
        sep, [] -> []
        sep, s:ss -> s : prependAll(sep, ss)
where
    fun prependAll :: A, [A] -> [A] =
        _, [] -> []
        sep, s:ss -> sep : s : prependAll(sep, ss)

fun intercalate :: [A], [[A]] -> [A] =
    sep, ss -> concat(intersperse(sep, ss))

codata S -> Fun(A,B) = 
    App :: A,S -> B

fun map :: Fun(A,B), [A] -> [B] = 
    _, [] -> []
    hfun, t:ts-> App(t,hfun) : map(hfun, ts)

fun concatMap :: Fun(A, [B]), [A] -> [B] =
    hfun,lst -> concat(map(hfun, lst))

fun showBool :: Bool -> [Char] =
    True -> "True"
    False -> "False"


defn
    fun transpose :: [[A]] -> [[A]] = 
        [] -> []
        []:ts -> transpose(ts)
        (s:ss):ts -> case headstails(ts) of
            nts -> (s : concatMap( (App := (head, _) -> head ), nts))
                : transpose( ss : map( (App := (_, tail) -> tail), nts ))
where
    fun headstails :: [[A]] -> [([A], [A])] =
        [] -> []
        []:ts -> ([], []) : headstails(ts)
        (s:ss):ts -> ([s], ss) : headstails(ts)

-- | logical and
fun land :: Bool, Bool -> Bool =
    True, True -> True
    _,_ -> False

-- | logical not
fun lnot :: Bool -> Bool =
    True -> False
    False -> True

-- | logical or
fun lor :: Bool, Bool -> Bool =
    a,b -> lnot(land(a,b))

-- | ors a list together.
fun or :: [Bool] -> Bool = 
    [] -> False
    True:_ -> True
    False:ts -> or(ts)

-- | replciates an element n times in a list (n should be positive) 
fun replicate :: Int, A -> [A] = 
    0, _ -> []
    n, s -> s : replicate(n - 1, s)

data Maybe(A) -> S =
    Just    :: A -> S
    Nothing ::   -> S

data Player -> S = 
    -- X piece
    X :: -> S
    -- O piece
    O :: -> S

-- | Tests if two players are equal to each other
fun eqPlayer :: Player, Player -> Bool =
    X, X -> True
    O, O -> True
    _, _ -> False

-- | the grid representation.
codata S -> Grid = 
    TopRow :: S -> (Maybe(Player), Maybe(Player), Maybe(Player))
    MidRow :: S -> (Maybe(Player), Maybe(Player), Maybe(Player))
    BotRow :: S -> (Maybe(Player), Maybe(Player), Maybe(Player))

-- | projections for triplets.
fun fst :: (A,B,C) -> A =
    (a,b,c) -> a

fun snd :: (A,B,C) -> B =
    (a,b,c) -> b

fun thd :: (A,B,C) -> C =
    (a,b,c) -> c

-- | indexes for the grids.. it goes row (character), then column (int)
data GridIx -> S = 
    A0 :: -> S
    A1 :: -> S
    A2 :: -> S
    B0 :: -> S
    B1 :: -> S
    B2 :: -> S
    C0 :: -> S
    C1 :: -> S
    C2 :: -> S

-- | converts a grid to a list of lists.
defn
    fun gridToLists :: Grid -> [[Maybe(Player)]] =
        (TopRow := top, MidRow := mid,  BotRow := bot) -> 
            [tripletToList(top), tripletToList(mid), tripletToList(bot)]
where
    fun tripletToList :: (A,A,A) -> [A] =
        (a,b,c) -> [a,b,c]

-- | test if the given player has won 
defn
    fun won :: Player, Grid -> Bool = 
        p, grid -> case gridToLists(grid) of
            lsts -> or 
                ( 
                    -- either: there is a horizontal line, vertical line, or diagonal line.
                    [ or(map((App := lst -> allIsPlayer(p, lst)), lsts))
                    , or(map((App := lst -> allIsPlayer(p, lst)), transpose(lsts)))
                    , or(map((App := lst -> allIsPlayer(p, lst)), getDiags(grid)))
                    ]
                )
where
    fun allIsPlayer :: Player, [Maybe(Player)] -> Bool =
        focusedp, ts -> 
            let fun go :: [Maybe(Player)] -> Bool =
                    [] -> True
                    Just(p) : ts -> if eqPlayer(focusedp,p)
                        then go(ts)
                        else False
                    Nothing : ts -> False
            in go(ts)


    fun getDiags :: Grid -> [[Maybe(Player)]] = 
        grid -> 
            [ [ fst(TopRow(grid)), snd(MidRow(grid)), thd(BotRow(grid))]
            , [ thd(TopRow(grid)), snd(MidRow(grid)), fst(BotRow(grid))]
            ] 
        

-- | show the grid (converts to string)
defn
    fun showGrid :: Grid -> [Char] = 
        grid -> case replicate(9, '-') of
            horizontalline -> 
                intercalate
                    ("\n"
                    ,
                        [ showRow(TopRow(grid))
                        , horizontalline
                        , showRow(MidRow(grid))
                        , horizontalline
                        , showRow(BotRow(grid))
                        ]
                    )
where
    fun showMaybePlayer :: Maybe(Player) -> [Char] =
        Nothing -> " " 
        Just(O) -> "O" 
        Just(X) -> "X" 
    
    fun showRow :: (Maybe(Player), Maybe(Player), Maybe(Player)) -> [Char] =
        (a,b,c) -> concat( 
                    [ showMaybePlayer(a)
                    , " | "
                    , showMaybePlayer(b)
                    , " | "
                    , showMaybePlayer(c)
                    ]
                )

fun testGrid =
    -> 
        ( TopRow := -> (Just(X), Just(O), Just(X))
        , MidRow := -> (Nothing, Just(X), Just(X))
        , BotRow := -> (Just(O), Just(X), Just(X))
        )

-- | plays a move, returns Just the new grid if it exists, otherwise nothing.
fun playMove :: Player,GridIx,Grid -> Maybe(Grid) =
    p, ix, grid -> case ix of
        A0 -> case fst(TopRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> (Just(p), snd(TopRow(grid)), thd(TopRow(grid))) 
                        , MidRow := -> MidRow(grid)
                        , BotRow := -> BotRow(grid)
                        )
                    )
        _ -> Nothing
            
        {-
        A0 :: -> S
        A1 :: -> S
        A2 :: -> S
        B0 :: -> S
        B1 :: -> S
        B2 :: -> S
        C0 :: -> S
        C1 :: -> S
        C2 :: -> S
        -}
    

proc run =
    | _console => -> do
        hput ConsolePut on _console
        -- put concat(transpose(["abc", "def","ghi"])) on _console
        -- put showGrid(testGrid) on _console
        put showBool(won(X, testGrid)) on _console
        

        hput ConsoleClose on _console
        halt _console
