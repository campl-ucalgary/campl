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

fun flipPlayer :: Player -> Player =
    X -> O
    O -> X

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
    A1 :: -> S
    A2 :: -> S
    A3 :: -> S
    B1 :: -> S
    B2 :: -> S
    B3 :: -> S
    C1 :: -> S
    C2 :: -> S
    C3 :: -> S

-- | converts a grid to a list of lists.
defn
    fun gridToLists :: Grid -> [[Maybe(Player)]] =
        (TopRow := top, MidRow := mid,  BotRow := bot) -> 
            [tripletToList(top), tripletToList(mid), tripletToList(bot)]
where
    fun tripletToList :: (A,A,A) -> [A] =
        (a,b,c) -> [a,b,c]

-- | test if the given player is a winner 
defn
    fun winner :: Player, Grid -> Bool = 
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

-- | tests if someone has won the game or not.
defn
    fun wonOrTie :: Grid -> Bool =
        grid -> or([winner(O, grid), winner(X, grid), allIsJust(concat(gridToLists(grid)))]) 
where
    fun allIsJust =
        [] -> True
        Nothing : _ -> False
        Just(_) : ts -> allIsJust(ts)
        

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

fun emptyGrid :: -> Grid =
    -> 
        ( TopRow := -> (Nothing, Nothing, Nothing)
        , MidRow := -> (Nothing, Nothing, Nothing)
        , BotRow := -> (Nothing, Nothing, Nothing)
        )

-- | plays a move, returns Just the new grid if it exists, otherwise nothing.
-- lots of duplicated code -- no record syntax. The idea is pretty straightfoward:
-- If the position is a Just, then there is something already there, so you cannot
-- make a move there... Otherwise, if it is Nothing, then you can actually make a 
-- move there...
fun playMove :: Player,GridIx,Grid -> Maybe(Grid) =
    p, ix, grid -> case ix of
        A1 -> case fst(TopRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> (Just(p), snd(TopRow(grid)), thd(TopRow(grid))) 
                        , MidRow := -> MidRow(grid)
                        , BotRow := -> BotRow(grid)
                        )
                    )
        A2 -> case snd(TopRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> (fst(TopRow(grid)), Just(p), thd(TopRow(grid))) 
                        , MidRow := -> MidRow(grid)
                        , BotRow := -> BotRow(grid)
                        )
                    )
        A3 -> case thd(TopRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> (fst(TopRow(grid)), snd(TopRow(grid)), Just(p)) 
                        , MidRow := -> MidRow(grid)
                        , BotRow := -> BotRow(grid)
                        )
                    )
        B1 -> case fst(MidRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> TopRow(grid)
                        , MidRow := -> (Just(p), snd(MidRow(grid)), thd(MidRow(grid))) 
                        , BotRow := -> BotRow(grid)
                        )
                    )
        B2 -> case snd(MidRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> TopRow(grid)
                        , MidRow := -> (fst(MidRow(grid)), Just(p), thd(MidRow(grid))) 
                        , BotRow := -> BotRow(grid)
                        )
                    )
        B3 -> case thd(MidRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> TopRow(grid)
                        , MidRow := -> (fst(MidRow(grid)), snd(MidRow(grid)), Just(p)) 
                        , BotRow := -> BotRow(grid)
                        )
                    )

        C1 -> case fst(BotRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> TopRow(grid)
                        , MidRow := -> MidRow(grid)
                        , BotRow := -> (Just(p), snd(BotRow(grid)), thd(BotRow(grid))) 
                        )
                    )
        C2 -> case snd(BotRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> TopRow(grid)
                        , MidRow := -> MidRow(grid)
                        , BotRow := -> (fst(BotRow(grid)), Just(p), thd(BotRow(grid))) 
                        )
                    )
        C3 -> case thd(BotRow(grid)) of
            Just(_) -> Nothing
            Nothing -> Just(
                        ( TopRow := -> TopRow(grid)
                        , MidRow := -> MidRow(grid)
                        , BotRow := -> (fst(BotRow(grid)), snd(BotRow(grid)), Just(p)) 
                        )
                    )

-- | parses a move index
fun parseGridIx :: [Char] -> Maybe(GridIx) =
    [l,r] -> case l of 
        'a' -> case r of 
            '1' -> Just(A1)
            '2' -> Just(A2)
            '3' -> Just(A3)
            _ -> Nothing
        'b' -> case r of 
            '1' -> Just(B1)
            '2' -> Just(B2)
            '3' -> Just(B3)
            _ -> Nothing
        'c' -> case r of 
            '1' -> Just(C1)
            '2' -> Just(C2)
            '3' -> Just(C3)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

protocol 
    UserMoveSet => S = 
        UserMoveSet       :: Put((Player, Grid) | T) => S
        UserMoveGameOver  :: Put(([Char], Grid) | TopBot) => S

    and

    UserMoveGet => T = 
        UserMoveGet   :: Get(Grid | S) => T
        UserMoveClose :: TopBot => T

defn 
    proc userMoveSetAndGet :: | UserMoveSet => StringTerminal = 
        |  usrmvset =>  _strterm -> hcase usrmvset of
            UserMoveSet -> do
                get (playerty, grid) on usrmvset

                hput StringTerminalPut on _strterm
                put "The opponent made a move as follows." on _strterm

                hput StringTerminalPut on _strterm
                put showGrid(grid) on _strterm

                userMoveGetLoop( playerty, grid | usrmvset =>  _strterm)

            UserMoveGameOver -> do
                get (gameovermsg, grid) on usrmvset

                hput StringTerminalPut on _strterm
                put gameovermsg on _strterm

                hput StringTerminalPut on _strterm
                put showGrid(grid) on _strterm

                hput StringTerminalClose on _strterm
                close _strterm

                halt usrmvset

    proc userMoveGetLoop :: Player, Grid | UserMoveGet =>  StringTerminal = 
        playerty, grid |  usrmvget =>  _strterm -> do
            hput StringTerminalPut on _strterm
            put "Please enter a move (given by [a-c][1-3]) where [a-c] denotes the row and [1-3] denotes the column:" on _strterm

            hput StringTerminalGet on _strterm
            get inp on _strterm

            case parseGridIx(inp) of
                Just(nix) -> case playMove(playerty, nix, grid) of
                    Just(ngrid) -> hcase usrmvget of
                        UserMoveGet -> do
                            put ngrid on usrmvget

                            hput StringTerminalPut on _strterm
                            put "Nice move! The board is now as follows." on _strterm

                            hput StringTerminalPut on _strterm
                            put showGrid(ngrid) on _strterm

                            userMoveSetAndGet( | usrmvget => _strterm)

                    Nothing -> userMoveGetInvalidMove( playerty, grid | usrmvget =>  _strterm)

                Nothing -> userMoveGetInvalidMove( playerty, grid | usrmvget =>  _strterm)

    proc userMoveGetInvalidMove :: Player, Grid | UserMoveGet =>  StringTerminal = 
        playerty, grid |  usrmvget =>  _strterm -> do
            hput StringTerminalPut on _strterm
            put "Invalid move." on _strterm

            userMoveGetLoop(playerty, grid | usrmvget =>  _strterm)

protocol 
    PasserMem(|M) => S =
        PasserMem :: M (+) T => S

    and 

    Passer(|M) => T =
        Passer :: (Neg(M) (*) S) => T
        Passed :: TopBot => T

protocol MemCell(A | ) => S = 
    MemPut :: Put(A| S) => S
    MemGet :: Get(A| S) => S
    MemClose :: TopBot => S

proc memCell :: A | MemCell(A| ) => =
    v | ch => -> hcase ch of
        MemPut -> do
            get nv on ch
            memCell(nv | ch => )
        MemGet -> do
            put v on ch
            memCell(v | ch => )

        MemClose -> 
            halt ch

proc playerO :: | => PasserMem( | MemCell(Grid |)), UserMoveSet =
    | => passermem, usrmvset -> do
        hput PasserMem on passermem
        split passermem into mem, passer

        -- get the current board state
        hput MemGet on mem 
        get grid on mem

        if wonOrTie(grid)
            -- someone has won, so just clean everything up
            then do
                hput MemClose on mem
                close mem

                hput UserMoveGameOver on usrmvset
                put (if winner(O, grid) then "You won!" else "You did not win!", grid) on usrmvset
                close usrmvset
                
                hput Passed on passer
                halt passer

            -- no one has won, so continue the game
            else do
                -- set the user board
                hput UserMoveSet  on usrmvset
                put (O, grid) on usrmvset

                -- query the new user board
                hput UserMoveGet  on usrmvset
                get ngrid on usrmvset

                -- set the new board state
                hput MemPut on mem
                put ngrid on mem

                hput Passer on passer
                -- do the passer thing 
                fork passer as
                    nmem with mem -> nmem |=| neg mem
                    npasser with usrmvset -> 
                        playerO( | => npasser, usrmvset )

defn
    proc playerX :: | PasserMem( | MemCell(Grid |)) => MemCell(Grid | ), UserMoveSet  =
        | passermem =>  mem, usrmvset -> hcase passermem of
            PasserMem -> do
                -- get the current board state
                hput MemGet on mem 
                get grid on mem

                -- really should hcase here to see if the game is done
                if wonOrTie(grid)
                    then do
                        goPasserMem( grid | passermem => mem, usrmvset)

                    else do
                        -- set the user board
                        hput UserMoveSet  on usrmvset
                        put (X, grid) on usrmvset

                        -- query the new user board
                        hput UserMoveGet  on usrmvset
                        get ngrid on usrmvset

                        -- set the new board state
                        hput MemPut on mem
                        put ngrid on mem

                        goPasserMem( ngrid | passermem => mem, usrmvset)

    proc goPasserMem :: Grid | MemCell(Grid|) (+) Passer( | MemCell(Grid|)) => MemCell(Grid|), UserMoveSet =
        grid | passermem => mem, usrmvset -> fork passermem as
            nmem with mem -> mem |=| nmem

            passer with usrmvset -> hcase passer of
                Passer -> do
                    split passer into nmem, npasser
                    plug 
                        playerX( | npasser => nnmem, usrmvset)
                        nnmem,nmem  => -> nmem |=| neg nnmem 
                Passed -> do
                    hput UserMoveGameOver on usrmvset
                    put (if winner(X, grid) then "You won!" else "You did not win!", grid) on usrmvset
                    close usrmvset

                    halt passer
        
proc run =
    | => _strtermO, _strtermX -> do
        plug 
            userMoveSetAndGet( | usrmvO => _strtermO)
            userMoveSetAndGet( | usrmvX =>  _strtermX)

            playerO( | => passer, usrmvO )
            playerX( | passer => mem, usrmvX)

            memCell(emptyGrid | mem => )
