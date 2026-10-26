
coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

-- the primitive string comparison operators can be used to implement a string comp function 

-- checking string equality
fun (&==) :: [Char], [Char] -> Bool =
    [], [] -> True
    [], (x:xs) -> False
    (x:xs), [] -> False
    (x:xs), (y:ys) -> (x == y) && (xs &== ys)

-- checking eq for length of strings
fun (|==) :: [Char], [Char] -> Bool =
    [], [] -> True
    [], (x:xs) -> False
    (x:xs), [] -> False
    (x:xs), (y:ys) -> (xs |== ys)

-- checking leq for length of strings
fun (|<=) :: [Char], [Char] -> Bool =
    [], [] -> True
    [], (x:xs) -> True
    (x:xs), [] -> False
    (x:xs), (y:ys) -> (xs |<= ys)

-- checking lt for length of strings
fun (|<) :: [Char], [Char] -> Bool =
    [], [] -> False
    [], (x:xs) -> True
    (x:xs), [] -> False
    (x:xs), (y:ys) -> (xs |< ys)

-- checking geq for length of strings
fun (|>=) :: [Char], [Char] -> Bool =
    [], [] -> True
    [], (x:xs) -> False
    (x:xs), [] -> True
    (x:xs), (y:ys) -> (xs |>= ys)

-- checking gt for length of strings
fun (|>) :: [Char], [Char] -> Bool =
    [], [] -> False
    [], (x:xs) -> False
    (x:xs), [] -> True
    (x:xs), (y:ys) -> (xs |> ys)

-- and the rest of the char ordering operations can be used to write ordering ops for strings too
fun (&<=) :: [Char], [Char] -> Bool =
    [], [] -> True
    [], (x:xs) -> True
    (x:xs), [] -> False
    (x:xs), (y:ys) -> (x < y) || ((x == y) && (xs &<= ys))

-- the above implementation has hello &<= hi since e<i, and i don't like that.
-- but we also can't have like hi < barbeque

-- basically i want an ordering like this
-- baby
-- beef
-- barbeque
-- he
-- hi
-- heal
-- hell
-- hill
-- hello
-- hilly
-- health
-- 
-- tear
-- tears

-- check first letters first, i.e. if its < then the whole string is less than
-- if the first letters are == then we compare the length
    -- if the first one is shorter then it's <
    -- if the lengths are the same, compare therest normally (as above)
    -- if the second one is shorter then >
-- if the first letter is > then it's definitely false

-- so we order by first letter then by length then using the above ordering
fun (%<=) :: [Char], [Char] -> Bool =
    [], [] -> True
    [], (x:xs) -> True
    (x:xs), [] -> False
    (x:xs), (y:ys) -> case ((x < y), (x == y), (x > y)) of
        (True, _, _) -> True
        (_, True, _) -> 
            if xs |== ys 
                then xs &<= ys
                else if xs |< ys
                    then True
                    else False    -- hello /&<= hi since hi shorter
        (_, _, True) -> False 

fun (&<) :: [Char], [Char] -> Bool =
    [], [] -> False
    [], (x:xs) -> True
    (x:xs), [] -> False
    (x:xs), (y:ys) -> (x < y) || ((x == y) && (xs &< ys))

fun (&>=) :: [Char], [Char] -> Bool =
    [], [] -> True
    [], (x:xs) -> False
    (x:xs), [] -> True
    -- (x:xs), (y:ys) -> (x > y) || ((x == y) && (xs &>= ys))    
    (x:xs), (y:ys) -> (x >= y) && (xs &>= ys)

fun (&>) :: [Char], [Char] -> Bool =
    [], [] -> False
    [], (x:xs) -> False
    (x:xs), [] -> True
    -- (x:xs), (y:ys) -> (x > y) || ((x == y) && (xs &> ys))
    (x:xs), (y:ys) -> (x >= y) && (xs &> ys)


fun show_bool :: Bool -> [Char] =
    True -> "True"
    False -> "False"

-- A simple function.
proc helloworld :: | Console => = 
    | console => -> do
        on console do        
            hput ConsolePut
            put "testing hi &== hi: " ++ show_bool("hi" &== "hi")
            hput ConsolePut
            put "testing hello &== hi: " ++ show_bool("hello" &== "hi")
            hput ConsolePut
            put "testing hi &<= hi: " ++ show_bool("hi" &<= "hi")
            hput ConsolePut
            put "testing hello &<= hi: " ++ show_bool("hello" &<= "hi")
            hput ConsolePut
            put "testing hi &< hi: " ++ show_bool("hi" &< "hi")
            hput ConsolePut
            put "testing hello &< hi: " ++ show_bool("hello" &< "hi")
            hput ConsolePut
            put "testing hi &>= hi: " ++ show_bool("hi" &>= "hi")
            hput ConsolePut
            put "testing hello &>= hi: " ++ show_bool("hello" &>= "hi")
            hput ConsolePut
            put "testing hi &> hi: " ++ show_bool("hi" &> "hi")
            hput ConsolePut
            put "testing hello &> hi: " ++ show_bool("hello" &> "hi")
        if ("hi" &== "hi")
            then on console do
                -- hput ConsolePut
                -- put "enter anything to finish"
                -- hput ConsoleGet
                -- get _
                hput ConsoleClose
                halt
            else on console do
                hput ConsoleClose
                halt

proc run = 
    | console => -> helloworld( |console=>)