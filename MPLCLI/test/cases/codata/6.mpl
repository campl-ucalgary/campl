{-
ab
-}

-- some what complicated example of using a fold to do dropWhileEnd.

codata S -> CataList(A,B)  =
    CataCons :: A,B,S -> B
    CataNil ::      S -> B

codata S -> App(A,B) =
    App :: A,S -> B

fun foldList :: [A], CataList(A,B) -> B =
    [], catalist -> CataNil(catalist)
    s:ss, catalist -> CataCons(s, foldList(ss,catalist), catalist )

fun dropWhileEnd :: [A], App(A, Bool) -> [A] =
    ss, (App := f) -> 
        let fun g =
                s, [] -> if f(s)
                    then []
                    else [s]
                s, ss -> s:ss
        in foldList(ss, (CataNil := -> [], CataCons := s, acc -> g(s,acc) ))

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do

        hput ConsolePut on _console
        put dropWhileEnd("abaa", (App := a -> if a == 'a' then True else False)) on _console


        hput ConsoleClose on _console
        halt _console
