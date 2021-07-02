{-
abc
abcd
-}

codata S -> CataList(A,B)  =
    CataCons :: A,B,S -> B
    CataNil ::      S -> B

fun foldList :: [A], CataList(A,B) -> B =
    [], catalist -> CataNil(catalist)
    s:ss, catalist -> CataCons(s, foldList(ss,catalist), catalist )

fun append :: [A],[A] -> [A] =
    ss, ts -> foldList(ss, (CataNil := -> ts, CataCons := s, acc -> s:acc))

fun concat :: [[A]] -> [A] =
    ss -> foldList(ss, (CataNil := -> [], CataCons := s, acc -> append(s,acc)))

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put append("ab", "c") on _console

        hput ConsolePut on _console
        put concat(["a","b","","cd"]) on _console


        hput ConsoleClose on _console
        halt _console
