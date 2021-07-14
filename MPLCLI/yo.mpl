{-
Nothing
Just((a, bc))
a
b
True
False
0
1
2
3
4
-}


{-
Nothing
Just(a)
Nothing
Just(bc)
Nothing
Just(c)
Nothing
Just(ab)
-}

{-
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

protocol IntTerminal => S =
    IntTerminalPut :: Put( Int | S) => S
    IntTerminalGet :: Get( Int | S) => S 
    IntTerminalClose :: TopBot => S

protocol Mem(M|) => S =
    MemPut :: Put(M|S) => S
    MemGet :: Get(M|S) => S
    MemCls :: TopBot => S

protocol Passer(|P) => S =
    Passer :: P (+) (Neg(P) (*) S) => S

proc memory :: A | Mem(A|) => =
    x | ch => -> do
        hcase ch of
            MemPut -> do
                get y on ch
                memory(y | ch => )
            MemGet -> do
                put x on ch
                memory(x | ch => )
            MemCls -> do
                halt ch

proc p1 :: | => Passer(|Mem(Int|)), IntTerminal = 
    | => passer, _inp -> do
        hput Passer on passer
        split passer into mm,nmpp
        hput MemGet on mm 
        get y on mm
        hput IntTerminalPut on _inp
        put y on _inp
        hput IntTerminalGet on _inp
        get x on _inp
        hput MemPut on mm
        put x on mm
        fork nmpp as
            nm with mm -> nm |=| neg mm
            pp with _inp -> p1(| => pp, _inp)

proc p2 :: | Passer(| Mem(Int|)) => IntTerminal, Mem(Int|) =
    | passer => _inp, mem -> do
        hcase passer of
            Passer -> do
                hput MemGet on mem
                get y on mem
                hput IntTerminalPut on _inp
                put y on _inp
                hput IntTerminalGet on _inp
                get x on _inp
                hput MemPut on mem
                put x on mem
                fork passer as
                    mm with mem -> do
                        mm |=| mem
                    nmpp with _inp -> do
                        split nmpp into nm, pp
                        plug
                            p2( | pp => _inp,z)
                            z,nm => -> nm |=| neg z 

proc run :: | => IntTerminal , IntTerminal =
    | => _inpterm0, _inpterm1 -> do
        plug 
            p1(| => passer, _inpterm0)
            p2(| passer => _inpterm1, mem)
            memory(100 | mem => )
-}


{-

protocol IntTerminal => S =
    IntTerminalPut :: Put( Int | S) => S
    IntTerminalGet :: Get( Int | S) => S 
    IntTerminalClose :: TopBot => S

-- infinite memory cell 
protocol Mem(M|) => S =
    MemPut :: Put(M|S) => S
    MemGet :: Get(M|S) => S
    MemCls :: TopBot => S

protocol Passer(|P) => S =
    Passer :: P (+) (Neg(P) (*) S) => S

protocol InfGetPut => S =
    InfGet :: Get(Int | S) => S
    InfPut :: Put(Int | S) => S


proc infgetput :: Int | InfGetPut => IntTerminal =
    n | ch  => _intterm -> hcase ch of 
        InfGet -> case n + 2 of
            res -> do
                put res on ch
                infgetput(res | ch => _intterm)
        InfPut -> do
            get nn on ch

            hput IntTerminalPut on _intterm
            put nn on _intterm


            infgetput(nn | ch => _intterm )

proc memory :: A | Mem(A|) => =
    x | ch => -> do
        hcase ch of
            MemPut -> do
                get y on ch
                memory(y | ch => )
            MemGet -> do
                put x on ch
                memory(x | ch => )
            MemCls -> do
                halt ch

proc p1 :: | => Passer(|Mem(Int|)), InfGetPut = 
    | => passer, infgetch -> do
        hput Passer on passer
        split passer into mm,nmpp
        hput MemGet on mm 
        get y on mm

        hput InfPut on infgetch
        put y on infgetch
        hput InfGet on infgetch
        get x on infgetch

        hput MemPut on mm
        put x on mm
        fork nmpp as
            nm with mm -> nm |=| neg mm 
            pp with infgetch -> p1(| => pp, infgetch)

proc p2 :: | Passer(| Mem(Int|)) => InfGetPut, Mem(Int|) =
    | passer => infgetch, mem -> do
        hcase passer of
            Passer -> do
                hput MemGet on mem
                get y on mem

                hput InfPut on infgetch
                put y on infgetch

                hput InfGet on infgetch
                get x on infgetch

                hput MemPut on mem
                put x on mem
                fork passer as
                    mm with mem -> do
                        mm |=| mem
                    nmpp with infgetch -> do
                        split nmpp into nm, pp
                        plug
                            p2( | pp => infgetch,z)
                            z,nm => -> nm |=| neg z 

proc run =
    | => _intterm0, _intterm1-> do
        plug 
            infgetput(0 | infget0 => _intterm0)
            infgetput(1 | infget1 => _intterm1)
            p1(| => passer, infget0)
            p2(| passer => infget1, mem)
            memory(100 | mem => )
-}

{-
protocol IntTerminal => S =
    IntTerminalPut :: Put( [Char] | S) => S
    IntTerminalGet :: Get( [Char] | S) => S 
    IntTerminalClose :: TopBot => S

proc run =
    | _console => -> do
        hput IntTerminalPut on _intterm
        put ['a'] on _intterm

        hput IntTerminalGet on _intterm
        get n on _intterm

        hput IntTerminalPut on _intterm
        put n on _intterm

        hput IntTerminalClose on _intterm
        halt _intterm

-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

codata S -> Monoid(M) =
    MAppend :: M,M,S -> M
    MUnit :: S -> M

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

fun listMonoid :: -> Monoid([A]) = 
    -> (MAppend := a,b -> append(a,b), MUnit := -> [])

proc run =
    | _console => -> do
        hput ConsolePut on _console
        put MAppend("pome", MAppend(MUnit(listMonoid), "ranian", listMonoid), listMonoid) 
            on _console

        hput ConsoleClose on _console
        halt _console



{-
codata S -> Fold(A,B) =
    Tally :: A,S-> 
-}
