include "utils.mpl" : U

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

protocol InOut(A, B | ) => S =
    InOut :: Get(A | Put(B | S)) => S


-- class Interact T where
-- pinteract :: | T => StringTerminal

-- instance Parse A, Interact T => Interact Get(A | T ) where
--      proc pinteract :: Parse A, Interact T ~> | Get(A | T) => StringTerminal =
--          | ch => strterm -> ...

-- translation of pinteract for Get type
proc get_pinteract :: Fun([Char], Maybe(A)) | Neg(T) (+) StringTerminal, Get(A | T) => StringTerminal =
    parseA | negTandStrtrm, ch => strterm -> do
        hput StringTerminalPut on strterm
        put "enter an integer" on strterm
        hput StringTerminalGet on strterm
        get input on strterm
        case App(input, parseA) of
            Just(a_val) -> do
                put a_val on ch
                fork negTandStrtrm as
                    negT -> negT |=| neg ch 
                    strtrm -> strterm |=| strtrm
            Nothing -> do
                get_pinteract( parseA | negTandStrtrm, ch => strterm)


-- instance Show A, Interact T => Interact Get(A | T ) where
--      proc pinteract :: Show A, Interact T ~> | Put(A | T) => StringTerminal =
--          | ch => strterm -> ...

proc put_pinteract :: Fun(A, [Char]) | Neg(T) (+) StringTerminal, Put(A | T) => StringTerminal =
    showA | negTandStrtrm, ch => strterm -> do
        get input on ch
        hput StringTerminalPut on strterm
        put App(input, showA) on strterm
        fork negTandStrtrm as
            negT -> negT |=| neg ch 
            strtrm -> strterm |=| strtrm

-- instance Show A, Interact T => Interact Get(A | T ) where
--      proc pinteract :: ParseA, ShowB, Interact T ~> | InOut(A, B |) => StringTerminal =
--          | ch => strterm -> ...

proc splitter :: | A, B => A (+) B = 
    | t1, t2 => t3 -> do
        split t3 into t11, t22
        plug
            z, t1 => t11 -> do
                close z
                t1 |=| t11
            t2 => t22, z -> do
                close z
                t2 |=| t22
            
proc pinteract :: Fun(B, [Char]), Fun([Char], Maybe(A)) | InOut(A, B | ) => StringTerminal =
    showB, parseA | ch => strterm -> do
        hcase ch of
            InOut -> plug
                get_pinteract(parseA | t1, ch => strterm)
                splitter( | t2, strterm2 => t1)            
                => negT2, t2 -> negT2 |=| neg t2
                put_pinteract(showB | t3, negT2 => strterm2)
                splitter( | t4, strterm3 => t3)
                => negT4, t4 -> negT4 |=| neg t4
                pinteract(showB, parseA | negT4 => strterm3)

-- type of the dictionary (package) that would be generated for "instance Interact InOut"
-- codata S -> InOut_Interact_Dict(A,B)  =
--        Pinteract :: S -> (InOut(A, B | ) => Console)


proc test :: Int | Console => InOut(Int, Int |) =
    val | console => ch -> do
        hput ConsolePut on console
        put "waiting for input" on console
        hput InOut on ch
        get inp on ch
        hput ConsolePut on console
        put "got input" on console
        hput ConsolePut on console
        put U.showInt(inp) on console
        put val on ch
        test(val | console => ch)

proc run :: | Console => StringTerminal, StringTerminal =
    | console => strterm ->
        plug
            test(8123 | console => a)
            -- pinteract( | a => strterm)
            -- translation:
            pinteract(ShowInt(U.int_show_dict()), ParseInt(U.int_parse_dict())| a => strterm)
