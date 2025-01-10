--Again, this code is mostly the same as the code in ringAroundTheRosie.mpl save for two changes:
--1. The memory cell has access to neither StringTerminal nor Console; 
--   instead, each process has a seperate StringTerminal which it prints to whenever it interacts with the meory cell
--2. The memory cell stores an Int instead of a string and each process prints this Int to its respective terminal when it interacts with the memory cell;
--   each process increments the value of the cell by one whenever it gets it, and the cell starts at zero.

--The program's output should look like this
-- p1's terminal: 
--P1 TERMINAL
--P1
--1
--P1
--4
--P1
--7
--...

-- p2's terminal: 
--P2 TERMINAL
--P2
--2
--P2
--5
--P2
--8
--...

-- p3's terminal: 
--P3 TERMINAL
--P3
--3
--P3
--6
--P3
--9
--...

protocol StringTerminal => S =
    StringTerminalGet :: Get ([ Char ]| S ) => S
    StringTerminalPut :: Put ([ Char ]| S ) => S
    StringTerminalClose :: TopBot => S



coprotocol S => Console =
    ConsolePut :: S => Get ([ Char ]| S )
    ConsoleGet :: S => Put ([ Char ]| S )
    ConsoleClose :: S => TopBot
    ConsoleStringTerminal :: S => S (*) Neg ( StringTerminal )
    
    

protocol
    Passer ( | M ) => S =
    Passer :: M (+) ( Neg ( M ) (*) S ) => S
  


  
--This is Braden's code, which was taken from 'PrimeFinder.mpl' (https://github.com/campl-ucalgary/campl/blob/main/MPLCLI/examples/bradenTests/primeFinder.mpl)
--I use it to print the value of the memCell to StringTerminals
-- Generates a list [...,100,10,1] where the first item is slightly smaller than 'a'
fun tensL :: Int,[Int] -> [Int] =
    a,b:bs -> if a < b*10 then b:bs else tensL(a,(b*10):b:bs)
    _,_ -> [] -- Should never happen
    
    
 -- Takes two positive ints (a/b), returns their quotient and remainder.
-- Works by repeated subtraction
fun div :: Int,Int -> (Int,Int) =
    a,b -> if a < b
        then (0,a)
        else case div(a-b,b) of
            (q,r) -> (q+1,r)
            
            

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

--This is the end of Braden's code



protocol MemCell ( A | ) => S =
    MemPut :: Put ( A | S ) => S 
    MemGet :: Get ( A | S ) => S 
    MemCls :: TopBot => S 



proc memCell :: Int | MemCell ( Int | ) =>  =
    val | memch => -> hcase memch of
        MemPut -> do 
            get nval on memch 
            memCell ( nval | memch => ) 
        MemGet -> do 
            put val on memch 
            memCell ( val | memch => ) 
            
            
        MemCls -> do
          
            halt memch 




proc p1 :: | Passer (| MemCell ( Int |)) => MemCell ( Int |), StringTerminal =
    | passer => mem, stringTerminal -> hcase passer of
        Passer -> do
            hput MemGet on mem
            get i on mem
            hput MemPut on mem
            put i+1 on mem
            hput StringTerminalPut on stringTerminal
            put "P1" on stringTerminal
            hput StringTerminalPut on stringTerminal
            put itos (i+1) on stringTerminal
            fork passer as
                nmem -> nmem |=| mem
                negmemnpasser -> do
                    split negmemnpasser into negmem , npasser
                    plug
                        p1 ( | npasser => z, stringTerminal )
                        z , negmem => -> negmem |=| neg z




proc p2 :: | Passer (| MemCell ( Int |)) => Passer (| MemCell ( Int |)), StringTerminal =
    | passer3 => passer1, stringTerminal -> do
        hput Passer on passer1
        split passer1 into mem1 , negmemnpasser1
        hput MemGet on mem1
        get i on mem1
        hput MemPut on mem1
        put i+1 on mem1
        hput StringTerminalPut on stringTerminal
        put "P2" on stringTerminal
        hput StringTerminalPut on stringTerminal
        put itos (i+1) on stringTerminal
        hcase passer3 of
            Passer -> do
                fork passer3 as
                        nmem3 -> nmem3 |=| mem1
                        negmemnpasser3 -> do
                            split negmemnpasser3 into negmem3 , npasser3
                            fork negmemnpasser1 as
                                negmem1 ->  negmem1 |=| negmem3    
                                npasser1 -> p2 ( | npasser3 => npasser1, stringTerminal )
                            
                            
                        
                        




proc p3 :: | => Passer ( | MemCell (Int|)), StringTerminal =
    | => passer, stringTerminal -> do
        hput Passer on passer
        split passer into mem , negmemnpasser
        hput MemGet on mem
        get i on mem
        hput MemPut on mem
        put i+1 on mem
        hput StringTerminalPut on stringTerminal
        put "P3" on stringTerminal
        hput StringTerminalPut on stringTerminal
        put itos (i+1) on stringTerminal
        fork negmemnpasser as
            negmem -> negmem |=| neg mem
            npasser -> p3 ( | => npasser, stringTerminal )




proc run :: | => StringTerminal, StringTerminal, StringTerminal =
    | => stringTerminal1, stringTerminal2, stringTerminal3 -> do
        hput StringTerminalPut on stringTerminal1 
        put "P1 TERMINAL" on stringTerminal1 
        hput StringTerminalPut on stringTerminal2 
        put "P2 TERMINAL" on stringTerminal2 
        hput StringTerminalPut on stringTerminal3 
        put "P3 TERMINAL" on stringTerminal3 
        --hput StringTerminalClose on stringTerminal1
        --close stringTerminal1
        --hput StringTerminalClose on stringTerminal2
        --close stringTerminal2
        --hput StringTerminalClose on stringTerminal3
        --halt stringTerminal3
        plug
            memCell ( 0 | ch0 =>)
            p1 ( | ch1 => ch0, stringTerminal1)
            p2 ( | ch2 => ch1, stringTerminal2)
            p3 ( | => ch2, stringTerminal3)
        
        