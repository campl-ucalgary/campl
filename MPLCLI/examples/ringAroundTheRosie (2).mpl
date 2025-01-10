--Suppose you have three processes, p1, p2, and p3, and you want them to pass a memory cell around forever in a round-robin manner -- i.e. p1 passes it to p2, which passes it to p3, which passes it to p1 and so on.
--You cannot do this naively in CaMPL as loops are not allowed.
--This program is meant to simulate such a round-robin interaction by having the processes pass the cell around in the following sequence:
--p1 passes the cell to p2 which passes it to p3 which passes it back to p2 which passes it back to p1 and so on
--where the processes do not interact with the cell when it is being 'passed back'
--When a process gets the cell (and it is not passing it back), it puts its own name onto the cell and the cell is supposed to print it to the console.
--So the program output should look like this:
--Program started!
--P1
--P2
--P3
--P1
--P2
--P3
--P1
--...
--Much of this code was taken from Jared Pon's guide to programming in CaMPL: https://github.com/campl-ucalgary/campl/blob/main/resources/502.02A_interim_pon.pdf.


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
    
    
protocol MemCell ( A | ) => S =
    MemPut :: Put ( A | S ) => S 
    MemGet :: Get ( A | S ) => S 
    MemCls :: TopBot => S 


--Note that whenever a new value is put onto this memory cell, it prints out the value to the console.
proc memCell :: [Char] | Console, MemCell ( [Char] | ) => =
    val | console, memch => -> hcase memch of
        MemPut -> do 
            get nval on memch 
            hput ConsolePut on console -- printing out the new value
            put nval on console 
            memCell ( nval | console, memch => ) 
        MemGet -> do 
            put val on memch 
            memCell ( val | console, memch => ) 
            
            
        MemCls -> do
            hput ConsoleClose on console
            close console
            halt memch 



--This is identical to process 'p1' on page 27 of Jared Pon's pdf save for the manner in which it interacts with the memory cell
proc p1 :: | Passer (| MemCell ( [Char] |)) => MemCell ( [Char] |) =
    | passer => mem -> hcase passer of
        Passer -> do
            hput MemPut on mem
            put "P1 of rar" on mem
            fork passer as
                nmem -> nmem |=| mem
                negmemnpasser -> do
                    split negmemnpasser into negmem , npasser
                    plug
                        p1 ( | npasser => z )
                        z , negmem => -> negmem |=| neg z




proc p2 :: | Passer (| MemCell ( [Char] |)) => Passer (| MemCell ( [Char] |)) =
    | passer3 => passer1 -> do 
        hput Passer on passer1 
        split passer1 into mem1 , negmemnpasser1
        hput MemPut on mem1 
        put "P2 of rar" on mem1
        hcase passer3 of
            Passer -> do
                fork passer3 as
                        nmem3 -> nmem3 |=| mem1 -- passing the cell 'forward' to p3
                        negmemnpasser3 -> do -- passing the cell 'back' to p1
                            split negmemnpasser3 into negmem3 , npasser3
                            fork negmemnpasser1 as
                                negmem1 ->  negmem1 |=| negmem3    
                                npasser1 -> p2 ( | npasser3 => npasser1 )
                            
                            
                        
                        



--This is identical to process 'p2' on page 27 of Jared Pon's pdf save for the manner in which it interacts with the memory cell 
proc p3 :: | => Passer ( | MemCell ([Char]|)) =
    | => passer -> do
        hput Passer on passer
        split passer into mem , negmemnpasser
        hput MemPut on mem
        put "P3 of rar" on mem
        fork negmemnpasser as
            negmem -> negmem |=| neg mem
            npasser -> p3 ( | => npasser )




proc run :: | Console =>  =
    | console => -> do
        hput ConsolePut on console 
        put "Program started!" on console 
        plug
            memCell ( "" | console, ch0 => ) --the memory cell starts with the empty string.
            p1 ( | ch1 => ch0)
            p2 ( | ch2 => ch1)
            p3 ( | => ch2)
        
        