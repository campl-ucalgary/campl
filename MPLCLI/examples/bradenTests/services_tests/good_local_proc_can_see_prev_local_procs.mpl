

-- checking whether locally defined procs (in where) can see each other

-- coprotocol S => Console = 
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot     

defn
    proc process =
        | ch => -> local1( | ch => )
where
    proc local2 =
        | ch => -> on ch do
            hput ConsolePut
            put "Hi from local2"
            hput ConsoleClose
            halt
    proc local1 =
        | ch => -> local2( | ch => )

-- okay local1 can see local2 if local2 is defined first
-- so local functions (within the same scope level) just need to be defined before they are used 
-- (same as how we need to define things before we can use them in the global scope)

-- if we define local1 and then local2 then we get the error
-- mpl: rename error:
--  •  Out of scope identifier `local2' at line 16 and column 20

-- this means that the names of things in the wheres cannot conflict with each other

proc run :: | Console => =
    | console => -> process( | console => )