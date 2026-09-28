

-- nesting where statements
-- this should work because the BNFC defines the statements within a where as 
-- arbitrary mpl statements (including defn where statements)

-- however it does not seem to work...

-- coprotocol S => Console = 
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot     

defn
    proc process =
        | ch => -> local1( | ch => )
where
    defn
        proc local2 =
            | ch => -> locallocal( | ch => )
      -- if we add a space before where then it works
      where                           -- why is this giving a parse error??
        proc locallocal =
            | ch => -> on ch do
                hput ConsolePut
                put "Hi from local2"
                hput ConsoleClose
                halt
    defn
        proc local1 =
            | ch => -> local2( | ch => )
    -- just wanted to test if this worked. it dosen't...
    -- defn
    --     proc local1 =
    --         | ch => -> locallocal( | ch => )
    -- where
    --     proc locallocal =
    --         | ch => -> local2( | ch => )


-- without the added space, this gives the error
-- mpl: parse error:
--  •  syntax error at line 21, column 5 before `where'
-- which seems to be a happyerror thrown in file MPL/src/MplLanguage/ParMPL.y

-- but this seems to be a problem with the layouts??

proc run :: | Console => =
    | console => -> process( | console => )