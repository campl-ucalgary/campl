

-- checking that locally defined procs in different scopes (in different wheres) can have same names without overlapping

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

defn
    proc process2 =
        | ch => -> local1( | ch => )
where
    proc local1 =
        | ch => -> on ch do
            hput ConsolePut
            put "Hi from local1"
            hput ConsoleClose
            halt

-- okay local1 can see local2 but neither of them can see the second local1
-- this is what we want

proc run :: | Console => =
    | console => -> process2( | console => )