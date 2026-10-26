

-- calling the run process

-- coprotocol S => Console = 
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot     

defn 
    proc process =
        | ch => -> run( | ch => )

    proc run :: | Console => =
        | console => -> do
            on console do
                hput ConsolePut
                put "Hi from run"
            process( | console => )


-- idk if this should work or not but it sure does

-- output:
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- ^CHi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run
-- Hi from run