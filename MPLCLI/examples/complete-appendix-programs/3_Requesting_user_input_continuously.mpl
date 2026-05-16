include Prelude (isEmpty| )

-- from Example 9:
protocol SendMsgs (A| ) => S =
    SendMsg :: Put(A|S) => S 
    CloseCh :: TopBot => S

-- from Example 24:
proc client :: | => SendMsgs([Char]|), StringTerminal =
	| => ch, terminal -> do
		on terminal do	
			hput StringTerminalPut
			put "Hello User! Enter message in terminal. Press ENTER to close."
			hput StringTerminalGet
			get msg
		if isEmpty(msg)
			then do
				on ch do 
					hput CloseCh
					close 
				on terminal do
					hput StringTerminalClose	
					halt 
			else do
				on ch do											
					hput SendMsg
					put msg
				client( | => ch, terminal)

-- from Example 15:
proc server :: | SendMsgs([Char]|), Console => =
    | ch, console => -> do
        hcase ch of
            SendMsg -> do
                get msg on ch
                on console do
                    hput ConsolePut
                    put "message from user: " ++ msg
                server( | ch, console => )
            CloseCh -> do
                close ch
                on console do
                    hput ConsoleClose
                    halt
                    
proc run :: | Console => StringTerminal = 
    | console => terminal -> plug
        client( | => ch, terminal)
        server( | ch, console => )
