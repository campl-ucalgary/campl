include Prelude

protocol SendMsgs => S =
	SendMsg :: Put([Char]|S) => S
	CloseCh :: TopBot => S

fun isEmpty :: [A] -> Bool =
	[] -> True
	_ -> False

-- client code
proc client :: | => SendMsgs, StringTerminal =
	| => ch , terminal -> do
		on terminal do
			hput StringTerminalPut
			put "Enter message in terminal. Press ENTER to close."
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
				client( | => ch, terminal )

-- server code
proc server_one_client :: [Char] | SendMsgs, Console => =
	c1id | ch1, console => -> do
		hcase ch1 of
			SendMsg -> do
				get msg on ch1
				hput ConsolePut on console
				put c1id ++ ": " ++ msg on console
				server_one_client(c1id | ch1, console => )		-- recurse
			CloseCh -> do
				close ch1
				hput ConsolePut on console
				put c1id ++ " ended session." on console
				hput ConsoleClose on console
				halt console

proc server_two_clients :: [Char], [Char] | SendMsgs, SendMsgs, Console => =
	c1id, c2id | ch1, ch2, console => -> do
		hcase ch1 of
			SendMsg -> do
				get msg on ch1
				hput ConsolePut on console
				put c1id ++ ": " ++ msg on console
				server_two_clients(c2id, c1id | ch2, ch1, console => )		-- swap clients so each one gets a turn
			CloseCh -> do
				close ch1
				hput ConsolePut on console
				put c1id ++ " ended session." on console
				server_one_client(c2id | ch2, console => )			-- don't need to take turns anymore

-- wrapper processes
proc two_clients :: | => SendMsgs (*) SendMsgs, StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		on term1 do
			hput StringTerminalPut
			put "Hello Client 1!"
		on term2 do
			hput StringTerminalPut
			put "Hello Client 2!"
		fork two_ch as					-- creates two new client processes
			ch1 -> client( | => ch1, term1)
			ch2 -> client( | => ch2, term2)

proc server :: | SendMsgs (*) SendMsgs, Console => =  
	| two_ch, console => -> do
		split two_ch into ch1, ch2					-- server splits channel
		-- calls process that deterministically swaps which client it receives a message from
		server_two_clients("Client 1", "Client 2" | ch1, ch2, console => )	
		
proc run :: | Console => StringTerminal, StringTerminal = 	
 	| console => term1, term2 -> plug						
 		two_clients( | => two_ch, term1, term2)		-- creates a two_clients process
 		server( | two_ch, console => )				-- creates a server process