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
				client( | => ch, terminal )

-- server code
defn
	proc server_two_clients :: [Char], [Char] | SendMsgs, SendMsgs, Console => =
		wid, lid | winner, loser, console => -> do
			hcase winner of
				SendMsg -> do				-- winner now Put([Char]|SendMsgs)
					race
						winner -> do
							get msg on winner	-- winner now SendMsgs
							hput ConsolePut on console
							put wid ++ ": " ++ msg on console
							race
								winner -> server_two_clients(wid, lid | winner, loser, console => )
								loser -> server_two_clients(lid, wid | loser, winner, console => )
						loser -> server_two_clients_hcase(lid, wid | loser, winner, console => ) -- winner still Put([Char]|SendMsgs)
				CloseCh -> do
					close winner
					hput ConsolePut on console
					put wid ++ " ended session." on console
					server_one_client(lid | loser, console => )			-- don't need to take turns anymore

	proc server_two_clients_hcase :: [Char], [Char] | SendMsgs, Put([Char]|SendMsgs), Console => =  
		wid, lid | winner, loser, console => -> do
			hcase winner of
				SendMsg -> do				-- winner now also Put([Char]|SendMsgs)
					race
						winner -> server_two_clients_get_msg(wid, lid | winner, loser, console => )
						loser -> server_two_clients_get_msg(lid, wid | loser, winner, console => )
				CloseCh -> do
					close winner
					hput ConsolePut on console
					put wid ++ " ended session." on console
					get msg on loser			-- loser now SendMsgs
					hput ConsolePut on console
					put lid ++ ": " ++ msg on console
					server_one_client(lid | loser, console => )

	proc server_two_clients_get_msg :: [Char], [Char] | Put([Char]|SendMsgs), Put([Char]|SendMsgs), Console => =  
		wid, lid | winner, loser, console => -> do
			get msg on winner							-- winner now SendMsgs
			hput ConsolePut on console
			put wid ++ ": " ++ msg on console
			race
				winner -> server_two_clients_hcase(wid, lid | winner, loser, console => )
				loser -> do
					get msg on loser					-- loser now also SendMsgs
					hput ConsolePut on console
					put lid ++ ": " ++ msg on console
					race
						winner -> server_two_clients(wid, lid | winner, loser, console => )
						loser -> server_two_clients(lid, wid | loser, winner, console => )

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


-- wrapper processes
proc two_clients :: | => SendMsgs (*) SendMsgs, StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		on term1 do
			hput StringTerminalPut
			put "Client 1."
		on term2 do
			hput StringTerminalPut
			put "Client 2."
		fork two_ch as								-- creates two new client processes
			ch1 -> client( | => ch1, term1)
			ch2 -> client( | => ch2, term2)

proc server :: | SendMsgs (*) SendMsgs, Console => =  
	| two_ch, console => -> do
		split two_ch into ch1, ch2		-- server splits channel
		race							-- server races channels to receive handles by hcase
			ch1 -> server_two_clients("Client 1", "Client 2" | ch1, ch2, console => )
			ch2 -> server_two_clients("Client 2", "Client 1" | ch2, ch1, console => )
		
			
proc run :: | Console => StringTerminal, StringTerminal = 	
 	| console => term1, term2 -> plug						
 		two_clients( | => two_ch, term1, term2)			-- creates a single two_clients process
 		server( | two_ch, console => )	