include Prelude

-- sequential type that we are using like a handle
data NewMsg -> C =
	Yes :: -> C
	End :: -> C 

-- recursive channel type for passing an arbitrary number of messages
-- process sening handles is receiving messages
coprotocol S => RecvMsgs =
    Recv :: S => Put([Char] | Put(NewMsg | S))
    Close :: S => TopBot

fun isEmpty :: [A] -> Bool =
	[] -> True
	_ -> False

-- client code
defn
	proc client :: | => Put(NewMsg | RecvMsgs), StringTerminal =
		| => ch, terminal -> do
			on terminal do
				hput StringTerminalPut
				put "Hello User! Enter message in terminal. Press ENTER to close."
				hput StringTerminalGet
				get msg
			if isEmpty(msg)
				then do
					put End on ch
					client_put_msg("" | => ch, terminal)
				else do
					put Yes on ch
					client_put_msg(msg | => ch, terminal)
	proc client_put_msg :: [Char] | => RecvMsgs, StringTerminal =
		msg | => ch, terminal -> do
			hcase ch of
				Recv -> do
					put msg on ch
					client( | => ch, terminal)
				Close -> do
					close ch
					on terminal do
						hput StringTerminalClose
						halt

-- server code
defn
	proc server_race_NewMsg :: [Char], [Char] | Put(NewMsg | RecvMsgs), Put(NewMsg | RecvMsgs), Console => =  
		wid, lid | winner, loser, console => -> do
			get rec on winner					-- interacts with first client
			case rec of							-- winner now RecvMsgs
				Yes -> do
					hput Recv on winner			-- winner now Put([Char] | Put(NewMsg | RecvMsgs))
					race
						winner -> do
							get msg on winner	-- winner now Put(NewMsg | RecvMsgs)
							hput ConsolePut on console
							put wid ++ ": " ++ msg on console
							race
								winner -> server_race_NewMsg(wid, lid | winner, loser, console => )
								loser -> server_race_NewMsg(lid, wid | loser, winner, console => )
						loser -> server_get_NewMsg(lid, wid | loser, winner, console => ) -- winner still Put([Char] | Put(NewMsg | RecvMsgs))
				End -> do
					hput Close on winner
					close winner
					hput ConsolePut on console
					put wid ++ " ended session."on console
					server_single_client(lid | loser, console => )
	
	proc server_get_NewMsg :: [Char], [Char] | Put(NewMsg | RecvMsgs), Put([Char] | Put(NewMsg | RecvMsgs)), Console => =  
		wid, lid | winner, loser, console => -> do
			get rec on winner					-- interacts with first client
			case rec of							-- winner now RecvMsgs
				Yes -> do
					hput Recv on winner			-- winner now Put([Char] | Put(NewMsg | RecvMsgs))
					race
						winner -> server_get_msg(wid, lid | winner, loser, console => )
						loser -> server_get_msg(lid, wid | loser, winner, console => ) -- winner still Put([Char] | Put(NewMsg | RecvMsgs))
				End -> do
					hput Close on winner
					close winner
					hput ConsolePut on console
					put wid ++ " ended session."on console
					get msg on loser			-- loser now Put(NewMsg | RecvMsgs)
					hput ConsolePut on console
					put lid ++ ": " ++ msg on console
					server_single_client(lid | loser, console => )

	proc server_get_msg :: [Char], [Char] | Put([Char] | Put(NewMsg | RecvMsgs)), Put([Char] | Put(NewMsg | RecvMsgs)), Console => =
		wid, lid | winner, loser, console => -> do
			get msg on winner							-- winner now Put(NewMsg | RecvMsgs)
			hput ConsolePut on console
			put wid ++ ": " ++ msg on console
			race
				winner -> server_get_NewMsg(wid, lid | winner, loser, console => )
				loser -> do
					get msg on loser					-- loser now also Put(NewMsg | RecvMsgs)
					hput ConsolePut on console
					put lid ++ ": " ++ msg on console
					race
						winner -> server_race_NewMsg(wid, lid | winner, loser, console => )
						loser -> server_race_NewMsg(lid, wid | loser, winner, console => )

	proc server_single_client :: [Char] | Put(NewMsg | RecvMsgs), Console => =
		wid | winner, console => -> do
			get rec on winner					-- check if we are recursing
			case rec of							-- winner now RecvMsgs
				Yes -> do
					hput Recv on winner
					get msg on winner	-- winner now Put(NewMsg | RecvMsgs)
					hput ConsolePut on console
					put wid ++ ": " ++ msg on console
					server_single_client(wid | winner, console => )
				End -> do
					hput Close on winner
					close winner
					hput ConsolePut on console
					put wid ++ " ended session."on console
					hput ConsoleClose on console
					halt console

-- wrapper processes
proc two_clients :: | => Put(NewMsg | RecvMsgs) (*) Put(NewMsg | RecvMsgs), StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		on term1 do
			hput StringTerminalPut
			put "Client 1."
		on term2 do
			hput StringTerminalPut
			put "Client 2."
		fork two_ch as									-- creates two new client processes
			ch1 -> client( | => ch1, term1)
			ch2 -> client( | => ch2, term2)

proc non_det_server :: | Put(NewMsg | RecvMsgs) (*) Put(NewMsg | RecvMsgs), Console => =  
	| two_ch, console => -> do
		split two_ch into ch1, ch2						-- server splits channel
		race											-- races clients to receive messages in a non-deterministic order
			ch1 -> server_race_NewMsg("Client 1", "Client 2" | ch1, ch2, console => )	-- client on ch1 wins
			ch2 -> server_race_NewMsg("Client 2", "Client 1" | ch2, ch1, console => )	-- client on ch2 wins
				
proc run :: | Console => StringTerminal, StringTerminal = 
 	| console => term1, term2 -> plug						
 		two_clients( | => two_ch, term1, term2)			-- creates a two_clients process
 		non_det_server( | two_ch, console => )			-- creates a non-deterministic server