include Prelude (isEmpty | )

-- sequential type that we are using like a handle (but we can also send messages at the same time)
data Msg -> M =
	Yes :: [Char] -> M     -- ordinary content; server resets channel via More
	End :: [Char] -> M     -- carries the cid; server closes channel via Done

-- recursive channel type for passing an arbitrary number of messages in the Put direction
-- process sending handles is receiving messages
coprotocol S => Cont =
	More :: S => Put(Msg | S)
	Done :: S => TopBot

-- client code: stores its own cid, formats messages itself
proc client :: [Char] | => Put(Msg | Cont), StringTerminal =
	cid | => ch, terminal -> do
		on terminal do
			hput StringTerminalPut
			put "Enter message in terminal. Press ENTER to close."
			hput StringTerminalGet
			get msg
		if isEmpty(msg)
			then do
				put End(cid) on ch
				hcase ch of
					Done -> do
						close ch
						on terminal do
							hput StringTerminalClose
							halt
					More -> client(cid | => ch, terminal)
			else do
				put Yes(cid ++ ": " ++ msg) on ch
				hcase ch of
					More -> client(cid | => ch, terminal)
					Done -> do
						close ch
						on terminal do
							hput StringTerminalClose
							halt

-- server code
defn
	proc server_two_clients_race :: | Put(Msg | Cont), Put(Msg | Cont), Console => =  
		| winner, loser, console => -> do
			get m on winner					-- check if we are recursing
			case m of							-- winner now Cont
				Yes(msg) -> do
					hput More on winner			-- winner now Put(Msg | Cont)	
					hput ConsolePut on console
					put msg on console
					race
						winner -> server_two_clients_race( | winner, loser, console => )
						loser -> server_two_clients_race( | loser, winner, console => )
				End(wid) -> do
					hput Done on winner
					close winner
					hput ConsolePut on console
					put wid ++ " ended session." on console
					server_one_client( | loser, console => )
	
	proc server_one_client :: | Put(Msg | Cont), Console => =
		| winner, console => -> do
			get m on winner					-- check if we are recursing
			case m of							-- winner now Cont
				Yes(msg) -> do
					hput More on winner			-- winner now Put(Msg | Cont)
					hput ConsolePut on console
					put msg on console
					server_one_client( | winner, console => )
				End(wid) -> do
					hput Done on winner
					close winner
					hput ConsolePut on console
					put wid ++ " ended session." on console
					hput ConsoleClose on console
					halt console

-- wrapper processes
proc two_clients :: | => Put(Msg | Cont) (*) Put(Msg | Cont), StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		fork two_ch as					-- creates two new client processes
			ch1 -> do
				on term1 do
					hput StringTerminalPut
					put "Hello Client 1!"
				client("Client 1" | => ch1, term1)
			ch2 -> do
				on term2 do
					hput StringTerminalPut
					put "Hello Client 2!"
				client("Client 2" | => ch2, term2)

proc non_det_server :: | Put(Msg | Cont) (*) Put(Msg | Cont), Console => =  
	| two_ch, console => -> do
		split two_ch into ch1, ch2		-- server splits channel
		race							-- races clients to receive messages in a non-deterministic order
			ch1 -> server_two_clients_race( | ch1, ch2, console => )	-- client on ch1 wins
			ch2 -> server_two_clients_race( | ch2, ch1, console => )	-- client on ch2 wins
				
proc run :: | Console => StringTerminal, StringTerminal = 
 	| console => term1, term2 -> plug						
 		two_clients( | => two_ch, term1, term2)			-- creates a two_clients process
 		non_det_server( | two_ch, console => )			-- creates a non-deterministic server