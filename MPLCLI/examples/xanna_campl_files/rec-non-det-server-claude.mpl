include Prelude(isEmpty | )

-- The only session-level choice, reached strictly after a payload
-- has already been received on a plain Put channel.
protocol Cont => T =
	More :: Put([Char] | Cont) => T
	Done :: TopBot             => T

-- client code (per-client terminal, repeated send, explicit close)
proc client :: | => Put([Char] | Cont), StringTerminal =
	| => ch , terminal -> do
		on terminal do
			hput StringTerminalPut
			put "Enter message in terminal. Press ENTER to close."
			hput StringTerminalGet
			get msg
		on ch do
			put msg							-- always send first (empty string if closing)
		if isEmpty(msg)
			then do
				on ch do
					hput Done				-- only *now* do we touch the session choice
					close
				on terminal do
					hput StringTerminalClose
					halt
			else do
				on ch do
					hput More
				client( | => ch, terminal )

-- handles whichever single client is left once the other has closed
proc server_one_client :: [Char] | Put([Char] | Cont), Console => =
	c1id | ch1, console => -> do
		get msg on ch1
		hcase ch1 of
			More -> do
				hput ConsolePut on console
				put c1id ++ ": " ++ msg on console
				server_one_client(c1id | ch1, console => )
			Done -> do
				close ch1
				hput ConsolePut on console
				put c1id ++ " ended session." on console
				hput ConsoleClose on console
				halt console

-- nondeterministic server: races the plain `get`s directly
proc server_two_clients_race :: [Char], [Char] | Put([Char] | Cont), Put([Char] | Cont), Console => =
	c1id, c2id | ch1, ch2, console => -> do
		race									-- both branches' pending action is a bare `get` -- legal
			ch1 -> do
				get msg on ch1
				hcase ch1 of					-- happens *after* race resolved -- no longer constrained
					More -> do
						hput ConsolePut on console
						put c1id ++ ": " ++ msg on console
						server_two_clients_race(c1id, c2id | ch1, ch2, console => )
					Done -> do
						close ch1
						hput ConsolePut on console
						put c1id ++ " ended session." on console
						server_one_client(c2id | ch2, console => )
			ch2 -> do
				get msg on ch2
				hcase ch2 of
					More -> do
						hput ConsolePut on console
						put c2id ++ ": " ++ msg on console
						server_two_clients_race(c1id, c2id | ch1, ch2, console => )
					Done -> do
						close ch2
						hput ConsolePut on console
						put c2id ++ " ended session." on console
						server_one_client(c1id | ch1, console => )

-- wrapper processes (unchanged in structure)
proc two_clients :: | => Put([Char] | Cont) (*) Put([Char] | Cont), StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		on term1 do
			hput StringTerminalPut
			put "Hello Client 1!"
		on term2 do
			hput StringTerminalPut
			put "Hello Client 2!"
		fork two_ch as
			ch1 -> client( | => ch1, term1)
			ch2 -> client( | => ch2, term2)

proc server :: | Put([Char] | Cont) (*) Put([Char] | Cont), Console => =  
	| two_ch, console => -> do
		split two_ch into ch1, ch2
		server_two_clients_race("Client 1", "Client 2" | ch1, ch2, console => )

proc run :: | Console => StringTerminal, StringTerminal = 	
 	| console => term1, term2 -> plug						
 		two_clients( | => two_ch, term1, term2)
 		server( | two_ch, console => )