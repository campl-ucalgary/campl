include Prelude(isEmpty | )

-- Coprotocol: the CONSUMER (whoever just did `get`) selects More/Done
-- immediately, based on the message it just received -- no separate
-- action from the client is needed to reach TopBot.
coprotocol T => Cont =
	More :: T => Put([Char] | Cont)
	Done :: T => TopBot

-- client: provider of ch. Puts its message, then just hears (via hcase)
-- what the server decided -- which will always match isEmpty(msg), since
-- that's exactly what the server's decision is based on.
proc client :: | => Put([Char] | Cont), StringTerminal =
	| => ch , terminal -> do
		on terminal do
			hput StringTerminalPut
			put "Enter message in terminal. Press ENTER to close."
			hput StringTerminalGet
			get msg
		on ch do
			put msg
		hcase ch of
			More -> client( | => ch, terminal )
			Done -> do
				close ch
				on terminal do
					hput StringTerminalClose
					halt

-- handles whichever single client is left once the other has closed
proc server_one_client :: [Char] | Put([Char] | Cont), Console => =
	c1id | ch1, console => -> do
		get msg on ch1
		if isEmpty(msg)
			then do
				on ch1 do
					hput Done
					close
				hput ConsolePut on console
				put c1id ++ " ended session." on console
				hput ConsoleClose on console
				halt console
			else do
				on ch1 do
					hput More
				hput ConsolePut on console
				put c1id ++ ": " ++ msg on console
				server_one_client(c1id | ch1, console => )

-- nondeterministic server: races the two plain `get`s. Each branch decides
-- More/Done for itself, immediately, from the message it just received --
-- so no client can ever hold up the other.
proc server_two_clients_race :: [Char], [Char] | Put([Char] | Cont), Put([Char] | Cont), Console => =
	c1id, c2id | ch1, ch2, console => -> do
		race
			ch1 -> do
				get msg on ch1
				if isEmpty(msg)
					then do
						on ch1 do hput Done; close
						hput ConsolePut on console
						put c1id ++ " ended session." on console
						server_one_client(c2id | ch2, console => )
					else do
						on ch1 do hput More
						hput ConsolePut on console
						put c1id ++ ": " ++ msg on console
						server_two_clients_race(c1id, c2id | ch1, ch2, console => )
			ch2 -> do
				get msg on ch2
				if isEmpty(msg)
					then do
						on ch2 do hput Done; close
						hput ConsolePut on console
						put c2id ++ " ended session." on console
						server_one_client(c1id | ch1, console => )
					else do
						on ch2 do hput More
						hput ConsolePut on console
						put c2id ++ ": " ++ msg on console
						server_two_clients_race(c1id, c2id | ch1, ch2, console => )

-- wrappers (unchanged)
proc two_clients :: | => Put([Char]|Cont) (*) Put([Char]|Cont), StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		on term1 do hput StringTerminalPut; put "Hello Client 1!"
		on term2 do hput StringTerminalPut; put "Hello Client 2!"
		fork two_ch as
			ch1 -> client( | => ch1, term1)
			ch2 -> client( | => ch2, term2)

proc server :: | Put([Char]|Cont) (*) Put([Char]|Cont), Console => =
	| two_ch, console => -> do
		split two_ch into ch1, ch2
		server_two_clients_race("Client 1", "Client 2" | ch1, ch2, console => )

proc run :: | Console => StringTerminal, StringTerminal =
	| console => term1, term2 -> plug
		two_clients( | => two_ch, term1, term2)
		server( | two_ch, console => )