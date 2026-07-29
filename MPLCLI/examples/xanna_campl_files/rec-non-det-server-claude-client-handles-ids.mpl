include Prelude(isEmpty | )

data Msg -> M =
	SendMsg :: [Char] -> M     -- ordinary content; server resets channel via More
	EndMsg  :: [Char] -> M     -- carries the cid; server closes channel via Done

coprotocol S => Cont =
	More :: S => Put(Msg | S)
	Done :: S => TopBot

proc client :: [Char] | => Put(Msg | Cont), StringTerminal =
	cid | => ch , terminal -> do
		on terminal do
			hput StringTerminalPut
			put "Enter message in terminal. Press ENTER to close."
			hput StringTerminalGet
			get msg
		if isEmpty(msg)
			then do
				on ch do
					put (EndMsg(cid))
				hcase ch of
					Done -> do
						close ch
						on terminal do
							hput StringTerminalClose
							halt
					More -> client(cid | => ch, terminal )
			else do
				on ch do
					put (SendMsg(cid ++ ": " ++ msg))
				hcase ch of
					More -> client(cid | => ch, terminal )
					Done -> do
						close ch
						on terminal do
							hput StringTerminalClose
							halt

-- no client id needed: the message itself already carries whatever
-- text should be printed
proc server_one_client :: | Put(Msg | Cont), Console => =
	| ch1, console => -> do
		get m on ch1
		case m of
			SendMsg(formatted) -> do
				on ch1 do
					hput More
				hput ConsolePut on console
				put formatted on console
				server_one_client( | ch1, console => )
			EndMsg(cid) -> do
				on ch1 do
					hput Done
					close
				hput ConsolePut on console
				put cid ++ " ended session." on console
				hput ConsoleClose on console
				halt console

proc server_two_clients_race :: | Put(Msg | Cont), Put(Msg | Cont), Console => =
	| ch1, ch2, console => -> do
		race
			ch1 -> do
				get m on ch1
				case m of
					SendMsg(formatted) -> do
						on ch1 do
							hput More
						hput ConsolePut on console
						put formatted on console
						server_two_clients_race( | ch1, ch2, console => )
					EndMsg(cid) -> do
						on ch1 do
							hput Done
							close
						hput ConsolePut on console
						put cid ++ " ended session." on console
						server_one_client( | ch2, console => )
			ch2 -> do
				get m on ch2
				case m of
					SendMsg(formatted) -> do
						on ch2 do
							hput More
						hput ConsolePut on console
						put formatted on console
						server_two_clients_race( | ch1, ch2, console => )
					EndMsg(cid) -> do
						on ch2 do
							hput Done
							close
						hput ConsolePut on console
						put cid ++ " ended session." on console
						server_one_client( | ch1, console => )

proc two_clients :: | => Put(Msg|Cont) (*) Put(Msg|Cont), StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		on term1 do
			hput StringTerminalPut
			put "Hello Client 1!"
		on term2 do
			hput StringTerminalPut
			put "Hello Client 2!"
		fork two_ch as
			ch1 -> client("Client 1" | => ch1, term1)
			ch2 -> client("Client 2" | => ch2, term2)

proc server :: | Put(Msg|Cont) (*) Put(Msg|Cont), Console => =
	| two_ch, console => -> do
		split two_ch into ch1, ch2
		server_two_clients_race( | ch1, ch2, console => )

proc run :: | Console => StringTerminal, StringTerminal =
	| console => term1, term2 -> plug
		two_clients( | => two_ch, term1, term2)
		server( | two_ch, console => )