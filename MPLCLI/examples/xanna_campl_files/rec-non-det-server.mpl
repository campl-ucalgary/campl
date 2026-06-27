include Prelude (intToString, isEmpty | )

-- sequential type that we are using like a handle (but we can also send messages at the same time)
data NewMsg -> C =
	Yes :: [Char] -> C
	End :: [Char] -> C 

-- recursive channel type for passing an arbitrary number of messages
-- process sending handles is receiving messages
coprotocol S => RecvMsgs =
    Recv :: S => Put(NewMsg | S)
    Close :: S => TopBot

-- client code
defn
    proc client :: [Char] | => Put(NewMsg | RecvMsgs), StringTerminal =
        cid | => ch, terminal -> do
            on terminal do
                hput StringTerminalPut
                put "Enter message in terminal. Press ENTER to close."
                hput StringTerminalGet
                get msg
            if isEmpty(msg)
                then do
                    put End(cid) on ch
                    client_recurse(cid | => ch, terminal)
                else do
                    put Yes(cid ++ ": " ++ msg) on ch
                    client_recurse(cid | => ch, terminal)
    proc client_recurse :: [Char] | => RecvMsgs, StringTerminal =
        cid | => ch, terminal -> do
            hcase ch of
                Recv -> do
                    client(cid | => ch, terminal)
                Close -> do
                    close ch
                    on terminal do
                        hput StringTerminalClose
                        halt

-- server code
defn
	proc server_race_rec :: | Put(NewMsg | RecvMsgs), Put(NewMsg | RecvMsgs), Console => =  
		| winner, loser, console => -> do
			get rec on winner					-- check if we are recursing
			case rec of							-- winner now RecvMsgs
				Yes(msg) -> do
					hput Recv on winner			-- winner now Put(NewMsg | RecvMsgs)	
					hput ConsolePut on console
					put msg on console
					race
						winner -> server_race_rec( | winner, loser, console => )
						loser -> server_race_rec( | loser, winner, console => )
				End(wid) -> do
					hput Close on winner
					close winner
					hput ConsolePut on console
					put wid ++ " ended session." on console
					server_single_client( | loser, console => )
	
	proc server_single_client :: | Put(NewMsg | RecvMsgs), Console => =
		| winner, console => -> do
			get rec on winner					-- check if we are recursing
			case rec of							-- winner now RecvMsgs
				Yes(msg) -> do
					hput Recv on winner			-- winner now Put(NewMsg | RecvMsgs)
					hput ConsolePut on console
					put msg on console
					server_single_client( | winner, console => )
				End(wid) -> do
					hput Close on winner
					close winner
					hput ConsolePut on console
					put wid ++ " ended session." on console
					hput ConsoleClose on console
					halt console

-- wrapper processes
proc two_clients :: | => Put(NewMsg | RecvMsgs) (*) Put(NewMsg | RecvMsgs), StringTerminal, StringTerminal =
	| => two_ch, term1, term2 -> do
		on term1 do
			hput StringTerminalPut
			put "Hello Client 1!"
		on term2 do
			hput StringTerminalPut
			put "Hello Client 2!"
		fork two_ch as									-- creates two new client processes
			ch1 -> client("Client 1" | => ch1, term1)
			ch2 -> client("Client 2" | => ch2, term2)

proc non_det_server :: | Put(NewMsg | RecvMsgs) (*) Put(NewMsg | RecvMsgs), Console => =  
	| two_ch, console => -> do
		split two_ch into ch1, ch2						-- server splits channel
		race											-- races clients to receive messages in a non-deterministic order
			ch1 -> server_race_rec( | ch1, ch2, console => )	-- client on ch1 wins
			ch2 -> server_race_rec( | ch2, ch1, console => )	-- client on ch2 wins
				
proc run :: | Console => StringTerminal, StringTerminal = 
 	| console => term1, term2 -> plug						
 		two_clients( | => two_ch, term1, term2)			-- creates a two_clients process
 		non_det_server( | two_ch, console => )			-- creates a non-deterministic server