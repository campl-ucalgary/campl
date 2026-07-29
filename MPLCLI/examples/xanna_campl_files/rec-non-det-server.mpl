include Prelude (isEmpty | )

-- More details explaining how I solved the problems:
-- How to write a non-deterministic server that recursively races two processes as they each send arbitrarily many messages? This was a problem because, if the clients are sending arbitrarily many messages using a SendMsgs protocol, the non-deterministic server would need to be able to race on the hcases of each channel, but this is not currently supported (and I don't feel like changing the compiler right now). I wanted to be able to demo a deterministic server and then a non-deterministic server to show why non-determinism is important, and I think the impact of this is stronger when a client can send arbitrarily many messages in a row without needing to wait for the other. Therefore, I needed to solve this problem with the current syntax to be able to write such a program. You can see the code I wrote to solve this in the attached demo. The TLDR of my solution was to use a sequential datatype, NewMsg, like a handle and make the channel type a RecvMsgs coprotocol (so the server is setting the channel type to receive a message and the client is hcasing) then we can define the server with a Put(NewMsg|RecvMsgs) channel, which it can race on, and the client indicates that it wants to send a message by sending the Yes(msg) constructor with the message! The server can case on the NewMsg type to see if it's Yes(msg) or End(client_id) and then reset the channel type using hput. The program is only 20 lines longer than the deterministic case! The downside is that we did need to change the client code to use a coprotocol which makes it a little clunky.
-- How to generate arbitrarily many clients and race them recursively? Priyaa and I were trying to solve this in Estonia, but we got stuck. I have now solved this in a few different ways. All of the solutions have a client terminal generation phase to generate arbitrarily many clients and then the server is called to print messages from however many clients were generated.
-- First, I wrote a server that had a channel type of a plist of clients channels. To do this I wrote another implementation of plist_cons which raced the new channel with the first channel in the list and put the winner in the first position and recursively raced the loser against the rest of the list. I called it  plist_cons_race. As the clients are generated, 

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
		fork two_ch as					-- creates two new client processes
			ch1 -> client("Client 1" | => ch1, term1)
			ch2 -> client("Client 2" | => ch2, term2)

proc non_det_server :: | Put(NewMsg | RecvMsgs) (*) Put(NewMsg | RecvMsgs), Console => =  
	| two_ch, console => -> do
		split two_ch into ch1, ch2		-- server splits channel
		race							-- races clients to receive messages in a non-deterministic order
			ch1 -> server_race_rec( | ch1, ch2, console => )	-- client on ch1 wins
			ch2 -> server_race_rec( | ch2, ch1, console => )	-- client on ch2 wins
				
proc run :: | Console => StringTerminal, StringTerminal = 
 	| console => term1, term2 -> plug						
 		two_clients( | => two_ch, term1, term2)			-- creates a two_clients process
 		non_det_server( | two_ch, console => )			-- creates a non-deterministic server