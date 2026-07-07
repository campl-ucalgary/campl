include Prelude (isEmpty| )

-- from Example 9:
protocol SendMsgs(A| ) => S =
	SendMsg :: Put(A|S) => S 
	CloseCh :: TopBot => S

-- from Example 10:
protocol SendMsgsWithAck(A| ) => S =
	SendMsgWithAck :: Put(A|Get([Char]|S)) => S
	CloseAckCh :: TopBot => S 
	
-- from Example 12:
coprotocol S => CoSendMsgs(A| ) =
	CoSendMsg ::  S => Get(A|S)
	CoCloseCh :: S => TopBot

-- new code (combines ideas from Examples 15 and 24):
proc sender :: | Console => SendMsgsWithAck([Char]| ) =
	| console => source -> do
		on console do	
			hput ConsolePut
			put "Hello User! Enter message to broadcast. Press ENTER to close."
			hput ConsoleGet
			get msg
		if isEmpty(msg)
			then do
				on console do
					hput ConsolePut
					put "Indicating to destination processes that source is finished."
				on source do 
					hput CloseAckCh
					close 
				on console do
					hput ConsoleClose	
					halt 
			else do
				on source do											
					hput SendMsgWithAck
					put msg
					get ack_msg
				on console do	
					hput ConsolePut
					put ack_msg
				sender( | console => source)

-- from Example 11:
proc broadcast :: [Char] | SendMsgsWithAck([Char]| ) => SendMsgs([Char]| ), SendMsgs([Char]| ) = 
	ack_msg | source => dest1, dest2 -> do
		hcase source of
			SendMsgWithAck -> do
				get msg on source
				on dest1 do
					hput SendMsg
					put msg
				on dest2 do
					hput SendMsg
					put msg
				on source do
					put ack_msg
				broadcast(ack_msg | source => dest1, dest2)
			CloseAckCh -> do
				close source							
				on dest1 do						
					hput CloseCh
					close
				on dest2 do
					hput CloseCh	
					halt

-- from Example 13:
proc send_to_dest1 :: | SendMsgs([Char]| ), CoSendMsgs([Char]| ) => =
	| source, dest1 => -> 
		hcase source of
			SendMsg -> do
				get msg on source 		
				on dest1 do
					hput CoSendMsg
					put msg
				send_to_dest1( | source, dest1 => )
			CloseCh -> do
				close source
				on dest1 do
					hput CoCloseCh
					halt

-- new code (combines ideas from Examples 15 and 24):
proc proc1 :: [Char] | => CoSendMsgs([Char]| ), StringTerminal =
	tag | => source, terminal -> do
		hcase source of
			CoSendMsg -> do
				get msg on source 		
				on terminal do
					hput StringTerminalPut
					put tag ++ " received message: " ++ msg
				proc1(tag | => source, terminal)
			CoCloseCh -> do
				close source
				on terminal do
					hput StringTerminalPut
					put "Source has finished sending messages to " ++ tag ++ ". Press ENTER to close."
					hput StringTerminalGet
					get _
					hput StringTerminalClose
					halt

proc proc2 :: [Char] | SendMsgs([Char]| ) => StringTerminal =
	tag | source => terminal -> do
		hcase source of
			SendMsg -> do
				get msg on source 		
				on terminal do
					hput StringTerminalPut
					put tag ++ " received message: " ++ msg
				proc2(tag | source => terminal)
			CloseCh -> do
				close source
				on terminal do
					hput StringTerminalPut
					put "Source has finished sending messages to " ++ tag ++ ". Press ENTER to close."
					hput StringTerminalGet
					get _
					hput StringTerminalClose
					halt

proc run =
    | console => term1, term2 -> plug
        sender( | console => source)
        broadcast("Message has been broadcasted." | source => dest1, dest2)
        send_to_dest1( | dest1, codest1 => )
        proc1("Process 1" | => codest1, term1)
        proc2("Process 2" | dest2 => term2)
