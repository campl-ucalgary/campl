-- from Example 28:
proc sender :: | => Put([Char] | Get([Char] | TopBot)) =
	| => source ->
		on source do
			put "Hi everyone!"
			get ack_msg
			halt
			
-- from Example 3:
proc broadcast :: [Char] | Put([Char] | Get([Char] | TopBot)) 								=> Put([Char] | TopBot), Put([Char] | TopBot) =
	ack_msg | source => dest1, dest2 -> do
		get msg on source
		put msg on dest1
		put msg on dest2
		put ack_msg on source
		close source
		close dest1
		halt dest2
		
-- from Example 8:
proc send_to_dest1 :: | Put([Char] | TopBot), Neg(Put([Char] | TopBot)) => =
	| source, neg_dest1 => -> do
		on source do
			get msg
			close
		plug
			neg_dest1, dest1 => ->
				neg_dest1 |=| neg dest1
			=> dest1 ->
				on dest1 do	
					put msg
					halt
					
-- new code:
proc proc1 :: | => Neg(Put([Char] | TopBot)) =
	| => neg_dest1 -> plug
		=> neg_dest1, dest1 ->
			neg_dest1 |=| neg dest1
		dest1 => ->
			on dest1 do	
				get msg
				halt

proc proc2 :: | Put([Char] | TopBot) => =
	| dest2 => ->
		on dest2 do
			get msg
			halt

-- from Example 8:
proc run =
	| => -> plug
		sender( | => source)
		broadcast("message broadcasted" | source => dest1, dest2)
		send_to_dest1( | dest1, neg_dest1 => )
		proc1( | => neg_dest1)	
		proc2( | dest2 =>)
