

proc sender =
	| => ch -> do
		on ch do
			put "Hi"
			get confirmation_msg
		split ch into ch1, ch2
		on ch1 do
			put "Hello!"
			get confirmation_msg2
			close
		on ch2 do
			put "hi again"
			get confirmation_msg2b
			halt


-- proc dest2 :: | Put([Char] | TopBot) => =
--     | ch => -> do
-- 		get msg on ch
-- 		halt ch

-- proc dest3 :: | Put([Char] | TopBot) => =
--     | ch => -> do
-- 		get msg on ch
-- 		halt ch

proc receiver_in =
    | ch => -> do
		get msg on ch
		get msg2 on ch
		halt ch

proc receiver_out =
    | => neg_ch -> 
		plug
			=> neg_ch, ch -> neg_ch |=| neg ch
			ch => -> 
				on ch do
					get msg 
					get msg2
					halt


-- proc dest1 :: | => Neg(Put([Char] | TopBot)) =
--     | => neg_ch -> 
-- 		plug
-- 			=> neg_ch, ch -> neg_ch |=| neg ch
-- 			ch => -> 
-- 				on ch do
-- 					get msg 
-- 					halt
						
proc send_to_dest1 =
    | source, neg_dest1 => -> do
		on source do
			get msg
			get msg2
			close
		plug
			neg_dest1, dest1 => -> neg_dest1 |=| neg dest1
			=> dest1 -> 
				on dest1 do
					put msg
					put msg2
					halt

-- :: [Char] | Put([Char] | Get([Char] | TopBot)) => Put([Char] | TopBot), Put([Char] | TopBot), Put([Char] | TopBot)
proc broadcast = 
	confirmation_msg | source => dest1, dest2, dest3 -> do
		get msg on source					-- receives message from source	
		-- send_to_dest1(msg | dest1 => ) -- This command cannot be not the last command in a command block.
		put msg on dest1				-- broadcasts message to each other process
		put msg on dest2
		put msg on dest3
		put confirmation_msg on source
		fork source as
			source1 with dest1, dest2 -> do
				get msg2 on source1
				on dest1 do
					put msg2
					close
				on dest2 do
					put msg2
					close
				put confirmation_msg on source1
				halt source1
			source2 with dest3 -> do
				get msg2 on source2
				on dest3 do
					put msg2
					close
				put confirmation_msg on source2
				halt source2

proc run =
    | => -> plug
        sender( | => source)
        broadcast("message broadcasted" | source => dest1, dest2, dest3)
        send_to_dest1( | dest1, neg_dest1 => )
        receiver_out( | => neg_dest1)
        receiver_in(| dest2 =>)
        receiver_in(| dest3 =>)
