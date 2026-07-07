

proc sender =
    | => ch ->
        on ch do
            put "Hi"
            get confirmation_msg
            halt


-- proc dest2 :: | Put([Char] | TopBot) => =
--     | ch => -> do
-- 		get msg on ch
-- 		halt ch

-- proc dest3 :: | Put([Char] | TopBot) => =
--     | ch => -> do
-- 		get msg on ch
-- 		halt ch

proc receiver_in :: | Put([Char] | TopBot) => =
    | ch => -> do
		get msg on ch
		halt ch

proc receiver_out :: | => Neg(Put([Char] | TopBot)) =
    | => neg_ch -> 
		plug
			=> neg_ch, ch -> neg_ch |=| neg ch
			ch => -> 
				on ch do
					get msg 
					halt


-- proc dest1 :: | => Neg(Put([Char] | TopBot)) =
--     | => neg_ch -> 
-- 		plug
-- 			=> neg_ch, ch -> neg_ch |=| neg ch
-- 			ch => -> 
-- 				on ch do
-- 					get msg 
-- 					halt
						
proc send_to_dest1 :: | Put([Char] | TopBot), Neg(Put([Char] | TopBot)) => =
    | source, neg_dest1 => -> do
		on source do
			get msg
			close
		plug
			neg_dest1, dest1 => -> neg_dest1 |=| neg dest1
			=> dest1 -> 
				on dest1 do
					put msg
					halt


proc broadcast :: [Char] | Put([Char] | Get([Char] | TopBot)) => Put([Char] | TopBot), Put([Char] | TopBot), Put([Char] | TopBot) = 
	confirmation_msg | source => dest1, dest2, dest3 -> do
		get msg on source					-- receives message from source	
		-- send_to_dest1(msg | dest1 => ) -- This command cannot be not the last command in a command block.
		put msg on dest1				-- broadcasts message to each other process
		put msg on dest2
		put msg on dest3
		put confirmation_msg on source
		close source
		close dest1
		close dest2
		halt dest3


proc run =
    | => -> plug
        sender( | => source)
        broadcast("message broadcasted" | source => dest1, dest2, dest3)
        send_to_dest1( | dest1, neg_dest1 => )
        receiver_out( | => neg_dest1)
        receiver_in(| dest2 =>)
        receiver_in(| dest3 =>)
