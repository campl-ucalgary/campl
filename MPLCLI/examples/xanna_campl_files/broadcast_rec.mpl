
protocol PassMessages (A | ) => S =			-- passes messages of type A
    SendMsg :: Put(A | S) => S 					-- handle to send another message
    CloseCh :: TopBot => S 	


proc sender =
    | => ch ->
        on ch do
            hput SendMsg
            put "Hi"
            hput CloseCh
            halt

proc receiver_in =
    | ch => -> do
        hcase ch of
            SendMsg -> do
                get msg on ch
                receiver_in(| ch => )
            CloseCh ->
                halt ch

proc receiver_out =
    | => neg_ch -> 
		plug
			=> neg_ch, ch -> neg_ch |=| neg ch
			ch => -> 
				hcase ch of
					SendMsg -> do
						get msg on ch
						plug
							new_neg_ch, ch => -> new_neg_ch |=| neg ch
							receiver_out(| => new_neg_ch )
					CloseCh ->
						halt ch



-- proc broadcast :: [Char] | PassMessages([Char]| ), Neg(PassMessages([Char]| )) => PassMessages([Char]| ), PassMessages([Char]| ) = 
-- 	confirmation_msg | source, dest1 => dest2, dest3 -> do
-- 		hcase source of					-- check whether there is another message
-- 			SendMsg -> do
-- 				get msg on source		-- receive message
-- 					-- send that message
-- 				on dest2 do
-- 					hput SendMsg
-- 					put msg
-- 				on dest3 do
-- 					hput SendMsg
-- 					put msg
-- 				plug 
-- 					dest1, z => -> dest1 |=| neg z
-- 					source => dest2, dest3, z -> do
-- 						on z do					-- broadcast message to other processes
-- 							hput SendMsg			-- indicate there is another message
-- 							put msg	
-- 						plug 
-- 							=> z, neg_z -> neg_z |=| neg z
-- 							broadcast(confirmation_msg | source, neg_z => dest2, dest3) -- recurse
-- 			CloseCh -> do					-- close all channels and halt
-- 				close source									
-- 				on dest2 do
-- 					hput CloseCh		
-- 					close	
-- 				on dest3 do
-- 					hput CloseCh		
-- 					close	
-- 				plug 
-- 					dest1, z => -> dest1 |=| neg z
-- 					=> z -> do
-- 						on z do						
-- 							hput CloseCh			-- indicate source is finished
-- 							halt	

proc receiver_out_negator :: | PassMessages([Char]| ), Neg(PassMessages([Char]| )) => =
	| source, receiver => -> 
		hcase source of					-- check whether there is another message
			SendMsg -> do
				get msg on source
				plug 
					receiver, z => -> receiver |=| neg z
					source => z -> do
						on z do					-- forward message to receiver
							hput SendMsg			-- indicate there is another message
							put msg	
						plug 
							=> z, neg_z -> neg_z |=| neg z
							receiver_out_negator( | source, neg_z => ) -- recurse
			CloseCh -> do					-- close all channels and halt
				close source	
				plug 
					receiver, z => -> receiver |=| neg z
					=> z -> do
						on z do						
							hput CloseCh			-- indicate source is finished
							halt		



proc broadcast :: [Char] | PassMessages([Char]| ) 
	=> PassMessages([Char]| ), PassMessages([Char]| ), PassMessages([Char]| ) = 
	confirmation_msg | source => dest1, dest2, dest3 -> do
		hcase source of					-- check whether there is another message
			SendMsg -> do
				get msg on source		-- receive message
				on dest1 do				-- broadcast message to other processes
					hput SendMsg
					put msg
				on dest2 do				
					hput SendMsg
					put msg
				on dest3 do
					hput SendMsg
					put msg
				broadcast(confirmation_msg | source => dest1, dest2, dest3) -- recurse
			CloseCh -> do					-- close all channels and halt
				close source									
				on dest1 do
					hput CloseCh		
					close										
				on dest2 do
					hput CloseCh		
					close	
				on dest3 do
					hput CloseCh		
					halt		


proc run =
    | => -> plug
        sender( | => source)
        broadcast("message broadcasted" | source => dest1, dest2, dest3)
        receiver_out_negator( | dest1, neg_dest1 => )
        -- send_to_dest1( | dest1, neg_dest1 => )
        receiver_out( | => neg_dest1)
        receiver_in(| dest2 =>)
        receiver_in(| dest3 =>)
