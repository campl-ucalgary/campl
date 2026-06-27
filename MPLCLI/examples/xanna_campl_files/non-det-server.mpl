proc client :: | => Put([Char] | TopBot) = 		
	| => ch ->
		on ch do 
			put "Hello Server!"				  -- client process sends string
			halt

proc two_clients :: | => Put([Char] | TopBot) (*) Put([Char] | TopBot)  =
	| => two_ch ->
		fork two_ch as								-- creates two new client processes
			ch1 -> client( | => ch1)
			ch2 -> client( | => ch2)

proc server :: | Put([Char] | TopBot), Put([Char] | TopBot) => =  
	| winner, loser => -> do
		on winner do										-- interacts with first client
			get msg
			close
		on loser do											-- interacts with second client
			get msg			
			halt	

proc non_det_server :: | Put([Char] | TopBot) (*) Put([Char] | TopBot) => =  
	| two_ch => -> do
		split two_ch into ch1, ch2							-- server splits channel
		race												-- races clients
			ch1 -> server( | ch1, ch2 => )					-- client on ch1 wins
			ch2 -> server( | ch2, ch1 => )					-- client on ch2 wins
				
proc run :: | => = 
 	| => -> plug						
 		two_clients( | => two_ch )			-- creates a single two_clients process
 		non_det_server( | two_ch => )	