proc close_ch :: | => TopBot = 		-- process that closes the channel and halts
	| => ch ->	
		halt ch

proc left :: | => TopBot (*) TopBot =
	| => two_ch ->
		fork two_ch as								-- forks to create two new processes
			ch1 -> close_ch( | => ch1)
			ch2 -> close_ch( | => ch2)

proc right :: | TopBot (*) TopBot => =  
	| two_ch => -> do
		split two_ch into ch1, ch2			-- process on the right splits channel 
		close ch1 										-- interacts with first process	
		halt ch2										-- interacts with second process
			
proc run :: | => = 
 	| => -> plug						
 		left( | => two_ch )		-- creates a single left process
 		right( | two_ch => )