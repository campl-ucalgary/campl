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

proc server :: | Put([Char] | TopBot) (*) Put([Char] | TopBot) => =  
	| two_ch => -> do
		split two_ch into ch1, ch2		-- server splits channel
		on ch1 do											-- interacts with first client
			get msg						
			close
		on ch2 do											-- interacts with second client
			get msg						
			halt
				
proc run :: | => = 
 	| => -> plug						
 		two_clients( | => two_ch )			-- creates a single two_clients process
 		server( | two_ch => )	