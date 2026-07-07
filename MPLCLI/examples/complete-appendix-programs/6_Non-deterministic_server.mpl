-- from Example 16:
defn
	proc server =
		| two_ch => -> do
			split two_ch into ch1, ch2
			server_non_deterministic( | ch1, ch2 => )

where defn
	proc server_non_deterministic =
		| ch1, ch2 => -> do
			race
				ch1 -> server_deterministic( | ch1, ch2 => )	
				ch2 -> server_deterministic( | ch2, ch1 => )	
	
	proc server_deterministic =
		| winner, loser => -> do
			on winner do
				get msg
				put msg
				close
			on loser do
				get msg
				put msg
				halt

-- from Example 2:
proc client = 
	| => ch ->
		on ch do 
			put "Hello Server!"
			get echo	
			halt

-- from Example 7:
proc two_clients =
	| => two_ch ->
		fork two_ch as	
			ch1 -> client( | => ch1)
			ch2 -> client( | => ch2)
			
proc run :: | => = 
 	| => -> plug						
 		two_clients( | => two_ch)	
 		server( | two_ch => )
