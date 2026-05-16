-- This example requires the compiler from the higher-order-procs branch

include Prelude

-- from Example 1:
proc helloworld :: | Console => =
	| console => -> do
		hput ConsolePut on console
		put "Hello World!" on console
		hput ConsoleClose on console
		halt console

-- from Example 18:
proc ho_sender :: | => Put( Store(|Console=>) | TopBot) = 
	| => ch -> do
		on ch do
			put store(helloworld)	
			halt
			
proc ho_receiver :: | Put( Store(|Console=>) | TopBot), Console => =
	| ch, console => -> do
		on ch do
			get stored_process
			close
		on console do
			hput ConsolePut
			put ("Higher order receiver says: Running the stored process")
		use(stored_process)( | console => )

-- new code:
proc run :: | Console => = 
	| console => -> plug
		ho_sender( | => ch)
		ho_receiver( | ch, console => )
