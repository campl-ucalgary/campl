include Prelude (isEmpty| )
    
-- modified from Example 26:
protocol Passer( |R) => S =
	Pass :: R (+) Neg(S) => S
	Done :: TopBot => S 									-- this handle is new
    
-- new code:
protocol MemCh(A| ) => S =							-- memory cell access protocol
	MemPut :: Put(A|S) => S 							-- write
	MemGet :: Get(A|S) => S 							-- read
	MemCls :: TopBot => S 								-- close

proc memCell :: A | MemCh(A| ) => =			-- memory cell process
	val | ch => -> hcase ch of
		MemPut -> do 												-- overwrite existing stored value
			get nval on ch
			memCell(nval | ch => )
		MemGet -> do 												-- send stored value
			put val on ch
			memCell(val | ch => )
		MemCls -> do 												-- close
			halt ch

proc memDone :: | Passer( |MemCh([Char]| )) => MemCh([Char]| ),  StringTerminal =
	| passer => mem, terminal -> do
		on terminal do 											-- user wants to close
			hput StringTerminalClose
			close
		hcase passer of  										-- check if other proc wants mem
			Pass -> do
				fork passer as
					pass_mem with mem -> 					-- if they do, pass it back
						pass_mem |=| mem
					neg_passer -> plug
						neg_passer, new_passer => ->		
							neg_passer |=| neg new_passer
						=> new_passer -> on new_passer do
							hput Done									-- but don't request it again
							halt
			Done -> do 												-- close if other proc is already done
				close passer
				on mem do
					hput MemCls
					halt
					
-- modified from Example 27:
defn
	proc memAccess :: [Char] | Passer( |MemCh([Char]| )) => MemCh([Char]| ), StringTerminal =
		tag | passer => mem, terminal -> do	-- terminal is new
			on mem do
				hput MemGet
				get mem_data
			on terminal do										-- print mem data, get user input
				hput StringTerminalPut
				put tag ++ " read value: " ++ mem_data
				hput StringTerminalPut
				put tag ++ ", please enter a string or press ENTER to close."
				hput StringTerminalGet
				get user_input
			if isEmpty(user_input)						-- check if user wants to close
				then memDone( | passer => mem, terminal)
				else do													-- if they don't, it's the same
					on mem do
						hput MemPut
						put user_input
					hcase passer of 
						Pass -> do
							fork passer as
								pass_mem with mem -> 
									pass_mem |=| mem
								neg_passer with terminal -> plug
									neg_passer, new_passer => ->		
										neg_passer |=| neg new_passer
									memWait(tag | => new_passer, terminal)	-- and keep terminal
						Done -> do									-- but other proc might be done
							close passer
							on mem do
								hput MemCls
								close
							on terminal do
								hput StringTerminalPut
								put "Other process is done. Press ENTER to close."
								hput StringTerminalGet
								get _
								hput StringTerminalClose
								halt
		
	proc memWait :: [Char] | => Passer( |MemCh([Char]| )), StringTerminal =
		tag | => passer, terminal -> do										-- terminal is new
			hput Pass on passer
			split passer into mem, neg_passer
			plug
				=> neg_passer, new_passer -> 
					neg_passer |=| neg new_passer
				memAccess(tag | new_passer => mem, terminal)	-- and used in memAccess

proc run =
	| => terminalPing, terminalPong -> plug 
		memCell( "" | mem => )
		memAccess("Ping" | passer => mem, terminalPing)
		memWait("Pong" | => passer, terminalPong)
