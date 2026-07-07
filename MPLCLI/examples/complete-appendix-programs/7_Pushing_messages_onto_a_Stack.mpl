include Prelude (isEmpty| )

-- from Example 9:
protocol SendMsgs(A| ) => S =
	SendMsg :: Put(A|S) => S 
	CloseCh :: TopBot => S

-- from Example 21:
data SF(A) -> C =
	SS :: A -> C
	FF :: -> C
	
codata S -> Stack(A) = 
	Push :: A, S -> S
	Pop :: S -> (SF(A),S)	

-- from Example 22:	
fun listStack :: [A] -> Stack(A) = 
	cs -> (
		Push := c -> listStack(c:cs),
		Pop := -> case cs of	
			b:bs -> (SS(b), listStack(bs))
			[] -> (FF, listStack([]))
		)

-- from Example 23:
defn        
	proc reverse_print :: Stack([Char]) | SendMsgs([Char]| ), Console => =
		stack | source, console => -> do
			hcase source of
				SendMsg -> do
					get msg on source
					reverse_print(Push(msg, stack) | source, console => )
				CloseCh -> do
					close source
					on console do
						hput ConsolePut
						put "Printing stack from top to bottom: " ++ flatten(stack)
						hput ConsoleClose
						halt
where
	fun flatten :: Stack([Char]) -> [Char] =
		s -> case Pop(s) of
			(SS(str), stack) -> str ++ " " ++ flatten(stack)
			(FF, stack) -> ""


-- from Example 24:
proc client :: | => SendMsgs([Char]|), StringTerminal =
	| => ch, terminal -> do
		on terminal do	
			hput StringTerminalPut
			put "Hello User! Enter message in terminal. Press ENTER to close."
			hput StringTerminalGet
			get msg
		if isEmpty(msg)
			then do
				on ch do 
					hput CloseCh
					close 
				on terminal do
					hput StringTerminalClose	
					halt 
			else do
				on ch do											
					hput SendMsg
					put msg
				client( | => ch, terminal)

-- new code:
proc server :: | SendMsgs([Char]| ), Console => =
	| ch, console => -> do
		on console do
			hput ConsolePut
			put "Collecting messages to print in reverse."
		reverse_print(listStack([]) | ch, console => )	-- initializes stack

proc run :: | Console => StringTerminal = 
    | console => terminal -> plug
        client( | => ch, terminal)
        server( | ch, console => )
        
