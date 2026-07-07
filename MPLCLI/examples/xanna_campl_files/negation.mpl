

-- proc client :: | => Put([Char]|TopBot) = 		
-- 	| => ch ->
-- 		on ch do 
-- 			put "Hello Server!"				  -- client sends string
-- 			halt

proc client :: | => Neg(Get([Char] | TopBot)) = 		-- Neg(Get) = Put
    | => neg_ch ->                                    
        plug
            ch => ->                                -- create a new non-neg channel
                on ch do                            -- use like Put channel in original client
                    put "Hello Server!"		
                    halt                 
            => ch, neg_ch -> neg_ch |=| neg ch          -- negate and identify with original


proc server :: | Neg(Get([Char] | TopBot)) => =  
    | neg_ch => ->
        plug
            ch, neg_ch => -> neg_ch |=| neg ch 
            => ch -> 
                on ch do										
                    get msg					
                    halt
			
proc run :: | => = 
 	| => -> plug						
 		client( | => ch )			--
 		server( | ch => )	