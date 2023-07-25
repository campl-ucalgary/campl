-- should print 'hi++'

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

-- include "folder/file.mpl" : M1
-- include M1
-- include "folder/file.mpl" : M1 (a,b|c,d)
-- include M1 (e,f|g,h)
-- include "folder/file.mpl" : M1 (|)
-- include M1 (|)

include "Second.mpl" : M1

fun f :: Int -> [Char] =
    a -> "hi" ++ M1.f(a)


proc run :: | Console => = 
    | console => -> do
        
        hput ConsolePut on console 
        put f(2) on console
        
        hput ConsoleClose on console
        halt console