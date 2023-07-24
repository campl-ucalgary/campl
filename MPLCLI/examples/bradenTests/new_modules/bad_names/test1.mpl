coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

-- TODO test alias-alias errors, alias-localDef errors and module local-name errors

-- include "folder/file.mpl" : M1
-- include M1
-- include "folder/file.mpl" : M1 (a,b|c,d)
-- include M1 (e,f|g,h)
-- include "folder/file.mpl" : M1 (|)
-- include M1 (|)

include "second.mpl" : M1

fun f :: Int -> [Char] =
    a -> "hi there. " ++ M1.f(a)


proc run :: | Console => = 
    | console => -> do
        
        hput ConsolePut on console 
        put "Done" on console
        
        hput ConsoleClose on console
        halt console