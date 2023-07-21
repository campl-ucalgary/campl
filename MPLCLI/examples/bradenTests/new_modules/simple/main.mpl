coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

include "folder/file.mpl" : M1
include M2
include "folder/file.mpl" : M3 (a,b|c,d)
include M4 (e,f|g,h)
include "folder/file.mpl" : M3 (|)
include M4 (|)

fun f :: Int -> Int =
    a -> M.f(a)


proc run :: | Console => = 
    | console => -> do
        
        hput ConsolePut on console
        put "Enter your name" on console
        
        on console do
            hput ConsoleGet
            get name
            
            hput ConsolePut
            put "Hello,"
            hput ConsolePut
            put name
        
        hput ConsolePut on console 
        put "Done" on console
        
        hput ConsoleClose on console
        halt console