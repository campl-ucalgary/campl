-- should print 'hi++'

include "folder/Test1.mpl" : T1
include "folder/folder/Second.mpl" : Second (f|)
include "folder/Second.mpl" : Third (|)

fun g :: Int -> [Char] =
    n -> T1.f(n) ++ f(n)

proc run :: | Console => = 
    | console => -> do
        
        hput ConsolePut on console 
        put g(1) on console
        
        hput ConsoleClose on console
        halt console
