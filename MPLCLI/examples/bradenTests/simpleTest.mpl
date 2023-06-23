
protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

fun isNull =
    [] -> True
    _ -> False

defn
    fun a :: [A] -> [A] =
        [] -> []
        q:qs -> b(qs)
    
    fun b :: [A] -> [A] =
        [] -> []
        q:qs -> a(qs)

proc run :: | => TopBot =
    | => s -> do
        halt s
        
        