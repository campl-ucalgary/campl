
protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S


-- class Kill T where
--      kill :: | T => 

------------------------------- Make TopBot an instance of Kill -------------------------
-- instance Kill TopBot where
-- kill :: | TopBot => =
--    | ch => -> halt ch

-- translation:
proc kill_TopBot :: | TopBot => =
    | ch => -> halt ch


------------------------------- Make Get(A | T) an instance of Kill -------------------------
-- instance Kill T ~> Kill Get(A | T) where
-- kill :: | Get(A | T) => =
--    | ch => -> do
--        put a on ch
--        kill( | ch => )

-- translation
proc kill_Get :: | Neg(T), Get(() | T) => =
    | negT, ch => -> do
        put () on ch
        negT |=| neg ch


------------------------------- Make Put(A | T) an instance of Kill -------------------------
-- instance Kill T ~> Kill Put(A | T) where
-- kill_Put :: | Put(A | T) => =
--    | ch => -> do
--        get _ on ch
--        kill( | ch => )

proc kill_Put :: | Neg(T), Put(A | T) => =
    | negT, ch => -> do
        get a on ch
        negT |=| neg ch



------------------------------- call kill on Get(A | Put (B | TopBot)) -----------------------

proc p :: | Get(() | Put (B | TopBot)) => StringTerminal =
    | ch => strterm -> do
        -- kill(ch)

        -- translation:
        plug
            kill_Get( | negT1, ch => )
            => t1, negT1 -> negT1 |=| neg t1
            kill_Put( | negT2, t1 => )
            => t2, negT2 -> negT2 |=| neg t2
            t2 => z -> do
                close z
                kill_TopBot( | t2 => )
            z => strterm -> do
                close z
                hput StringTerminalPut on strterm
                put "everything is being killed!, press s.th to kill me!" on strterm
                hput StringTerminalGet on strterm
                get _ on strterm
                hput StringTerminalClose on strterm
                halt strterm
            

proc run :: | Get(() | Put(B | TopBot)) => StringTerminal =
    | ch => strterm -> p(| ch => strterm)
