include Prelude


protocol PList( | M) => S =
    PListEmpty :: TopBot => S
    PListCons :: M (*) S => S

-- coprotocol S => PList( | M) =
--     PListEmpty :: S => TopBot
--     PListCons :: S => M (*) S

proc plist_cons :: | M, PList( | M) => PList( | M) =
    | ch, chs => ret_chs -> do
        on ret_chs do hput PListCons
        fork ret_chs as
            ret_ch with ch -> ret_ch |=| ch
            ret_list with chs -> ret_list |=| chs

proc equate_2 :: | M, P => M, P =
    | in1, in2 => out1, out2 -> plug 
        in1 => dummy, out1 -> do
            on dummy do close
            in1 |=| out1
        in2, dummy => out2 -> do
            on dummy do close
            in2 |=| out2

-- because we only can race on Put: M = Put(A | P)
-- proc p_listrace :: | Put(A | P), PList( | Put(A | P)) => Put(A | P), PList( | Put(A | P)) =
proc p_listrace =
    | ch1, chs => ret_winner, ret_losers -> hcase chs of
        PListEmpty -> do
            -- on chs do close
            -- on ret_losers do
            --     hput PListEmpty
            --     close
            -- ret_winner |=| ch1
            -- what if instead we just id them both (can't because chs is now topbot and not PList)
            -- equate_2( | ch1, chs => ret_winner, ret_losers ) 
            -- ok so what if we make ret losers topbot too and then id them. (works but same error)
            hput PListEmpty on ret_losers           
            equate_2( | ch1, chs => ret_winner, ret_losers ) 

        PListCons -> do
            on chs do split into ch2,new_chs
            plug
                -- subprocess call
                p_listrace( | ch2, new_chs => rec_winner, rec_losers )
                -- racer
                ch1, rec_winner => ret_winner, step_loser -> race
                    rec_winner -> equate_2( | rec_winner, ch1 => ret_winner, step_loser )
                    ch1 -> equate_2( | ch1, rec_winner => ret_winner, step_loser ) 
                -- subprocess call
                plist_cons( | step_loser, rec_losers => ret_losers )


---- Testing protocol lists

-- proc client ::  | => Put([Char] | TopBot), StringTerminal =
proc client =

     | => ch, term -> do
        hput StringTerminalPut on term
        put "Client terminal" on term
        hput StringTerminalGet on term
        get fruit on term                        -- Reading input from the user

        put fruit on ch                          -- send message to server

        hput StringTerminalPut on term           
        put "Press Enter to close" on term
        hput StringTerminalGet on term
        get _ on term 

        hput StringTerminalClose on term         -- close terminal  
        close term

        halt ch





-- ok what if instead of this we use passer on a channel connected to a proc that interfaces with the console?

-- coprotocol S => Console =
--     ...
--     ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

coprotocol S => Passer ( | M) =
    Passer :: S => M (*) (Neg(M) (+) S)
    -- we do fork and then split

coprotocol S => ConCell (A | ) =
    ConPut :: S => Get(A|S)
    -- ConPass :: S => Neg(Get(A|TopBot)) (+) S
    ConCls :: S => TopBot

-- so we have like a console cell instead of a mem cell??
proc conCell :: | Console => ConCell([Char]| ) =
    | console => ch -> hcase ch of
        ConPut -> do
            get str on ch
            hput ConsolePut on console
            put str on console
            conCell( | console => ch )
        -- ConPass -> do
        --     split ch into msg_ch, rec_ch
        --     on msg_ch do 
        --         get str 
        --         close
        --     hput ConsolePut on console
        --     put str on console
        --     conCell( | console => rec_ch )
        ConCls -> do
            close ch
            hput ConsoleClose on console
            halt console



-- protocol Passer( | M ) => S =
--     Passer :: M (+) (Neg(M) (*) S)  => S

-- proc p1 :: | Passer( | MemCell([Char] | ) ) => MemCell([Char]| )
--     | passer => mem, _strterm -> hcase passer of 
--         Passer -> do 
--             ...
--             fork passer as 
--                 mmem with mem -> mmem |=| mem

--                 negmemandnpasser with _strterm -> do
--                     split negmemandnpasser into negmem, npasser
--                     plug 
--                         p1( | npasser => z, _strterm)
--                         z, negmem => -> negmem |=| neg z


-- proc p2 :: |  => Passer( | MemCell([Char] | )), StringTerminal =
--     | => passer, _strterm -> do
--         hput Passer on passer
--         split passer into mem, negmemandnpasser
--         ...
--         fork negmemandnpasser as 
--             negmem with mem -> negmem |=| neg mem 
--             npasser with _strterm -> p2( | => npasser, _strterm )


proc server :: | PList( | Put([Char] | TopBot) ), ConCell([Char]| ) => =
-- proc server =
    | chs, console => -> do
        hcase chs of 
            PListEmpty -> do 
                close chs
                hput ConCls on console
                halt console

            PListCons -> do
                split chs into ch, new_chs
                -- ok what if instead we use a mem cell kind of process but with the console??
                -- cycle :(((
                plug
                    p_listrace( | ch, new_chs => ret_winner, ret_losers )
                    -- neg_term, term =>  -> neg_term |=| neg term
                    ret_winner, console => new_console -> do
                        on ret_winner do
                            get fruit
                            close 
                        on msg_ch do
                            put fruit
                            halt
                    server( | ret_losers, new_console =>  )

-- proc client_wrapper :: | Put(A|TopBot), Put(A|TopBot), Put(A|TopBot), PList( | Put(A|TopBot)) => PList( | Put(A|TopBot)) =
proc client_wrapper = 
    | ch1, ch2, ch3, nil => out_plist ->
        do 
            plug
                plist_cons( | ch3, nil => temp1) 
                plist_cons( | ch2, temp1 => temp2)
                plist_cons( | ch1, temp2 => out_plist)

proc plist_nil :: | => PList( | M) =
    | => nil -> do 
        hput PListEmpty on nil
        halt nil

proc run :: | Console => StringTerminal, StringTerminal, StringTerminal =
    | console => term1, term2, term3 -> plug
        client( | => ch1, term1 )
        client( | => ch2, term2 )
        client( | => ch3, term3 )
        client_wrapper( | ch1, ch2, ch3, nil => out_plist  )
        plist_nil( | => nil)
        server( | out_plist, ch => )
        conCell ( | console => ch )