include Prelude

protocol PList( | M) => S =
    PListEmpty :: TopBot => S
    PListCons :: M (*) S => S

proc plist_nil :: | => PList( | M) =
    | => nil -> do 
        hput PListEmpty on nil
        halt nil




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
 proc p_listrace :: | Put(A | P), PList( | Put(A | P)) => Put(A | P), PList( | Put(A | P)) =
    | ch1, chs => ret_winner, ret_losers -> hcase chs of
        PListEmpty -> do
            -- this didn't change anything but it makes it fewer lines.
            hput PListEmpty on ret_losers           
            equate_2( | ch1, chs => ret_winner, ret_losers ) 

        PListCons -> do
            on chs do split into ch2, new_chs
            plug
                -- subprocess call
                p_listrace( | ch2, new_chs => rec_winner, rec_losers )
                -- racer
                ch1, rec_winner => ret_winner, step_loser -> 
                    race
                        rec_winner -> equate_2( | rec_winner, ch1 => ret_winner, step_loser )
                        ch1 -> equate_2( | ch1, rec_winner => ret_winner, step_loser ) 
                -- subprocess call
                plist_cons( | step_loser, rec_losers => ret_losers )


---- Testing protocol lists

proc client ::  | => Put([Char] | TopBot), StringTerminal =
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

proc server :: | PList( | Put([Char] | TopBot) ), Console => =
    | chs, console => -> do
        hcase chs of 
            PListEmpty -> do 
                close chs
                hput ConsoleClose on console
                halt console

            PListCons -> do
                split chs into ch, new_chs
                hput ConsoleStringTerminal on console
                split console into new_console, neg_term
                plug
                    p_listrace( | ch, new_chs => ret_winner, ret_losers )
                    neg_term, term =>  -> neg_term |=| neg term
                    ret_winner => term -> do
                        get fruit on ret_winner
                        close ret_winner 
                        hput StringTerminalPut on term
                        put fruit on term 
                        hput StringTerminalGet on term
                        get _ on term 
                        hput StringTerminalClose on term
                        halt term 
                    server( | ret_losers, new_console =>  )

proc client_wrapper :: | Put(A|TopBot), Put(A|TopBot), Put(A|TopBot), PList( | Put(A|TopBot)) => PList( | Put(A|TopBot)) = 
    | ch1, ch2, ch3, nil => out_plist ->
        do 
            plug
                plist_cons( | ch3, nil => temp1) 
                plist_cons( | ch2, temp1 => temp2)
                plist_cons( | ch1, temp2 => out_plist)

proc run :: | Console => StringTerminal, StringTerminal, StringTerminal =
    | console => term1, term2, term3 -> plug
        client( | => ch1, term1 )
        client( | => ch2, term2 )
        client( | => ch3, term3 )
        client_wrapper( | ch1, ch2, ch3, nil => out_plist  )
        plist_nil( | => nil)
        server( | out_plist, console => )