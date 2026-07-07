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

-- we race processes as we add them to the list
-- then the server only needs to peel them off and get the messages from them in order
-- thank you karen little for this wonderful suggestion!!!!!!!!!!
proc plist_cons_race :: | Put(A|S), PList( | Put(A|S)) => PList( | Put(A|S)) =
    | ch, chs => ret_chs -> do
        hcase chs of
            PListEmpty -> do
                close chs
                plug
                    plist_nil( | => new_chs)
                    plist_cons( | ch, new_chs => ret_chs)
            PListCons -> do
                split chs into ch2, new_chs
                race
                    ch -> plug
                        plist_cons( | ch, temp_ret_chs => ret_chs)
                        plist_cons_race( | ch2, new_chs => temp_ret_chs)    -- recursively race losing processes
                    ch2 -> plug
                        plist_cons( | ch2, temp_ret_chs => ret_chs)
                        plist_cons_race( | ch, new_chs => temp_ret_chs)

proc equate_2 :: | M, P => M, P =
    | in1, in2 => out1, out2 -> plug 
        in1 => dummy, out1 -> do
            on dummy do close
            in1 |=| out1
        in2, dummy => out2 -> do
            on dummy do close
            in2 |=| out2


---- Testing protocol lists

proc client :: [Char] | => Put([Char] | TopBot), StringTerminal =
    cid | => ch, term -> do
        hput StringTerminalPut on term
        put cid on term
        hput StringTerminalGet on term
        get msg on term                         -- Reading input from the user

        put cid ++ ": " ++ msg on ch            -- send message to server

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
                get msg on ch
                close ch 
                hput ConsolePut on console
                put msg on console 
                server( | new_chs, console => )

proc client_wrapper :: | Put(A|TopBot), Put(A|TopBot), Put(A|TopBot), PList( | Put(A|TopBot)) => PList( | Put(A|TopBot)) = 
    | ch1, ch2, ch3, nil => out_plist -> do
        plug
            plist_cons_race( | ch3, nil => temp_out_plist_1) 
            plist_cons_race( | ch2, temp_out_plist_1 => temp_out_plist_2)
            plist_cons_race( | ch1, temp_out_plist_2 => out_plist)

proc run :: | Console => StringTerminal, StringTerminal, StringTerminal =
    | console => term1, term2, term3 -> plug
        client( "Client 1" | => ch1, term1 )
        client( "Client 2" | => ch2, term2 )
        client( "Client 3" | => ch3, term3 )
        client_wrapper( | ch1, ch2, ch3, nil => out_plist  )
        plist_nil( | => nil)
        server( | out_plist, console => )