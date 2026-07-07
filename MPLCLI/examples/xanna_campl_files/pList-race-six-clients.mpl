include Prelude (intToString, isEmpty | )

data NewTerm -> C =
	Yes :: -> C
	End :: -> C 

coprotocol S => NewTerms =
    Recv :: S => Put(NewTerm | S)
    Close :: S => TopBot

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


-- we race processes as we add them to the list
-- then the server only needs to peel them off and get the messages from them in order
-- thank you karen little for this wonderful suggestion!!!!!!!!!!
proc plist_cons_race :: | Put(A|S), PList( | Put(A|S)) => PList( | Put(A|S)) =
    | ch1, chs => raced_chs -> do
        hcase chs of
            PListEmpty -> do                -- these are the losers because otherwise they would have won the race at a higher level and not been sent down?
                close chs
                plug
                    plist_nil( | => nil)
                    plist_cons( | ch1, nil => raced_chs)
            PListCons -> do                
                -- split chs into ch2, new_chs             -- we can't race splitting channels :,(( or we would be done.
                plug                                       -- so instead, we will split and race concurrently?!
                    chs => ch2, new_chs -> do
                        split chs into ch2_temp, new_chs_temp
                        equate_2( | ch2_temp, new_chs_temp => ch2, new_chs)
                    ch1, ch2 => winner, loser ->
                        race
                            ch1 -> equate_2( | ch1, ch2 => winner, loser)
                            ch2 -> equate_2( | ch2, ch1 => winner, loser)
                    winner, loser_chs => raced_chs -> 
                        plist_cons( | winner, loser_chs => raced_chs)       -- winning process is on the outside
                    loser, new_chs => loser_chs -> 
                        plist_cons_race( | loser, new_chs => loser_chs)     -- recursively race losing processes

                -- split chs into ch2, new_chs             -- we can't race splitting channels :,(( or we would be done.
                -- race                                
                --     ch1 -> plug
                --         plist_cons( | ch1, ch2_chs => raced_chs)
                --         plist_cons_race( | ch2, new_chs => ch2_chs)    -- recursively race losing processes
                --     ch2 -> plug
                --         plist_cons( | ch2, ch1_chs => raced_chs)
                --         plist_cons_race( | ch1, new_chs => ch1_chs)    -- recursively race losing processes


proc race_PList :: | PList( | Put(A|S)) => PList( | Put(A|S)) =
    | chs => raced_chs -> do 
        hcase chs of
            PListEmpty -> do
                close chs
                plug
                    plist_nil( | => new_chs)
                    new_chs => raced_chs -> new_chs |=| raced_chs
            PListCons -> do
                split chs into ch, new_chs
                plist_cons_race( | ch, new_chs => raced_chs)


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



-- maybe need a proc that handles the hcasing since we can't race on it? this would be easy if we could race on the hcase...
-- it has an output channel that's just the put type so we can race on it? otherwise we could do more shenanigans....

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

proc client_wrapper =
    -- :: | Put(A|TopBot), Put(A|TopBot), Put(A|TopBot), PList( | Put(A|TopBot)) => PList( | Put(A|TopBot)) = 
    | ch1, ch2, ch3, ch4, ch5, ch6, nil => out_plist -> do
        plug
            plist_cons_race( | ch1, temp_out_plist_5 => out_plist)
            plist_cons_race( | ch6, nil => temp_out_plist_1) 
            plist_cons_race( | ch5, temp_out_plist_1 => temp_out_plist_2)
            plist_cons_race( | ch4, temp_out_plist_2 => temp_out_plist_3)
            plist_cons_race( | ch3, temp_out_plist_3 => temp_out_plist_4)
            plist_cons_race( | ch2, temp_out_plist_4 => temp_out_plist_5)
            -- race_PList( | temp_out_plist_5 => out_plist)

proc run :: | Console => StringTerminal, StringTerminal, StringTerminal, StringTerminal, StringTerminal, StringTerminal =       
    | console => term1, term2, term3, term4, term5, term6 -> plug     
        client("Client 1" | => ch1, term1)
        client("Client 2" | => ch2, term2)
        client("Client 3" | => ch3, term3)
        client("Client 4" | => ch4, term4)
        client("Client 5" | => ch5, term5)
        client("Client 6" | => ch6, term6)
        client_wrapper( | ch1, ch2, ch3, ch4, ch5, ch6, nil => chs)
        plist_nil( | => nil)
        server( | chs, console => )



            -- make list server with two phases. 
            -- one where the console is used to generate arbitrarily many windows and put the channels in a list (not raced?)
            -- in the second, the server races the list of channels