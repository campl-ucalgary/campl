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
                split chs into ch, new_chs                                  -- i think this doesn't work because we can't race split.
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


proc race_clients :: | PList( | Put([Char] | TopBot) ), Console => =
    | plist, console => ->
        plug
            race_PList( | plist => raced_plist)                 -- racing a list of channels does not seem to work...
            server( | raced_plist, console => )

-- proc client_wrapper :: Int | PList( | Put([Char]|TopBot)), Console  => = 
--     cid | in_plist, console => -> do
--         on console do
--             hput ConsolePut
--             put "Enter message to create new terminal. Press ENTER to finish terminal generation phase."
--             hput ConsoleGet
--             get msg
--         if isEmpty(msg)
--             then do
--                 on console do 
--                     hput ConsolePut
--                     put "Terminal generation phase complete."
--                 server( | in_plist, console => )
--             else do        
--                 hput ConsoleStringTerminal on console
--                 split console into new_console, neg_term
--                 plug
--                     neg_term, term => -> neg_term |=| neg term
--                     client("Client " ++ intToString(cid) | => ch, term)
--                     plist_cons_race( | ch, in_plist => new_chs)
--                     client_wrapper( cid + 1 | new_chs, new_console => )

proc gen_clients_no_race :: Int | PList( | Put([Char]|TopBot)), Console => = 
    cid | plist, console => -> do
        on console do
            hput ConsolePut
            put "Enter message to create new terminal. Press ENTER to finish terminal generation phase."
            hput ConsoleGet
            get msg
        if isEmpty(msg)
            then do
                on console do 
                    hput ConsolePut
                    put "Terminal generation phase complete."
                race_clients( | plist, console => )
            else do        
                hput ConsoleStringTerminal on console
                split console into new_console, neg_term
                plug
                    neg_term, term => -> neg_term |=| neg term
                    client("Client " ++ intToString(cid) | => ch, term)
                    plist_cons( | ch, plist => new_chs)
                    gen_clients_no_race( cid + 1 | new_chs, new_console => )


proc run :: | Console => =       
    | console => -> plug  
        plist_nil( | => nil)
        gen_clients_no_race( 1 | nil, console => )



            -- make list server with two phases. 
            -- one where the console is used to generate arbitrarily many windows and put the channels in a list (not raced?)
            -- in the second, the server races the list of channels