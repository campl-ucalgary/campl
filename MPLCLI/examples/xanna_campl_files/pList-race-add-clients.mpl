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


proc server_base_case :: | PList( | Put([Char] | TopBot) ), Console => =
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
                server_base_case( | new_chs, console => )

proc client_master :: | => StringTerminal, NewTerms =
    | => terminal, new_term -> do
        hcase new_term of
            Recv -> do
                on terminal do
                    hput StringTerminalPut
                    put "Enter message to create new terminal. Press ENTER to close."
                    hput StringTerminalGet
                    get msg
                if isEmpty(msg)
                    then do
                        put End on new_term
                        client_master( | => terminal, new_term)
                    else do
                        put Yes on new_term
                        client_master( | => terminal, new_term)
            Close -> do
                close new_term
                on terminal do
                    hput StringTerminalClose
                    halt


-- maybe need a proc that handles the hcasing since we can't race on it? this would be easy if we could race on the hcase...
-- it has an output channel that's just the put type so we can race on it? otherwise we could do more shenanigans....

proc server :: Int | PList( | Put([Char] | TopBot) ), Console, Put(NewTerm|NewTerms) => =
    cid | chs, console, new_term => -> do
        hcase chs of 
            PListEmpty -> do 
                close chs
                get val on new_term
                case val of
                    Yes -> do
                        hput Recv on new_term
                        hput ConsoleStringTerminal on console
                        split console into new_console, neg_term
                        plug
                            neg_term, term => -> neg_term |=| neg term
                            client("Client " ++ intToString(cid) | => ch, term)
                            plist_nil( | => new_chs)
                            plist_cons( | ch, new_chs => ret_chs)
                            server( cid + 1 | ret_chs, new_console, new_term => )
                    End -> do
                        hput Close on new_term
                        close new_term
                        hput ConsoleClose on console
                        halt console

            PListCons -> do
                split chs into ch, new_chs
                race
                    ch -> do
                        get msg on ch
                        close ch
                        hput ConsolePut on console
                        put msg on console 
                        server(cid | new_chs, console, new_term => )
                    new_term -> do
                        get val on new_term
                        case val of
                            Yes -> do
                                hput Recv on new_term
                                hput ConsoleStringTerminal on console
                                split console into new_console, neg_term
                                plug
                                    neg_term, term => -> neg_term |=| neg term
                                    client("Client " ++ intToString(cid) | => new_ch, term)
                                    plist_cons( | new_ch, temp_ret_chs => ret_chs)
                                    plist_cons( | ch, new_chs => temp_ret_chs)
                                    race_PList( | ret_chs => raced_chs) 
                                    -- => -> race
                                    --     new_ch -> 

                                    --     ch ->

                                    --     new_term -> plug
                                    server(cid + 1 | raced_chs, new_console, new_term => )
                            End -> do
                                hput Close on new_term
                                close new_term
                                get msg on ch
                                close ch
                                hput ConsolePut on console
                                put msg on console 
                                server_base_case( | new_chs, console => )

proc client_wrapper =
    -- :: | Put(A|TopBot), Put(A|TopBot), Put(A|TopBot), PList( | Put(A|TopBot)) => PList( | Put(A|TopBot)) = 
    | ch1, ch2, ch3, ch4, ch5, nil => out_plist -> do
        plug
            plist_cons_race( | ch5, nil => temp_out_plist_1) 
            plist_cons_race( | ch4, temp_out_plist_1 => temp_out_plist_2)
            plist_cons_race( | ch3, temp_out_plist_2 => temp_out_plist_3)
            plist_cons_race( | ch2, temp_out_plist_3 => temp_out_plist_4)
            plist_cons_race( | ch1, temp_out_plist_4 => out_plist)
            -- race_PList( | temp_out_plist_5 => out_plist)

proc run :: | Console => StringTerminal, StringTerminal, StringTerminal, StringTerminal, StringTerminal, StringTerminal =       
    -- , StringTerminal, StringTerminal, StringTerminal
    | console => term1, term2, term3, term4, term5, master -> plug     
        client("Client 1" | => ch1, term1)
        client("Client 2" | => ch2, term2)
        client("Client 3" | => ch3, term3)
        client("Client 4" | => ch4, term4)
        client("Client 5" | => ch5, term5)
        client_wrapper( | ch1, ch2, ch3, ch4, ch5, nil => chs)
        plist_nil( | => nil)
        -- client("Client 1" | => ch, terminal)
        -- plist_cons( | ch, nil => chs)
        => master, new_term -> do
            hput StringTerminalPut on master
            put "Client Master" on master
            client_master( | => master, new_term)
        chs, console, new_term => -> do
            hput Recv on new_term
            server( 6 | chs, console, new_term => )



            -- make list server with two phases. one where the console is used to generate arbitrarily many windows 