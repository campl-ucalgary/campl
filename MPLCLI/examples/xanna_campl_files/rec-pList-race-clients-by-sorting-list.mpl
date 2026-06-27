include Prelude (intToString, isEmpty | )

-- sequential type that we are using like a handle
data NewMsg -> C =
	Yes :: [Char] -> C
	End :: [Char] -> C 

-- recursive channel type for passing an arbitrary number of messages
-- process sending handles is receiving messages
coprotocol S => RecvMsgs =
    Recv :: S => Put(NewMsg | S)
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
-- and puts them back by race consing them? maybe?
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


---- Testing protocol lists

-- client code (recursive, cannot race on handles)
defn
    proc client_wrapper :: [Char] | => Put(NewMsg | RecvMsgs), StringTerminal =
        cid | => ch, terminal -> do
            on terminal do
                hput StringTerminalPut
                put "Hello " ++ cid ++ "!"
            client(cid | => ch, terminal)
    proc client :: [Char] | => Put(NewMsg | RecvMsgs), StringTerminal =
        cid | => ch, terminal -> do
            on terminal do
                hput StringTerminalPut
                put "Enter message in terminal. Press ENTER to close."
                hput StringTerminalGet
                get msg
            if isEmpty(msg)
                then do
                    put End(cid) on ch
                    client_recurse(cid | => ch, terminal)
                else do
                    put Yes(cid ++ ": " ++ msg) on ch
                    client_recurse(cid | => ch, terminal)
    proc client_recurse :: [Char] | => RecvMsgs, StringTerminal =
        cid | => ch, terminal -> do
            hcase ch of
                Recv -> do
                    client(cid | => ch, terminal)
                Close -> do
                    close ch
                    on terminal do
                        hput StringTerminalClose
                        halt


-- server code
proc server :: | Put(NewMsg | RecvMsgs), PList( | Put(NewMsg | RecvMsgs) ), Console => =  
    | winner, loser_chs, console => -> do
        get rec on winner					-- winner now RecvMsgs
        case rec of							
            Yes(msg) -> do
                hput ConsolePut on console
                put msg on console
                hput Recv on winner			-- winner now Put(NewMsg | RecvMsgs)
                plug
                    plist_cons_race( | winner, loser_chs => raced_plist)
                    raced_plist, console => -> do
                        hcase raced_plist of
                            PListEmpty -> do        -- i don't think this will happen because we just consed winner on,,,
                                close raced_plist
                                hput ConsolePut on console
                                put "I don't know what happened but something is wrong. See line 123." on console
                                hput ConsoleClose on console
                                halt console                            
                            PListCons -> do                     -- the next ch in the list should be the next winner
                                split raced_plist into new_winner, new_loser_chs
                                server( | new_winner, new_loser_chs, console => )
            End(wid) -> do
                hput Close on winner
                close winner
                hput ConsolePut on console
                put wid ++ " ended session." on console
                hcase loser_chs of 
                    PListEmpty -> do 
                        close loser_chs
                        hput ConsolePut on console
                        put "All clients have finished." on console
                        hput ConsoleClose on console
                        halt console
                    PListCons -> do                     -- the next ch in the list should be the next winner
                        split loser_chs into new_winner, new_loser_chs  
                        server( | new_winner, new_loser_chs, console => )


proc gen_clients :: Int | PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
    cid | plist, console => -> do       -- channel connected to the next one
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
                hcase plist of
                    PListEmpty -> do
                        close plist
                        hput ConsolePut on console
                        put "Please generate at least one client to connect to the server." on console
                        hput ConsoleClose on console
                        halt console                            
                    PListCons -> do                     -- the first ch in the list should be the winner
                        split plist into winner, loser_chs
                        server( | winner, loser_chs, console => )
            else do        
                hput ConsoleStringTerminal on console
                split console into new_console, neg_term
                plug
                    neg_term, term => -> neg_term |=| neg term
                    client_wrapper("Client " ++ intToString(cid) | => ch, term)
                    -- node
                    plist_cons_race( | ch, plist => new_chs)                  -- racing channels as we go works
                    gen_clients( cid + 1 | new_chs, new_console => )


-- this does not seem to work...

proc race_PList :: | PList( | Put(A|S)) => PList( | Put(A|S)) =
    | chs => raced_chs -> do 
        hcase chs of
            PListEmpty -> do
                close chs
                plug
                    plist_nil( | => new_chs)
                    new_chs => raced_chs -> new_chs |=| raced_chs
            PListCons -> do
                split chs into ch, new_chs              -- i think it gets stuck here, and we can't plug because both ch and new_chs need to go into the same rec call
                plist_cons_race( | ch, new_chs => raced_chs)

-- generating the clients without racing and then using this does not seem to work

-- proc race_clients :: | PList( | Put(NewMsg | RecvMsgs) ), Console => =
--     | plist, console => ->
--         plug
--             race_PList( | plist => raced_plist)                 -- this gets stuck at the split
--             raced_plist, console => -> do
--                 -- need hcase to fix this, but i don't think it works anyway
--                 split raced_plist into winner, loser_chs
--                 server( | winner, loser_chs, console => )

-- proc gen_clients_no_race :: Int | PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
--     cid | plist, console => -> do
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
--                 race_clients( | plist, console => )
--             else do        
--                 hput ConsoleStringTerminal on console
--                 split console into new_console, neg_term
--                 plug
--                     neg_term, term => -> neg_term |=| neg term
--                     client("Client " ++ intToString(cid) | => ch, term)
--                     plist_cons( | ch, plist => new_chs)
--                     gen_clients_no_race( cid + 1 | new_chs, new_console => )


-- make list server with two phases. 
-- first phase generates arbitrarily many clients using the console and puts the channels in a list (so far it seems like we need to race as we go :,(
-- in the second, the server prints the winner's message and recursively re-races the list of channels until all clients close

proc run :: | Console => =       
    | console => -> plug  
        plist_nil( | => nil)
        -- after this generates the clients and races as it goes, it will call the server
        gen_clients( 1 | nil, console => )          

