include Prelude (intToString, isEmpty | )

-- sequential type that we are using like a handle
data NewMsg -> C =
	Yes :: [Char] -> C
	End :: [Char] -> C 

-- used by "linked list" processes
data FwdMsg -> C =
    Msg :: NewMsg -> C
    Nil :: -> C

-- recursive channel type for passing an arbitrary number of messages
-- process sending handles is receiving messages
coprotocol S => RecvMsgs =
    Recv :: S => Put(NewMsg | S)
    Close :: S => TopBot

-- used by "linked list" processes
coprotocol S => Link =
    Fwd :: S => Put(FwdMsg | S)
    Nil :: S => TopBot

-- protocol for a list of arbitrarily many channels
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

-- higher-order fold process that will "take in" a PList and two stored processes: a cons_proc and a nil_proc,
-- connect the channels in the PList to these processes as suggested by the processes' names  
-- and "output" a channel connected to the outermost cons_proc
-- in our case, we will give it the stored racer process and nil process code
proc plist_h_o_fold :: Store( | X, Y => Y), Store( | => Y) | PList( | X) => Y =
    cons_proc, nil_proc | chs => ret_ch -> do
        hcase chs of
            PListEmpty -> do                -- chs now TopBot
                close chs
                use(nil_proc)( | => ret_ch)
            PListCons -> do                 -- chs now (*)
                split chs into ch, new_chs  -- peel one off
                plug
                    plist_h_o_fold(cons_proc, nil_proc | new_chs => folded_new_chs) -- recurse on the rest
                    use(cons_proc)( | ch, folded_new_chs => ret_ch)             -- replace cons with cons_proc


-- -- previous idea does not seem to work... it's kind of like a map instead of a fold?
-- note that this code needs plist_cons_race which i have since deleted
-- proc race_PList :: | PList( | Put(A|S)) => PList( | Put(A|S)) =
--     | chs => raced_chs -> do 
--         hcase chs of
--             PListEmpty -> do
--                 close chs
--                 plug
--                     plist_nil( | => new_chs)
--                     new_chs => raced_chs -> new_chs |=| raced_chs
--             PListCons -> do
--                 split chs into ch, new_chs              -- i think it gets stuck here, and we can't plug because both ch and new_chs need to go into the same rec call
--                 plist_cons_race( | ch, new_chs => raced_chs)

-- proc equate_2 :: | M, P => M, P =
--     | in1, in2 => out1, out2 -> plug 
--         in1 => dummy, out1 -> do
--             on dummy do close
--             in1 |=| out1
--         in2, dummy => out2 -> do
--             on dummy do close
--             in2 |=| out2


---- Testing folded protocol list race

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
proc server :: | Put(FwdMsg | Link), Console => =  
    | clients, console => -> do
        get rec on clients					-- clients now Link
        case rec of
            Msg(client_msg) -> do
                hput Fwd on clients			-- clients now Put(FwdMsg | Link)
                case client_msg of							
                    Yes(msg) -> do
                        hput ConsolePut on console
                        put msg on console
                        server( | clients, console => )
                    End(cid) -> do
                        hput ConsolePut on console
                        put cid ++ " ended session." on console                        
                        server( | clients, console => )
            Nil -> do
                hput Nil on clients             -- again if we set to Fwd, will create a livelock
                close clients                
                hput ConsolePut on console
                put "All clients have finished." on console
                hput ConsoleClose on console
                halt console

-- racer linked list nodes:

-- nil_node sends Nil until channel is set to Nil then halt
-- warning: this can cause livelock if it is not interacted with properly.
proc nil_node :: | => Put(FwdMsg | Link) =
    | => outbox -> do
        put Nil on outbox           -- outbox now Link
        hcase outbox of
            Fwd -> do                       -- outbox now Put(FwdMsg | Link)
                nil_node( | => outbox)
            Nil ->                          -- outbox now TopBot
                halt outbox

-- forward_node forwards messages from client until it closes, then will become nil_node
proc forward_node :: | Put(NewMsg | RecvMsgs) => Put(FwdMsg | Link) =
    | client => outbox -> do
        get rec on client					-- client now RecvMsgs
        case rec of							
            Yes(msg) -> do
                put Msg(Yes(msg)) on outbox           -- outbox now Link
                hcase outbox of
                    Fwd -> do               -- outbox now Put(FwdMsg | Link)
                        hput Recv on client         	-- client now Put(NewMsg | RecvMsgs)
                        forward_node( | client => outbox)
                    Nil -> do              -- i guess the server could just close us :,((( anyway outbox now TopBot
                        close outbox
                        hput Close on client
                        halt client
            End(cid) -> do
                hput Close on client
                close client
                put Msg(End(cid)) on outbox         -- outbox now Link
                hcase outbox of
                    Fwd -> do                       -- outbox now Put(FwdMsg | Link)
                        nil_node( | => outbox)
                    Nil ->                          -- outbox now TopBot
                        halt outbox

-- racer_node races two channels and forwards messages from both
-- client ch is connected to client, inbox is connected to rest of list, and outbox is eventually connected to server
-- it cases on the outbox ch to see if server has closed it or not
-- it receives NewMsg from its client on a RecvMsgs ch
-- racer_nodes are connected to each other and the server with Link channels
-- they pass FwdMsg types which contain a NewMsg or are Nil 
-- this differentiates a client End message to be forwarded from a Nil message from the actual end of the chain.
-- if racer_node becomes the end of the chain, it becomes a forwarding node for its client
-- if the client drops off then it only needs to forward msgs up the chain, so it just identifies the channels.
proc racer_node :: | Put(NewMsg | RecvMsgs), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
    | client, inbox => outbox -> race
        client -> do
            get rec on client					-- client now RecvMsgs
            case rec of							
                Yes(msg) -> do
                    put Msg(Yes(msg)) on outbox           -- outbox now Link
                    hcase outbox of
                        Fwd -> do               -- outbox now Put(FwdMsg | Link)
                            hput Recv on client         	-- client now Put(NewMsg | RecvMsgs)
                            racer_node( | client, inbox => outbox)
                        Nil -> do               -- i guess the server could just close us :,((( anyway outbox now TopBot
                            close outbox
                            hput Close on client
                            close client
                            get _ on inbox
                            hput Nil on inbox
                            halt inbox
                End(cid) -> do
                    hput Close on client
                    close client
                    put Msg(End(cid)) on outbox           -- outbox now Link
                    hcase outbox of
                        Fwd -> do               -- outbox now Put(FwdMsg | Link)
                            inbox |=| outbox
                        Nil -> do               -- i guess the server could just close us :,((( anyway outbox now TopBot
                            close outbox
                            get _ on inbox
                            hput Nil on inbox
                            halt inbox
        inbox -> do
            get rec on inbox                    -- inbox now Link
            case rec of			
                Msg(msg) -> do
                    put Msg(msg) on outbox           -- outbox now Link
                    hcase outbox of
                        Fwd -> do               -- outbox now Put(FwdMsg | Link)
                            hput Fwd on inbox         	-- inbox now Put(FwdMsg | Link)
                            racer_node( | client, inbox => outbox)
                        Nil -> do               -- i guess the server could just close us :,((( anyway outbox now TopBot
                            close outbox
                            hput Nil on inbox
                            close inbox
                            get _ on client
                            hput Close on client
                            halt client
                Nil -> do                   -- rest of chain is nil_node, so put it out of its misery by setting ch to Nil
                    hput Nil on inbox       -- if we reset to Fwd, we create a livelock :,(
                    close inbox
                    -- forward_node forwards messages from client and when it closes, becomes nil_node
                    forward_node( | client => outbox)


-- generate a list of clients, use h o fold to turn list into a racer linked list, call server with start of linked list
-- the higher order fold will change the list of RecvMsgs chs into a single channel with
-- the "linked list" protocol. basically a recv msgs protocol but with a different datatype
-- it's used by linked list node processes: racer, forward, nil
-- if a client drops off, its racer node will id the other channels - unless the rest of the list is nil then becomes nil_node
-- this is based on the idea of constant time deletion in a doubley linked list by identifying forward/backwards pointers
-- anyway the fold will construct the racer linked list and "return" a channel connected to the start of the linked list
proc gen_clients_list :: Int | PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
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
                plug
                    -- then we race clients using a h_o_fold to turn the list into a racer linked list
                    plist_h_o_fold( store(racer_node), store(nil_node) | plist => raced_plist)            
                    server( | raced_plist, console => )
            else do        
                hput ConsoleStringTerminal on console
                split console into new_console, neg_term
                plug
                    neg_term, term => -> neg_term |=| neg term
                    client_wrapper("Client " ++ intToString(cid) | => ch, term)
                    plist_cons( | ch, plist => new_chs)
                    gen_clients_list( cid + 1 | new_chs, new_console => )


-- make a list server with two phases. 
-- first phase generates arbitrarily many clients using the console and puts the channels in a list
-- in the second, the server recursively prints winning messages as they are forwarded up the racer linked list chain

proc run :: | Console => =       
    | console => -> plug  
        plist_nil( | => nil)
        -- this will generate a list of clients, 
        -- use a h o fold to turn it into a racer linked list,
        -- and finally call the server with the start of the linked list
        gen_clients_list( 1 | nil, console => )          

