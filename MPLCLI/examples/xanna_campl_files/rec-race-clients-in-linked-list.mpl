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

---- Testing linked list race

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

-- forward_node. will forward messages from client until it closes, then will become nil_node
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
-- one ch is connected to client and the other connected to rest of list
-- it also cases on the outbox ch to see if server has closed it or not
-- it receives NewMsg from its client on a RecvMsgs ch
-- racer_nodes are connected to each other and the server with Link channels
-- they pass FwdMsg types which contain a NewMsg or are Nil 
-- this differentiates a client End msg to be forwarded vs the actual end of the chain.
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

-- changed channel type to "linked list" protocol. basically a recv msgs protocol but with a different datatype
-- it's used by linked list node processes: racer, forward, nil
-- if a clients drops off, ids channels - unless the rest of the list is nil then becomes nil_node
proc gen_clients :: Int | Put(FwdMsg | Link), Console  => = 
    cid | chs, console => -> do       -- need to hold on to the channel connected to the rest of the list
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
                server( | chs, console => )
            else do        
                hput ConsoleStringTerminal on console
                split console into new_console, neg_term
                plug
                    neg_term, term => -> neg_term |=| neg term
                    client_wrapper("Client " ++ intToString(cid) | => ch, term)
                    racer_node( | ch, chs => new_chs)               -- racer node will race client and rest of clients
                    gen_clients( cid + 1 | new_chs, new_console => )

-- make linked list server with two phases. 
-- first phase generates arbitrarily many clients using the console and constructs linked list 
-- in the second, the server recursively prints the winning messages as they are forwarded up the linked list chain of races
proc run :: | Console => =       
    | console => -> plug  
        nil_node( | => nil)        -- nil_node that sends Nil until the channel is set to Nil and then halts
        -- after this generates the clients and constructs linked list, it will call the server
        gen_clients( 1 | nil, console => )          

