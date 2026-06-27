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

-- protocol for an arbitrarily large binary tree with a channel stored at each node
protocol PTree( | M) => S =
    PTreeLeaf :: TopBot => S
    PTreeNode :: M (*) (S (*) S) => S

proc ptree_leaf :: | => PTree( | M) =
    | => nil -> do 
        hput PTreeLeaf on nil
        halt nil

proc ptree_node :: | M, PTree( | M), PTree( | M) => PTree( | M) =
    | ch, tree_1, tree_2 => tree_out -> do
        on tree_out do hput PTreeNode
        fork tree_out as
            ch_out with ch -> ch_out |=| ch
            trees_out with tree_1, tree_2 -> do
                fork trees_out as
                    tree_1_out with tree_1 -> tree_1_out |=| tree_1
                    tree_2_out with tree_2 -> tree_2_out |=| tree_2

-- given a new channel and a single tree, adds the channel to the tree
-- this will recursively add the channel to the left subtree, and then swap which one is left and which one is right
-- which will keep the tree balanced, but not sorted.
proc ptree_add :: | M, PTree( | M) => PTree( | M) =
    | ch, tree_in => tree_out -> do
        hcase tree_in of
            PTreeLeaf -> do         -- there is not already a channel here!!
                close tree_in
                plug
                    ptree_leaf( | => tree_1)
                    ptree_leaf( | => tree_2)
                    ptree_node( | ch, tree_1, tree_2 => tree_out)
            PTreeNode -> do         -- there is already a channel here...
                split tree_in into tree_ch, trees
                split trees into tree_1, tree_2
                plug
                    ptree_add( | ch, tree_1 => tree_1_out)      -- recursively call add on tree_1, 
                    ptree_node( | tree_ch, tree_2, tree_1_out => tree_out)  -- and swap tree_1 and tree_2 to keep balance


-- higher-order fold process that will "take in" a PTree and two stored processes: a node_proc and a leaf_proc,
-- connect the channels in the PTree to these processes as suggested by the processes' names  
-- and "output" a channel connected to the outermost node_proc
-- in our case, we will give it a stored racer_tree_client_node process and nil_node process code
proc ptree_h_o_fold :: Store( | X, Y, Y => Y), Store( | => Y) | PTree( | X) => Y =
    node_proc, leaf_proc | ch_tree => ret_ch -> do
        hcase ch_tree of
            PTreeLeaf -> do                -- ch_tree now TopBot
                close ch_tree
                use(leaf_proc)( | => ret_ch)
            PTreeNode -> do                 -- ch_tree now (*) (*)
                split ch_tree into ch, trees  -- peel one off
                split trees into tree_1, tree_2
                plug
                    ptree_h_o_fold(node_proc, leaf_proc | tree_1 => folded_tree_1) -- recurse on the left
                    ptree_h_o_fold(node_proc, leaf_proc | tree_2 => folded_tree_2) -- recurse on the right
                    use(node_proc)( | ch, folded_tree_1, folded_tree_2 => ret_ch)  -- replace node with node_proc


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

-- racer linked list nodes (will be used by racer tree nodes):

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


-- racer tree nodes:

-- this is the case when the client at this node has closed
-- this is basically the server with two clients from rec-non-det-server
-- assumes that tree_1 has won a race
proc racer_tree_node :: | Put(FwdMsg | Link), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
    | tree_1, tree_2 => root -> do
        get rec on tree_1                   -- tree_1 now Link
        case rec of			
            Msg(msg) -> do
                put Msg(msg) on root        -- root now Link
                hcase root of
                    Fwd -> do               -- root now Put(FwdMsg | Link)
                        hput Fwd on tree_1  -- tree_1 now Put(FwdMsg | Link)
                        race                -- in the recursive case, maintain that tree in position 1 is the winner
                            tree_1 -> racer_tree_node( | tree_1, tree_2 => root)
                            tree_2 -> racer_tree_node( | tree_2, tree_1 => root)
                    Nil -> do               -- i guess the server could just close us :,((( anyway outbox now TopBot
                        close root
                        hput Nil on tree_1
                        close tree_1
                        get _ on tree_2
                        hput Nil on tree_2
                        halt tree_2
            Nil -> do                   -- rest of tree_1 is nil_node, so just connect tree_2 to root directly
                hput Nil on tree_1      -- if we reset to Fwd, we create a livelock :,(
                close tree_1
                tree_2 |=| root

-- this is the case when root closes the channel and we have two trees, 
-- assumes that tree_1 won the race to close them in order
proc racer_tree_close :: | Put(FwdMsg | Link), Put(FwdMsg | Link) => =
    | tree_1, tree_2 => -> do
        get _ on tree_1
        hput Nil on tree_1
        close tree_1
        get _ on tree_2
        hput Nil on tree_2
        halt tree_2

-- this is the case when root closes channel and we have an open client and tree, 
-- so we race them and closes them in order
proc racer_tree_client_close :: | Put(NewMsg | RecvMsgs), Put(FwdMsg | Link) => =
    | client, tree => -> race
        client -> do
            get _ on client
            hput Close on client
            close client
            get _ on tree
            hput Nil on tree
            halt tree
        tree -> do
            get _ on tree
            hput Nil on tree
            close tree
            get _ on client
            hput Close on client
            halt client

-- this is the process that we originally place at each node in the tree
defn
    -- tree node with a client and two sub trees that are all raced
    proc racer_tree_client_node :: | Put(NewMsg | RecvMsgs), Put(FwdMsg | Link), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
        | client, tree_1, tree_2 => root -> race
            client -> do
                get rec on client					-- client now RecvMsgs
                case rec of							
                    Yes(msg) -> do
                        put Msg(Yes(msg)) on root   -- root now Link
                        hcase root of
                            Fwd -> do               -- root now Put(FwdMsg | Link)
                                hput Recv on client -- client now Put(NewMsg | RecvMsgs)
                                racer_tree_client_node( | client, tree_1, tree_2 => root)
                            Nil -> do               -- i guess the server could just close us :,((( anyway outbox now TopBot
                                close root
                                hput Close on client
                                close client
                                race
                                    tree_1 -> racer_tree_close( | tree_1, tree_2 => )
                                    tree_2 -> racer_tree_close( | tree_2, tree_1 => )
                    End(cid) -> do      
                        hput Close on client
                        close client
                        put Msg(End(cid)) on root      -- root now Link
                        hcase root of                  -- after client leaves, this is basically the rec-non-det-server with two clients
                            Fwd -> race                -- root now Put(FwdMsg | Link)
                                tree_1 -> racer_tree_node( | tree_1, tree_2 => root)
                                tree_2 -> racer_tree_node( | tree_2, tree_1 => root)
                            Nil -> do
                                close root
                                race
                                    tree_1 -> racer_tree_close( | tree_1, tree_2 => )
                                    tree_2 -> racer_tree_close( | tree_2, tree_1 => )
            tree_1 -> racer_tree_1_winner( | client, tree_1, tree_2 => root)
            tree_2 -> racer_tree_1_winner( | client, tree_2, tree_1 => root)

    -- helper process to minimize duplicated code 
    -- assumes that the proc at tree_1 has won a race
    proc racer_tree_1_winner :: | Put(NewMsg | RecvMsgs), Put(FwdMsg | Link), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
        | client, tree_1, tree_2 => root -> do
            get rec on tree_1                           -- tree_1 now Link
            case rec of			
                Msg(msg) -> do
                    put Msg(msg) on root            -- root now Link
                    hcase root of
                        Fwd -> do                   -- root now Put(FwdMsg | Link)
                            hput Fwd on tree_1      -- tree_1 now Put(FwdMsg | Link)
                            racer_tree_client_node( | client, tree_1, tree_2 => root)
                        Nil -> do                   -- i guess the server could just close us :,((( anyway outbox now TopBot
                            close root
                            hput Nil on tree_1
                            close tree_1
                            racer_tree_client_close( | client, tree_2 => )
                Nil -> do                       -- tree_1 is gone, so become racer node with client and tree_2
                    hput Nil on tree_1          -- if we reset to Fwd, we create a livelock :,(
                    close tree_1
                    racer_node( | client, tree_2 => root)


-- generate a tree of clients, use h o fold to turn tree into a racer tree, call server with root node
-- the higher order fold will change the tree of RecvMsgs chs into a single channel with
-- the "Link" protocol. basically a recv msgs protocol but with a different datatype
-- it's used by racer tree processes: racer_tree_client_node, racer_tree_node, ... racer_node, forward_node, nil_node
-- if a client drops off, its node will turn into a racer_tree_node which is like the two-channel-non-det server sending up the tree
    -- if one of the subtrees then drops off, it will identify the other subtree with the root channel
-- if one of its subtrees is nil, it will turn into a racer_node (from the linked list version) for the client and other tree
-- anyway the fold will construct the racer tree and "return" a channel connected to the root of the tree
proc gen_clients_tree :: Int | PTree( | Put(NewMsg | RecvMsgs) ), Console  => = 
    cid | ptree, console => -> do
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
                    -- then we race clients using a h_o_fold to turn the list into a racer TREE??
                    ptree_h_o_fold( store(racer_tree_client_node), store(nil_node) | ptree => raced_ptree)            
                    server( | raced_ptree, console => )
            else do        
                hput ConsoleStringTerminal on console
                split console into new_console, neg_term
                plug
                    neg_term, term => -> neg_term |=| neg term
                    client_wrapper("Client " ++ intToString(cid) | => ch, term)
                    ptree_add( | ch, ptree => new_ptree)
                    gen_clients_tree( cid + 1 | new_ptree, new_console => )

-- makes a non-deterministic tree? server in two phases. 
-- first phase generates arbitrarily many clients using the console and adds the channels to a tree
-- in the second, the server recursively prints winning messages as they are forwarded up the racer tree
proc run :: | Console => =       
    | console => -> plug  
        ptree_leaf( | => nil)
        -- this will generate a binary tree of clients, 
        -- use a h o fold to turn it into a racer tree,
        -- and finally call the server with the root of the tree
        gen_clients_tree( 1 | nil, console => )          

