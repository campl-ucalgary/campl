include Prelude (isEmpty, lg, intToString | )

-- sequential type that we are using like a handle
data NewMsg -> C =
	Yes :: [Char] -> C
	End :: [Char] -> C 

-- used by "linked list" processes
data FwdMsg -> C =
    Msg :: NewMsg, [Char] -> C
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


-- TODO decompose tree node into racer tree node and racer node.
-- also edit plist code to take in int and generate clients

-- to do check whether this is a balanced tree by adding path of client ids to messages as they go up the tree.
-- to do how to add another process to the tree
-- to do how to send a message through the tree from one client to another
-- to do how to pass a memory cell through the tree from one client to another.


-- want to separately label the nodes without the client needing to send its id before the node can send messages
-- okay we know that the client at the root node is 1, then one side has 2 and then all the even clients, 
-- and the other side has 3 and then all the odd clients. so if we call fold with 1 and then have an equation of what the 
    -- children client ids will be, then we can label the nodes i think? they might not actually match but at least we can 
    -- have a labelling that ensures each one will have a different id.

    -- node n has children n + floor(log2 n) + 1 and n + 2(floor(log2 n) + 1)
-- added function lg to prelude to take floor(log2 n) using only integer division lmao
-- so we will be able to assign some channel ids
-- if we give the fold the total number of clients is there a way for it to figure out which tree is which? so that the ids match?
    -- and then is there a way to recursively split that number up so that each subtree can also figure it out?
    -- like we give the fold both the id of the client at that node and some value of the highest client id either globally or locally?

-- TODO: update compiler to allow use of imported functions to compute values passed into use (line 105)

-- higher-order fold process that will "take in" a PTree and two stored processes: a node_proc and a leaf_proc,
-- connect the channels in the PTree to these processes as suggested by the processes' names  
-- and "output" a channel connected to the outermost node_proc
-- in our case, we will give it a stored racer_tree_client_node process and nil_node process code
defn
    proc ptree_h_o_fold :: Int, Store([Char] | X, Y, Y => Y), Store( | => Y) | PTree( | X) => Y =
        node_id, node_proc, leaf_proc | ch_tree => ret_ch -> do
            hcase ch_tree of
                PTreeLeaf -> do                -- ch_tree now TopBot
                    close ch_tree
                    use(leaf_proc)( | => ret_ch)
                PTreeNode -> do                 -- ch_tree now (*) (*)
                    split ch_tree into ch, trees  -- peel one off
                    split trees into tree_1, tree_2
                    plug
                        ptree_h_o_fold(node_id + lg(node_id) + 1, node_proc, leaf_proc | tree_1 => folded_tree_1) -- recurse on the left
                        ptree_h_o_fold(node_id + 2 * (lg(node_id) + 1), node_proc, leaf_proc | tree_2 => folded_tree_2) -- recurse on the right
                        use(node_proc)( id_str(node_id) | ch, folded_tree_1, folded_tree_2 => ret_ch)  -- replace node with node_proc
where 
    fun id_str :: Int -> [Char] =
        node_id -> intToString(node_id)

--
---- Testing folded protocol TREE!! race

-- client code

protocol SendMsgs => S =
	SendMsg :: Put([Char]|S) => S
	CloseCh :: TopBot => S

-- client code, uses SendMsgs protocol. should change to have acknowledgement too
proc client :: | => SendMsgs, StringTerminal =
	| => ch, terminal -> do
		on terminal do
			hput StringTerminalPut
			put "Enter message in terminal. Press ENTER to close."
			hput StringTerminalGet
			get msg
		if isEmpty(msg)
			then do
				on ch do
					hput CloseCh
					close
				on terminal do
					hput StringTerminalClose
					halt
			else do
				on ch do
					hput SendMsg
					put msg
				client( | => ch, terminal )

-- we connect the above client code to a client_node which hcases both server and client and mediates between them
defn    
    proc client_wrapper :: [Char] | => Put(NewMsg | RecvMsgs), StringTerminal =
            cid | => server_ch, terminal -> do
                on terminal do
                    hput StringTerminalPut
                    put "Hello " ++ cid ++ "!"
                plug
                    client( | => client_ch, terminal)               -- client_ch has type SendMsgs
                    client_node(cid | client_ch => server_ch)
    
    -- client node has two channels it hcases on
    -- one from the client and one from the server and it mediates between them, 
    -- on the client side, it hcases to keep receiving messages (will add functionality to ack that server is still open)
    -- on the server side, it passes on the messages from the client
    proc client_node :: [Char] | SendMsgs => Put(NewMsg | RecvMsgs) =
        cid | client_ch => server_ch -> do
            hcase client_ch of
                SendMsg -> do
                    get msg on client_ch
                    put Yes(cid ++ ": " ++ msg) on server_ch
                    hcase server_ch of 
                        Recv -> client_node(cid | client_ch => server_ch)
                        Close -> do
                            close server_ch
                            client_node_handle_server_close( | client_ch => )    -- this can be a more helpful proc in future
                CloseCh -> do
                    close client_ch
                    client_end_node(cid | => server_ch)

    -- with a SendMsgsWithAck protocol, we would just send negative ack
    proc client_node_handle_server_close :: | SendMsgs => =
        | client_ch => -> do
            hcase client_ch of
                SendMsg -> do
                    get _ on client_ch
                    client_node_handle_server_close( | client_ch => )
                CloseCh -> do
                    halt client_ch

    proc client_end_node :: [Char] | => Put(NewMsg | RecvMsgs) =
        cid | => server_ch -> do
            put End(cid) on server_ch
            hcase server_ch of
                Recv -> client_end_node(cid | => server_ch)      -- keep sending that the client ended until the channel closes
                Close -> do
                    halt server_ch

-- client code (does not use SendMsgs, hcases on ch to server)
-- defn
--     proc client_wrapper :: [Char] | => Put(NewMsg | RecvMsgs), StringTerminal =
--         cid | => ch, terminal -> do
--             on terminal do
--                 hput StringTerminalPut
--                 put "Hello " ++ cid ++ "!"
--             client(cid | => ch, terminal)
--     proc client :: [Char] | => Put(NewMsg | RecvMsgs), StringTerminal =
--         cid | => ch, terminal -> do
--             on terminal do
--                 hput StringTerminalPut
--                 put "Enter message in terminal. Press ENTER to close."
--                 hput StringTerminalGet
--                 get msg
--             if isEmpty(msg)
--                 then do
--                     put End(cid) on ch
--                     client_recurse(cid | => ch, terminal)
--                 else do
--                     put Yes(cid ++ ": " ++ msg) on ch
--                     client_recurse(cid | => ch, terminal)
--     proc client_recurse :: [Char] | => RecvMsgs, StringTerminal =
--         cid | => ch, terminal -> do
--             hcase ch of
--                 Recv -> do
--                     client(cid | => ch, terminal)
--                 Close -> do
--                     close ch
--                     on terminal do
--                         hput StringTerminalClose
--                         halt

-- server code
proc server :: | Put(FwdMsg | Link), Console => =  
    | clients, console => -> do
        get rec on clients					-- clients now Link
        case rec of
            Msg(client_msg, trace) -> do
                hput Fwd on clients			-- clients now Put(FwdMsg | Link)
                case client_msg of							
                    Yes(msg) -> do
                        hput ConsolePut on console
                        put msg ++ "  " ++ trace on console
                        server( | clients, console => )
                    End(cid) -> do
                        hput ConsolePut on console
                        put cid ++ " ended session. " ++ trace on console                        
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
            Fwd ->                       -- outbox now Put(FwdMsg | Link)
                nil_node( | => outbox)
            Nil ->                          -- outbox now TopBot
                halt outbox

-- forward_node forwards messages from client until it closes, then will become nil_node
proc forward_node :: [Char] | Put(NewMsg | RecvMsgs) => Put(FwdMsg | Link) =
    node_id | client => outbox -> do
        get rec on client					-- client now RecvMsgs
        case rec of							
            Yes(msg) -> do
                put Msg(Yes(msg), "Trace: forward node " ++ node_id) on outbox           -- outbox now Link
                hcase outbox of
                    Fwd -> do               -- outbox now Put(FwdMsg | Link)
                        hput Recv on client         	-- client now Put(NewMsg | RecvMsgs)
                        forward_node(node_id | client => outbox)
                    Nil -> do              -- i guess the server could just close us :,((( anyway outbox now TopBot
                        close outbox
                        hput Close on client
                        halt client
            End(cid) -> do
                hput Close on client
                close client
                put Msg(End(cid), "Trace: forward node " ++ node_id) on outbox         -- outbox now Link
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
proc racer_node :: [Char] | Put(NewMsg | RecvMsgs), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
    node_id | client, inbox => outbox -> race
        client -> do
            get rec on client					-- client now RecvMsgs
            case rec of							
                Yes(msg) -> do
                    put Msg(Yes(msg), "Trace: racer node " ++ node_id) on outbox           -- outbox now Link
                    hcase outbox of
                        Fwd -> do               -- outbox now Put(FwdMsg | Link)
                            hput Recv on client         	-- client now Put(NewMsg | RecvMsgs)
                            racer_node(node_id | client, inbox => outbox)
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
                    put Msg(End(cid), "Trace: racer node " ++ node_id) on outbox           -- outbox now Link
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
                Msg(msg, trace) -> do
                    put Msg(msg, trace ++ ", racer node " ++ node_id) on outbox           -- outbox now Link
                    hcase outbox of
                        Fwd -> do               -- outbox now Put(FwdMsg | Link)
                            hput Fwd on inbox         	-- inbox now Put(FwdMsg | Link)
                            racer_node(node_id | client, inbox => outbox)
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
                    forward_node(node_id | client => outbox)


-- racer tree nodes:

-- this is the case when the client at this node has closed
-- this is basically the server with two clients from rec-non-det-server
-- assumes that tree_1 has won a race
proc racer_tree_node :: [Char] | Put(FwdMsg | Link), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
    node_id | tree_1, tree_2 => root -> do
        get rec on tree_1                   -- tree_1 now Link
        case rec of			
            Msg(msg, trace) -> do
                put Msg(msg, trace ++ ", racer tree node " ++ node_id) on root        -- root now Link
                hcase root of
                    Fwd -> do               -- root now Put(FwdMsg | Link)
                        hput Fwd on tree_1  -- tree_1 now Put(FwdMsg | Link)
                        race                -- in the recursive case, maintain that tree in position 1 is the winner
                            tree_1 -> racer_tree_node(node_id | tree_1, tree_2 => root)
                            tree_2 -> racer_tree_node(node_id | tree_2, tree_1 => root)
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
    proc racer_tree_client_node :: [Char] | Put(NewMsg | RecvMsgs), Put(FwdMsg | Link), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
        node_id | client, tree_1, tree_2 => root -> race
            client -> do
                get rec on client					-- client now RecvMsgs
                case rec of							
                    Yes(msg) -> do
                        put Msg(Yes(msg), "Trace: racer tree client node " ++ node_id) on root   -- root now Link
                        hcase root of
                            Fwd -> do               -- root now Put(FwdMsg | Link)
                                hput Recv on client -- client now Put(NewMsg | RecvMsgs)
                                racer_tree_client_node(node_id | client, tree_1, tree_2 => root)
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
                        put Msg(End(cid), "Trace: racer tree client node " ++ node_id) on root      -- root now Link
                        hcase root of                  -- after client leaves, this is basically the rec-non-det-server with two clients
                            Fwd -> race                -- root now Put(FwdMsg | Link)
                                tree_1 -> racer_tree_node(node_id | tree_1, tree_2 => root)
                                tree_2 -> racer_tree_node(node_id | tree_2, tree_1 => root)
                            Nil -> do
                                close root
                                race
                                    tree_1 -> racer_tree_close( | tree_1, tree_2 => )
                                    tree_2 -> racer_tree_close( | tree_2, tree_1 => )
            tree_1 -> racer_tree_1_winner(node_id | client, tree_1, tree_2 => root)
            tree_2 -> racer_tree_1_winner(node_id | client, tree_2, tree_1 => root)

    -- helper process to minimize duplicated code 
    -- assumes that the proc at tree_1 has won a race
    proc racer_tree_1_winner :: [Char] | Put(NewMsg | RecvMsgs), Put(FwdMsg | Link), Put(FwdMsg | Link) => Put(FwdMsg | Link) =
        node_id | client, tree_1, tree_2 => root -> do
            get rec on tree_1                           -- tree_1 now Link
            case rec of			
                Msg(msg, trace) -> do
                    put Msg(msg, trace ++ ", racer tree client node " ++ node_id) on root            -- root now Link
                    hcase root of
                        Fwd -> do                   -- root now Put(FwdMsg | Link)
                            hput Fwd on tree_1      -- tree_1 now Put(FwdMsg | Link)
                            racer_tree_client_node(node_id | client, tree_1, tree_2 => root)
                        Nil -> do                   -- i guess the server could just close us :,((( anyway outbox now TopBot
                            close root
                            hput Nil on tree_1
                            close tree_1
                            racer_tree_client_close( | client, tree_2 => )
                Nil -> do                       -- tree_1 is gone, so become racer node with client and tree_2
                    hput Nil on tree_1          -- if we reset to Fwd, we create a livelock :,(
                    close tree_1
                    racer_node(node_id | client, tree_2 => root)


-- wrapper process that doesn't have client id stored, so client can send it once it's connected after the fold?
-- the problem is what if the client doesn't send its id right away and other clients are already trying to send messages.
-- we can't race the client sending its id wiht the other procs sending their messages because we need to append our client's id
-- okay what if we separately label the node processes. the id of the node doesn't need to be the same as the client's id right?
    -- then we need to do this in the fold i think?

-- proc internal_node :: | Put([Char] | Put(NewMsg | RecvMsgs)), Put(FwdMsg | Link), Put(FwdMsg | Link) => Put(FwdMsg | Link) =


-- generate a tree of clients, use h o fold to turn tree into a racer tree, call server with root node
-- the higher order fold will change the tree of RecvMsgs chs into a single channel with
-- the "Link" protocol. basically a recv msgs protocol but with a different datatype
-- it's used by racer tree processes: racer_tree_client_node, racer_tree_node, ... racer_node, forward_node, nil_node
-- if a client drops off, its node will turn into a racer_tree_node which is like the two-channel-non-det server sending up the tree
    -- if one of the subtrees then drops off, it will identify the other subtree with the root channel
-- if one of its subtrees is nil, it will turn into a racer_node (from the linked list version) for the client and other tree
-- anyway the fold will construct the racer tree and "return" a channel connected to the root of the tree
defn 
    proc gen_clients_tree :: Int | PTree( | Put(NewMsg | RecvMsgs) ), Console  => = 
        cid | ptree, console => -> do
            on console do
                hput ConsolePut
                put "Enter number of terminals to generate."
                hput IntConsoleGet
                get n
            gen_n_clients(n, cid | ptree, console => )

    proc gen_n_clients :: Int, Int |  PTree( | Put(NewMsg | RecvMsgs) ), Console  => = 
        n, cid | ptree, console => -> do
            if n <= 0
                then do 
                    on console do 
                        hput ConsolePut
                        put "Terminal generation phase complete."
                    plug
                        -- then we race clients using a h_o_fold to turn the list into a racer TREE??
                        ptree_h_o_fold(1, store(racer_tree_client_node), store(nil_node) | ptree => raced_ptree)            
                        server( | raced_ptree, console => )
                else do                    
                    hput ConsoleStringTerminal on console
                    split console into new_console, neg_term
                    plug
                        neg_term, term => -> neg_term |=| neg term
                        client_wrapper("Client " ++ intToString(cid) | => ch, term)
                        ptree_add( | ch, ptree => new_ptree)
                        gen_n_clients(n - 1, cid + 1 | new_ptree, new_console => )



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

