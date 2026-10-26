include Prelude (intToString, isEmpty | )

-- sequential type that we are using like a handle
data NewMsg -> C =
	Yes :: [Char] -> C
	End :: [Char] -> C 

-- used by "linked list" processes
data FwdMsg -> C =
    Msg :: NewMsg -> C
    Nil :: -> C

fun prependToMsg ::  NewMsg, [Char] -> NewMsg =
    End(cid), _ -> End(cid)
    Yes(msg), string -> Yes(string ++ msg)

fun apppendToMsg ::  NewMsg, [Char] -> NewMsg =
    End(cid), _ -> End(cid)
    Yes(msg), string -> Yes(msg ++ string)

data SF(A) -> C =
    SS :: A -> C 
    FF :: -> C 

codata F -> Func(A, B) =
    Eval :: A, F -> B 

fun foldInt :: Func(B, B), B, Int -> B =
    f, b, 0 -> b 
    f, b, n -> Eval(foldInt(f, b, n - 1), f)

data Nat -> N =
    Succ :: N -> N 
    Zero :: -> N

fun foldNat :: Func(B, B), B, Nat -> B =
    f, b, Zero -> b 
    f, b, Succ(n) -> Eval(foldNat(f, b, n), f)

fun getSucc :: -> Func(Nat, Nat) =
    -> (Eval := n -> Succ(n))

fun intToNat :: Int -> Nat =
    n -> foldInt(getSucc, Zero, n)

codata N -> CoNat =
    Pred :: N -> SF(N)

fun infinity :: -> CoNat =
    -> (Pred := -> SS(infinity))

proc test_infinity :: CoNat, Int | Console => = 
    inf, 0 | console => -> on console do
        hput ConsolePut
        put "okay that's enough"
        hput ConsoleClose 
        halt
    inf, x | console => -> do
        case Pred(inf) of
            FF -> on console do
                hput ConsolePut
                put "what?? this should never be printed. infinity never runs out"
                hput ConsoleClose 
                halt
            SS(inf') -> do 
                on console do 
                    hput ConsolePut 
                    put "-1"
                test_infinity(inf', (x - 1) | console => )

-- fun countDown :: Nat -> CoNat =
--     n -> (Pred := -> 
--         case n of
--             Succ(n') -> SS(countDown(n'))
--             Zero -> FF
--         )

fun unfoldCoNat :: Func(A, SF(A)), A -> CoNat =
    f, a -> (Pred := ->
        case (Eval(a, f)) of 
            SS(a') -> SS(unfoldCoNat(f, a'))
            FF -> FF
        )

fun getPred :: -> Func(Nat, SF(Nat)) =
    -> (Eval := n ->
        case n of
            Succ(n') -> SS(n')
            Zero -> FF
        )

fun countDown :: Nat -> CoNat =
    n -> unfoldCoNat(getPred, n)

fun minusOne :: -> Func(Int, SF(Int)) =
    -> (Eval := x ->
        if x <= 0
            then FF
            else SS(x - 1)
        )

fun countDownInt :: Int -> CoNat =
    x -> unfoldCoNat(minusOne, x)

proc take_step :: CoNat | Console => =
    x | console => -> do
        case Pred(x) of 
            FF -> on console do
                hput ConsoleClose 
                halt
            SS(x') -> do 
                on console do 
                    hput ConsolePut 
                    put "-1"
                take_step(x' | console => )

proc test_countDown :: Int | Console => =
    i | console => -> do
        on console do
            hput ConsolePut
            put "i can be -1 this many times: "
        -- take_step(countDown(intToNat(i)) | console => )
        take_step(countDownInt(i) | console => )


fun foldList :: Func((A,B), B), B, [A] -> B =
    f, b, [] -> b 
    f, b, (a:aa) -> Eval((a, foldList(f, b, aa)), f)

codata S -> InfList(A) =
    HT :: S -> SF((A, S))

fun unfoldList :: Func(A, SF(B)), Func(A, SF(A)), A -> InfList(B) =
    -- given destructor and initial value, returns the record:
    head, tail, a -> (HT := -> 
        case (Eval(a, head), Eval(a, tail)) of       -- implementation of HT
            (SS(b), SS(a')) -> SS((b, unfoldList(head, tail, a')))
            x -> FF
        )

-- codata F -> Func(A, B) =
--     Eval :: F, A -> B 

-- example using unfold on an int to generate a list of ints?
fun getNum :: -> Func((Int, Func(Int, Int)), SF(Int)) =
    -> (Eval := (x, f) -> SS(x))

fun sequenceNums :: -> Func((Int, Func(Int, Int)), SF((Int, Func(Int, Int))))=
    -> (Eval := (x, f) -> SS((Eval(x,f), f)))

fun plusOne :: -> Func(Int, Int) =
    -> (Eval := x -> x + 1)

fun infStream :: -> InfList(Int) =
    -> unfoldList(getNum, sequenceNums, (1, plusOne))

fun collatz :: -> Func(Int, Int) =
    -> (Eval := x ->
        if x == 1 
            then 0  -- yeah yeah yeah i know but i don't actually want it to be inf
            else if (x % 2) == 0
                then x / 2
                else 3 * x + 1        
        )

fun getCollatz :: -> Func((Int, Func(Int, Int)), SF(Int)) =
    -> (Eval := (x, f) ->
            if x == 0
                then FF
                else SS(x)
        )

fun collatzStream :: Int -> InfList(Int) =
    x -> unfoldList(getCollatz, sequenceNums, (x, collatz))

proc print_collatz :: Int | Console => =
    0 | console => -> on console do
        hput ConsolePut
        put "end of collatz seq"
        hput ConsoleClose
        halt
    1 | console => -> do 
        on console do
            hput IntConsolePut
            put 1
        print_collatz(0 | console => )
    x | console => -> do
        on console do
            hput IntConsolePut
            put x
        if (x % 2) == 0
            then print_collatz(x / 2 | console => )
            else print_collatz(3 * x + 1 | console => )


proc print_ints :: Int, InfList(Int) | Console => =
    0, stream | console => -> on console do
        hput ConsolePut
        put "okay that's enough"
        hput ConsoleClose
        halt
    x, stream | console => -> do
        case HT(stream) of
            FF -> on console do
                hput ConsolePut
                put "the stream has ended. (this should not happen if the stream is infinite.)"
                hput ConsoleClose
                halt
            SS((i, stream')) -> do
                on console do
                    hput IntConsolePut
                    put i 
                print_ints((x - 1), stream' | console => )

proc test_collatzStream :: Int | Console => =
    x | console => -> do
        on console do
            hput ConsolePut
            put "how many numbers are in this collatz sequence?"
        print_ints(x * 10, collatzStream(x) | console => ) 


proc test_infStream :: Int | Console => =
    x | console => -> do
        on console do
            hput ConsolePut
            put "how many numbers are in this infinite stream?"
        print_ints(x, infStream | console => ) 

-- fun unfoldList :: Func(A, SF(B,InfList(B))), A -> InfList(B) =
--     -- given a destructor and initial value, returns the record:
--     ht, a -> (HT := -> Eval(ht, a))        -- implementation of HT

-- fun nextNum :: -> Func((Int, Func(Int, Int)), SF(Int,InfList(Int))) =
--     -> (Eval := (x, f) -> SS(x, unfoldList(nextNum, (Eval(f, x), f))))

-- fun plusOne :: -> Func(Int, Int) =
--     -> (Eval := x -> x + 1)

-- fun infStream :: -> InfList(Int) =
--     -> unfoldList(nextNum, (1, plusOne))

fun sumInts :: -> Func((Int, Int), Int) = -- produces a Func instance
    -> (Eval := (a, b) -> a + b)

fun sumList :: [Int] -> Int =
    list -> foldList(sumInts, 0, list)

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
        hput PListCons on ret_chs
        fork ret_chs as
            ret_ch with ch -> ret_ch |=| ch
            ret_list with chs -> ret_list |=| chs

protocol Maybe( | M) => S =
    Just :: M => S 
    Noth :: TopBot => S

protocol Either( | X, Y) => S =
    Left :: X => S 
    Right :: Y => S

proc plist_ht :: | PList( | M) => Maybe( | M (*) PList( | M)) =
    | chs => ret_chs -> do
        hcase chs of 
            PListEmpty -> do
                close chs
                hput Noth on ret_chs
                halt ret_chs
            PListCons -> do                 -- chs now (*)
                split chs into ch, new_chs  -- peel one off
                hput Just on ret_chs
                fork ret_chs as
                    ret_ch with ch -> ret_ch |=| ch
                    ret_list with new_chs -> ret_list |=| new_chs


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

-- what about if we want to put values in the processes for like proc ids or something?
-- proc drivePListWithSeq :: Store(A | X, Y => Y), Store(A | => Y) | PList( | X) => Y =
--     cons_proc, nil_proc | chs => ret_ch -> do
--         hcase chs of
--             PListNil -> do                -- chs now TopBot
--                 close chs
--                 use(nil_proc)( | => ret_ch)
--             PListCons -> do                 -- chs now (*)
--                 split chs into ch, new_chs  -- peel one off
--                 plug
--                     drivePList(cons_proc, nil_proc | new_chs => folded_new_chs) -- recurse on the rest
--                     use(cons_proc)( | ch, folded_new_chs => ret_ch)             -- replace cons with cons_proc

-- fun unfoldList :: Func(A, SF(B)), Func(A, SF(A)), A -> InfList(B) =
--     -- given destructor and initial value, returns the record:
--     head, tail, a -> (HT := -> 
--         case (Eval(a, head), Eval(a, tail)) of       -- implementation of HT
--             (SS(b), SS(a')) -> SS((b, unfoldList(head, tail, a')))
--             x -> FF
--         )

proc plist_h_o_unfold :: Store( | X => Maybe( | Y (*) X)) | X => PList( | Y) =
    ht_proc | ch => ret_chs -> do
        plug
            ch => maybe_ret_ch_new_ch -> use(ht_proc)( | ch => maybe_ret_ch_new_ch)
            maybe_ret_ch_new_ch => ret_chs -> do
                hcase maybe_ret_ch_new_ch of
                    Just -> do                     
                        split maybe_ret_ch_new_ch into ret_ch, new_ch
                        plug 
                            new_ch => ret_list -> plist_h_o_unfold(ht_proc | new_ch => ret_list)
                            ret_ch, ret_list => ret_chs -> plist_cons( | ret_ch, ret_list => ret_chs)
                    Noth -> do
                        close maybe_ret_ch_new_ch
                        hput PListEmpty on ret_chs
                        halt ret_chs


proc plist_map :: Store( | X => Y) | PList( | X) => PList( | Y) =
    transformation_proc | in_chs => out_chs -> do
        hcase in_chs of
            PListEmpty -> do                -- chs now TopBot
                close in_chs
                plist_nil( | => out_chs)
            PListCons -> do                 -- chs now (*)
                split in_chs into x_ch, x_chs  -- peel one off
                plug
                    x_ch => y_ch -> use(transformation_proc)( | x_ch => y_ch)
                    plist_map( transformation_proc | x_chs => y_chs)
                    plist_cons( | y_ch, y_chs => out_chs)




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


proc forwarder :: | Put(NewMsg | RecvMsgs) => Put(NewMsg | RecvMsgs) =
    | client => server -> do
        get msg on client
        put prependToMsg(msg, "Forwarded: ") on server 
        hcase server of
            Recv -> do
                hput Recv on client
                forwarder( | client => server )
            Close -> do
                close server
                on client do
                    hput Close
                    halt


-- generate a list of clients, use h o fold to turn list into a racer linked list, call server with start of linked list
-- the higher order fold will change the list of RecvMsgs chs into a single channel with
-- the "linked list" protocol. basically a recv msgs protocol but with a different datatype
-- it's used by linked list node processes: racer, forward, nil
-- if a client drops off, its racer node will id the other channels - unless the rest of the list is nil then becomes nil_node
-- this is based on the idea of constant time deletion in a doubley linked list by identifying forward/backwards pointers
-- anyway the fold will construct the racer linked list and "return" a channel connected to the start of the linked list
-- proc gen_clients_list :: Int | PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
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
--                 plug
--                     -- then we race clients using a h_o_fold to turn the list into a racer linked list
--                     plist_map( store(forwarder) | plist => mapped_plist)
--                     plist_h_o_fold( store(racer_node), store(nil_node) | mapped_plist => raced_plist)            
--                     server( | raced_plist, console => )
--             else do        
--                 hput ConsoleStringTerminal on console
--                 split console into new_console, neg_term
--                 plug
--                     neg_term, term => -> neg_term |=| neg term
--                     client_wrapper("Client " ++ intToString(cid) | => ch, term)
--                     plist_cons( | ch, plist => new_chs)
--                     gen_clients_list( cid + 1 | new_chs, new_console => )

-- defn 
--     proc gen_clients_list :: Int | PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
--         cid | plist, console => -> do
--             on console do
--                 hput ConsolePut
--                 put "Enter number of terminals to generate."
--                 hput IntConsoleGet
--                 get n
--             gen_n_clients(n, cid | plist, console => )

--     proc gen_n_clients :: Int, Int |  PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
--         n, cid | plist, console => -> do
--             if n <= 0
--                 then do 
--                     on console do 
--                         hput ConsolePut
--                         put "Terminal generation phase complete."
--                     plug
--                         -- then we race clients using a h_o_fold to turn the list into a racer linked list
--                         plist_map( store(forwarder) | plist => mapped_plist)
--                         plist_h_o_fold( store(racer_node), store(nil_node) | mapped_plist => raced_plist)            
--                         server( | raced_plist, console => )
--                 else do                    
--                     hput ConsoleStringTerminal on console
--                     split console into new_console, neg_term
--                     plug
--                         neg_term, term => -> neg_term |=| neg term
--                         client_wrapper("Client " ++ intToString(cid) | => ch, term)
--                         plist_cons( | ch, plist => new_chs)
--                         gen_n_clients(n - 1, cid + 1 | new_chs, new_console => )

defn 
    proc gen_clients_list :: Int | PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
        cid | plist, console => -> do
            on console do
                hput ConsolePut
                put "Enter number of terminals to generate."
                hput IntConsoleGet
                get n
            plug
                plist_h_o_unfold(store() | console => plist)
            -- gen_n_clients(n, cid | plist, console => )

    proc gen_n_clients :: Int, Int |  PList( | Put(NewMsg | RecvMsgs) ), Console  => = 
        n, cid | plist, console => -> do
            if n <= 0
                then do 
                    on console do 
                        hput ConsolePut
                        put "Terminal generation phase complete."
                    plug
                        -- then we race clients using a h_o_fold to turn the list into a racer linked list
                        plist_map( store(forwarder) | plist => mapped_plist)
                        plist_h_o_fold( store(racer_node), store(nil_node) | mapped_plist => raced_plist)            
                        server( | raced_plist, console => )
                else do                    
                    hput ConsoleStringTerminal on console
                    split console into new_console, neg_term
                    plug
                        neg_term, term => -> neg_term |=| neg term
                        client_wrapper("Client " ++ intToString(cid) | => ch, term)
                        plist_cons( | ch, plist => new_chs)
                        gen_n_clients(n - 1, cid + 1 | new_chs, new_console => )

-- proc plist_h_o_unfold :: Store( | X => Maybe( | Y (*) X)) | X => PList( | Y) =
--     ht_proc | ch => ret_chs -> do
--         plug
--             ch => maybe_ret_ch_new_ch -> use(ht_proc)( | ch => maybe_ret_ch_new_ch)
--             maybe_ret_ch_new_ch => ret_chs -> do
--                 hcase maybe_ret_ch_new_ch of
--                     Just -> do                     
--                         split maybe_ret_ch_new_ch into ret_ch, new_ch
--                         plug 
--                             new_ch => ret_list -> plist_h_o_unfold(ht_proc | new_ch => ret_list)
--                             ret_ch, ret_list => ret_chs -> plist_cons( | ret_ch, ret_list => ret_chs)
--                     Noth -> do
--                         close maybe_ret_ch_new_ch
--                         hput PListEmpty on ret_chs
--                         halt ret_chs

-- make a list server with two phases. 
-- first phase generates arbitrarily many clients using the console and puts the channels in a list
-- in the second, the server recursively prints winning messages as they are forwarded up the racer linked list chain

proc run :: | Console => =
    | console => -> do
        -- on console do
        --     hput ConsolePut
        --     put "how many times can we -1 infinity?"
        -- test_infinity(infinity, 100 | console => )
        on console do
            hput ConsolePut
            put "Enter number to collatz:"
            hput IntConsoleGet
            get x
        print_collatz(x | console => )
        -- test_collatzStream(x | console => )
        -- test_infStream(100 | console => )
        -- on console do
        --     hput ConsolePut
        --     put "let i = 7."
        -- test_countDown(7 | console => )


-- proc run :: | Console => =       
--     | console => -> plug  
--         plist_nil( | => nil)
--         -- this will generate a list of clients, 
--         -- use a h o fold to turn it into a racer linked list,
--         -- and finally call the server with the start of the linked list
--         gen_clients_list( 1 | nil, console => )          

