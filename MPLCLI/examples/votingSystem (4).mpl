protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

coprotocol S => Timer =
    Timer :: S => Get(Int | S (*) Put( () | TopBot))
    TimerClose :: S => TopBot

protocol MemCell (A | ) => M =
    MemPut :: Put(A|M) => M
    MemGet :: Get(A|M) => M 
    MemCls :: TopBot => M

fun append :: [A],[A] -> [A] =
    [],ts -> ts
    s:ss,ts -> s : append(ss,ts)

fun concat :: [[A]] -> [A] =
	[] -> []
	s:ss -> append(s, concat(ss))

fun land :: Bool, Bool -> Bool =
    True, True -> True
    _, _ -> False
    
fun not :: Bool -> Bool =
    True -> False
    False -> True

fun stringEqual :: [Char], [Char] -> Bool =
    [], [] -> True
    (x:xs), (y:ys) -> land(x == y, stringEqual(xs, ys))
    _, _ -> False

protocol Passer( | M ) => S =
    Passer :: M (+) (Neg(M) (*) Put(() | S)) => S 

proc memCell :: A | MemCell(A | ) =>  =
    val | ch => -> hcase ch of
        MemPut -> do
            get nval on ch
            memCell(nval | ch => )
        MemGet -> do
            put val on ch
            memCell(val | ch => )
        MemCls -> do
            halt ch

data Maybe(A) -> S =
    Just :: A -> S
    Nothing :: -> S

data Either(A, B) -> S =
    Left :: A -> S
    Right :: B -> S

codata F -> Fun(A, B) =
    App :: A, F -> B

fun map :: [A], Fun(A, B) -> [B] =
    [], _ -> []
    (x:xs), f -> App(x, f):map(xs,f)

defn
    fun showInt :: Int -> [Char] =
        0 -> "0"
        n -> convert(n, [])
where
    fun intToDigit :: Int -> Char =
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        _ -> '#'

    fun divqr :: Int, Int -> (Int, Int) =
        a, b -> if a < b
            then (0, a)
            else case divqr(a - b, b) of
                (q, r) ->  (q + 1, r)

    fun convert :: Int, [Char] -> [Char] =
        0, acc -> acc
        n, acc -> case divqr(n, 10) of
            (q, r) -> convert(q, intToDigit(r):acc)


codata V -> Voter =
    Name :: V -> [Char]
    HasVoted :: V -> Bool
    IsVoting :: V -> Bool

fun makeVoter :: [Char], Bool, Bool -> Voter = 
    name, hasVoted, isVoting -> (Name := -> name,  HasVoted := -> hasVoted, IsVoting := -> isVoting)

codata C -> Candidate =
    CName :: C -> [Char]
    VoteCount :: C -> Int

fun makeCandidate :: [Char], Int -> Candidate = 
    name, voteCount -> (CName := -> name,  VoteCount := -> voteCount)

codata M -> MemData =
    Voters :: M -> [Voter]
    Candidates :: M -> [Candidate]

fun makeMemData :: [Voter], [Candidate] -> MemData =
    voters, candidates -> (Voters := -> voters, Candidates := -> candidates)

protocol Vote => S =
    ValidateVoter :: Put([Char] | Get(Either([Char], Voter) | S)) => S
    SubmitVote :: Put([Char] | Get(Either([Char], ()) | S)) => S
    GetResults :: Get([Candidate] | S) => S
    QuitVoting :: S => S

defn
    proc votingClient :: Maybe(Voter) | Timer => Vote, StringTerminal =
        maybeVoter | timer => vch, strterm ->
            case maybeVoter of
                Nothing -> do
                    hput StringTerminalPut on strterm
                    put "Please enter your name or press r to see the results:" on strterm

                    hput StringTerminalGet on strterm
                    get userInput on strterm
                    case stringEqual(userInput, "r") of
                        True -> do
                            hput GetResults on vch
                            get results on vch
                            hput StringTerminalPut on strterm
                            put showResults(results) on strterm
                            votingClient(Nothing | timer => vch, strterm)

                        False -> do
                            hput ValidateVoter on vch
                            put userInput on vch
                            get response on vch

                            case response of
                                Left(error) -> do
                                    hput StringTerminalPut on strterm
                                    put error on strterm
                                    votingClient(Nothing | timer => vch, strterm)
                                Right(voter) -> do
                                    votingClient(Just(voter) | timer => vch, strterm)

                Just(voter) -> do
                    hput StringTerminalPut on strterm
                    put "Please enter your candidate's name:" on strterm

                    hput StringTerminalGet on strterm

                    hput Timer on timer
                    put 15 * 1000000 on timer

                    split timer into futTimer, currTimer

                    race
                        strterm -> do
                            get candidateName on strterm
                            hput SubmitVote on vch
                            put candidateName on vch
                            get response on vch

                            case response of
                                Left(error) -> do
                                    hput StringTerminalPut on strterm
                                    put error on strterm
                                    resetTimerAndRecurse(Just(voter) | futTimer, currTimer => vch, strterm)
                                Right(_) -> do
                                    hput StringTerminalPut on strterm
                                    put "Your vote has been cast successfully!" on strterm
                                    hput QuitVoting on vch
                                    resetTimerAndRecurse(Nothing | futTimer, currTimer => vch, strterm)

                        currTimer -> do
                            get () on currTimer
                            close currTimer
                            hput QuitVoting on vch
                            get _ on strterm
                            hput StringTerminalPut on strterm
                            put "Time Out! please try again!" on strterm
                            votingClient(Nothing | futTimer => vch, strterm)
    
    proc resetTimerAndRecurse = 
        maybeVoter | futTimer, currTimer => vch, strterm ->
            plug
                futTimer, currTimer => nntimer -> do
                    fork nntimer as
                        futntimer -> futntimer |=| futTimer
                        currntimer -> do
                            get _ on currTimer
                            close currTimer
                            halt currntimer
                nntimer => ntimer -> do
                    split nntimer into t0, t1
                    close t1
                    t0 |=| ntimer
                votingClient(maybeVoter | ntimer => vch, strterm )
where
    fun showCandidateRes :: Candidate -> [Char] =
        candidate -> append(append(append(CName(candidate), ": "), showInt(VoteCount(candidate))), "\n")
    fun showResults :: [Candidate] -> [Char] =
        candidates -> concat(map(candidates, (App := c -> showCandidateRes(c))))

defn                          
    proc votingServer :: Maybe(Voter) | Vote => Put(() | Passer( | MemCell(MemData | ))) = 
        maybeVoter | vch => passer ->
            hcase vch of
                ValidateVoter -> do
                    get voterName on vch
                    
                    put () on passer
                    hput Passer on passer
                    split passer into mem, negMemAndPasser

                    hput MemGet on mem
                    get memData on mem

                    case findVoter(voterName, Voters(memData)) of
                        Nothing -> do
                            put Left("Voter Not registered") on vch
                            passMemAndRecurse(Nothing | vch => mem, negMemAndPasser)
                        Just(voter) -> do
                            switch
                                IsVoting(voter) -> do
                                    put Left("Voter is voting in another station") on vch
                                    passMemAndRecurse(Nothing | vch => mem, negMemAndPasser)
                                HasVoted(voter) -> do
                                    put Left("Voter has already voted") on vch
                                    passMemAndRecurse(Nothing | vch => mem, negMemAndPasser)
                                True -> do
                                    put Right(voter) on vch
                                    hput MemPut on mem
                                    put replaceVoter(voter, toggleIsVoting(voter), memData) on mem
                                    passMemAndRecurse(Just(toggleIsVoting(voter)) | vch => mem, negMemAndPasser)
                SubmitVote -> do
                    get candidateName on vch
                    case maybeVoter of
                        Nothing -> do
                            put Left("Voter not specified") on vch
                            votingServer(Nothing | vch => passer)
                        Just(voter) -> do
                            put () on passer
                            hput Passer on passer
                            split passer into mem, negMemAndPasser


                            hput MemGet on mem
                            get memData on mem
                            
                            case findCandidate(candidateName, Candidates(memData)) of
                                Nothing -> do
                                    put Left("Candidate Not Found!") on vch
                                    passMemAndRecurse(Just(voter) | vch => mem, negMemAndPasser)
                                Just(candidate) -> do
                                    put Right(()) on vch
                                    hput MemPut on mem
                                    put replaceCandidate(candidate, incrementVoteCount(candidate), memData) on mem
                                    hput MemGet on mem
                                    get memData on mem
                                    hput MemPut on mem
                                    put replaceVoter(voter, toggleHasVoted(voter), memData) on mem
                                    passMemAndRecurse(Just(toggleHasVoted(voter)) | vch => mem, negMemAndPasser)
                GetResults -> do
                    put () on passer
                    hput Passer on passer
                    split passer into mem, negMemAndPasser

                    hput MemGet on mem
                    get memData on mem

                    put Candidates(memData) on vch
                    passMemAndRecurse(maybeVoter | vch => mem, negMemAndPasser)

                QuitVoting ->
                    case maybeVoter of
                        Nothing -> votingServer(Nothing | vch => passer)
                        Just(voter) -> do
                            put () on passer
                            hput Passer on passer
                            split passer into mem, negMemAndPasser
                            hput MemGet on mem
                            get memData on mem

                            hput MemPut on mem
                            put replaceVoter(voter, toggleIsVoting(voter), memData) on mem
                            passMemAndRecurse(Nothing | vch => mem, negMemAndPasser)

    proc passMemAndRecurse = 
        maybeVoter | vch => mem, negMemAndPasser ->
            fork negMemAndPasser as
                negMem -> negMem |=| neg mem
                passer -> votingServer(maybeVoter | vch => passer)
where
    fun findVoter :: [Char], [Voter] -> Maybe(Voter) =
        _, [] -> Nothing
        x, v:vs -> if stringEqual(x, Name(v)) then Just(v) else findVoter(x, vs) 

    fun toggleHasVoted :: Voter -> Voter =
        voter -> makeVoter(Name(voter), not(HasVoted(voter)), IsVoting(voter))
    fun toggleIsVoting :: Voter -> Voter =
        voter -> makeVoter(Name(voter), HasVoted(voter), not(IsVoting(voter)))
    fun removeVoter :: [Char], [Voter] -> [Voter] =
        _, [] -> []
        x, v:vs -> if stringEqual(x, Name(v)) then vs else v:removeVoter(x, vs)

    fun replaceVoter :: Voter, Voter, MemData -> MemData =
        prevVoter, newVoter, memData -> makeMemData(newVoter:removeVoter(Name(prevVoter), Voters(memData)), Candidates(memData))
    
    fun findCandidate :: [Char], [Candidate] -> Maybe(Candidate) =
        _, [] -> Nothing
        x, v:vs -> if stringEqual(x, CName(v)) then Just(v) else findCandidate(x, vs) 
    fun incrementVoteCount :: Candidate -> Candidate =
        candidate -> makeCandidate(CName(candidate), VoteCount(candidate) + 1)
        
    fun removeCandidate :: [Char], [Candidate] -> [Candidate] =
        _, [] -> []
        x, v:vs -> if stringEqual(x, CName(v)) then vs else v:removeCandidate(x, vs)

    fun replaceCandidate :: Candidate, Candidate, MemData -> MemData =
        prevCandidate, newCandidate, memData -> (Voters := -> Voters(memData), Candidates := -> newCandidate:removeCandidate(CName(prevCandidate), Candidates(memData)))
 

defn
    proc memAccessRacer :: | Put( () | Passer(| MemCell(A | ))), Put( () | Passer(| MemCell(A | ))), Put( () | Passer(| MemCell(A | )))  => MemCell(A | ) = 
        | ch1, ch2, ch3 => mem -> race 
            ch1 -> memAccessHelper(| ch1, ch2, ch3 => mem)
            ch2 -> memAccessHelper(| ch2, ch1, ch3 => mem)
            ch3 -> memAccessHelper(| ch3, ch1, ch2 => mem)

    proc memAccessHelper :: | Put( () | Passer(| MemCell(A | ))), Put( () | Passer(| MemCell(A | ))), Put( () | Passer(| MemCell(A | )))  => MemCell(A | ) =
        | winner, loser1, loser2 => mem -> do
            get _ on winner
            hcase winner of 
                Passer -> fork winner as 
                    rmem -> mem |=| rmem
                    negMemAndWinner -> do
                        split negMemAndWinner into negmem, nwinner
                        plug 
                            memAccessRacer( | nloser1, nloser2, nwinner => nmem )
                            negmem, nmem => -> negmem |=| neg nmem
                            loser1 => nloser1 -> do
                                get _ on loser1
                                put () on nloser1
                                nloser1 |=| loser1
                            loser2 => nloser2 -> do
                                get _ on loser2
                                put () on nloser2
                                nloser2 |=| loser2
proc run =
    | console, timer1, timer2, timer3 => strterm1, strterm2, strterm3 ->
        plug
            votingClient(Nothing | timer1 => vch1, strterm1)
            votingClient(Nothing | timer2 => vch2, strterm2)
            votingClient(Nothing | timer3 => vch3, strterm3)

            votingServer(Nothing | vch1 => ch1)
            votingServer(Nothing | vch2 => ch2)
            votingServer(Nothing | vch3 => ch3)

            memAccessRacer(| ch1, ch2, ch3 => mem)
            memCell(makeMemData([makeVoter("Melika", False, False), makeVoter("Saina", False, False), makeVoter("Robin", False, False), makeVoter("Jon", False, False)], [makeCandidate("Trump", 0), makeCandidate("Biden", 0), makeCandidate("Clinton", 0)]) | mem => )

        