-- (Test(A,(A,B), (A,B(A,B((), A(), (),A( ))),() ) ), (A := (), B := ( ) ))

potato

defn
    potato
defn
    data 
        A -> [(B, (), A(), (), A( ) , A(B,C) )] =
            Names, Names :: [([A],B,C(A,(B)))] -> Get(A|B)
where 
    data 
        A -> [(B, (), A(), (), A( ) , A(B,C) )] =
            Names, Names :: [([A],B,C(A,(B)))] -> Get(A|B)

data 
    A -> [(B, (), A(), (), A( ) , A(B,C) )] =
        Names, Names :: [([A],B,C(A,(B)))] -> Get(A|B)
  and
    B -> B =
        Names, Names :: B -> [()] (+) (B)

codata 
    B -> B =
        Names, Names :: B -> B 
  and
    B -> B =
        Names, Names :: B -> B 

protocol 
    B => B = 
        Names, Names :: B => B 
    and
    B => B = Names, Names :: B => B 

coprotocol 
    B => B =
        Names, Names :: B => B 
  and
    B => B =
        Names, Names :: () (*) [Neg(A)] (+) (Neg,Neg (+) A) (*) TopBot => B 


fun function :: A,B -> A =
    _ -> a
    _ -> 
        let fun sfa = -> 3 
        in sfaA +   45
    _ -> 
        let fun sfa = -> 3 
        in sfaA + 123 - 123

    _ -> 
        let fun sfa = -> 3 
        in sfaA + 45 

    _ -> 
        let fun sfa = -> 3  
        in 34^3

fun function :: A,B -> A =
    (a : b),(a,b) -> 4
    a -> fold asdf of 
            A : a,b,c -> 4
            B : a,b,c -> 4

    a -> fold asdf of 
            A : a,b,c -> 4
            B : a,b,c -> 4

    a:b,(a,b) -> 
        let fun sfa = -> 3 
        in 3 !! (if 3 then a else b)
    a:b,(a,b) -> if 3 then 5 else 3 + 3
    a:b,(a,b) -> 
        let fun sfa = -> 3 
        in (let fun sfa = -> 3 
            in 3 + 3)

    a -> unfold asdf of
            state of 
                A : a,b,c -> 4
                B : a,b,c -> 4
            state of 
                A : a,b,c -> 4
                B : a,b,c -> 4
        + 3
    a -> case 3 of
        a -> 3
        b -> 3
    a -> Constructrs() 
    a -> Constructrs( ) 
    a -> Constructrs(a, b,c) 
    a -> Constructrs
    a -> callfun()
    a -> callfun(  )
    a -> callfun( () )
    (  ) -> callfun(())
    (  ) -> if a then b else if a then b else c
    (  ) -> if a then b else c !! b
    (  ) -> switch
        a -> b
        c -> d

proc myProcess :: A | A => A = 
    a | b => c -> do
        myProcess( | => )
    b | b => c -> do
        myProcess( | => )

proc myProcess = 
    a | b => c -> do
        myProcess( | => )
    b | b => c -> do
        myProcess( | => )
        close a
        halt a

        get n on ch
        put 3 + 3 on ch

        hcase ch of 
            OhCute -> close a
            OhCute -> do close a

        hput SoSweet on ch

        split ch into a,b,c,d

        fork ch as
            ch1 -> close a
            ch1 with a,b,c -> close a

        ch |=| ch
        ch |=| neg ch

        race 
            ch -> close a
            ch -> close a

        plug
            close a
            do
                close a
                close b

        case 3 + 3 of 
            6 -> close a
            () -> close a

        switch
            3 -> close a


        

