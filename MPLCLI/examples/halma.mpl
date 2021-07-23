{-

(halma is a bit easier to play)

              /a\
             /a a\
            /a a a\
  _________/a a a a\________
  \. . . . . . . . . . . . ./   
   \. . . . . . . . . . . ./   
    \. . . . . . . . . . ./   
     \. . . . . . . . . ./   
     / . . . . . . . . . \
    / b . . . . . . . . c \
   / b b . . . . . . . c c \
  / b b b . . . . . . c c c \
 / b b b b . . . . . c c c c \
/---------- . . . . ----------\
           \ . . . /
            \ . . /
             \ . /
              \ /

-- notes: there are 121 different spots.


8x8 halma
10 pieces for each player
 _______________________________
| d | d | d | d | c | c | c | c |
---------------------------------
| d | d | d |   |   | c | c | c |
---------------------------------
| d | d |   |   |   |   | c | c |
---------------------------------
| d |   |   |   |   |   |   | c |
---------------------------------
| a |   |   |   |   |   |   | b |
---------------------------------
| a | a |   |   |   |   | b | b |
---------------------------------
| a | a | a |   |   | b | b | b |
---------------------------------
| a | a | a | a | b | b | b | b |
---------------------------------

      a   b   c   d   e   f   g   h
     _______________________________
 8  | @ | @ | @ | @ | # | # | # | # |  8
    ---------------------------------
 7  | @ | @ | @ |   |   | # | # | # |  7
    ---------------------------------
 6  | @ | @ |   |   |   |   | # | # |  6
    ---------------------------------
 5  | @ |   |   |   |   |   |   | # |  5
    ---------------------------------
 4  | % |   |   |   |   |   |   | $ |  4
    ---------------------------------
 3  | % | % |   |   |   |   | $ | $ |  3
    ---------------------------------
 2  | % | % | % |   |   | $ | $ | $ |  2
    ---------------------------------
 1  | % | % | % | % | $ | $ | $ | $ |  1
    ---------------------------------
      a   b   c   d   e   f   g   h

-}

{-
protocol Passer( | M ) => S =
    Passer :: M (+) (Neg(M) (*) S)  => S
-}

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

{-
protocol 
    Passer( | M ) => S =
        Passer :: M (+) T => S

    and 

    PassBack( | M) => T =
        PassBack :: Neg(M) (*) S => T
        Passed :: TopBot => T
-}

{-
protocol 
    Passer( | M ) => S =
        Passer :: M (+) (Neg(M) (*) S) => S
-}

protocol 
    PasserForward( | M) => S =
        PassForward :: M (+) (M (*) S) => S
        PasserTurn :: M (+) (M (*) T) => S
        
    and 

    PasserBackward( | M ) => T =
        PassBackward :: M (+) (Neg(M) (*) T) => T

protocol MemCell (A | ) => S =
    MemPut :: Put(A|S) => S
    MemGet :: Get(A|S) => S
    MemCls :: TopBot => S

proc memCell :: A | MemCell(A | ) => =
    val | ch => -> hcase ch of
        MemPut -> do
            get nval on ch
            memCell(nval | ch => )
        MemGet -> do
            put val on ch
            memCell(val | ch => )
        MemCls -> halt ch


{-
defn 
    proc distributor :: | => MemCell( [Char] | ),  StringTerminal, StringTerminal, StringTerminal =
        | => mem, _strterm0, _strterm1, _strterm2 -> plug
            initpasser( | => mem, passbackward, _strterm0, _strterm1, _strterm2)
            endpasser( | => passbackward )
where
    proc initpasser :: |  => MemCell( [Char] | ), PassBackward( MemCell( [Char] | ) | ), StringTerminal, StringTerminal, StringTerminal =
        | => mem, passbackward , _strterm0, _strterm1, _strterm2 -> do
            plug 
               p0( |       passer => mem, passbackward, _strterm0)
               p1( | passbackward => passer, _strterm1)

    proc p0 = 
        | passer => mem, passbackward, _strterm0 -> hcase passer of 
            PassForward -> do

            PasserTurn -> do
                hput PasserBackward on passer
                split passer into nmem, negmemandnpasserbackward



    proc p1 = 
        | => passer, _strterm0 -> hcase passer of 
            hput PassForward on passer
-}

{-
protocol 
    Passable( | M)
    -}

defn 
    proc distributor :: | => MemCell( [Char] | ),  StringTerminal, StringTerminal, StringTerminal, StringTerminal  =
        | => mem, _strterm0, _strterm1, _strterm2, _strterm3 -> 
            plug 
                root( | rootl, rootr          => mem)
                branch( |  llpassup, lrpassup => rootl)
                branch( |  rlpassup, rrpassup => rootr)
                leaf(   |                     => llpassup , _strterm0)
                leaf(   |                     => lrpassup , _strterm1)
                leaf(   |                     => rlpassup , _strterm2)
                leaf(   |                     => rrpassup , _strterm3)
where
    proc root =
        | lch, rch => mem -> do 
            close lch
            close rch

            hput MemCls on mem
            halt mem

    proc branch = 
        | lch, rch => nodech -> do
            close lch
            close rch
            halt nodech

    proc leaf = 
        | => passer , _strterm  -> do
            close passer 

            hput StringTerminalClose on _strterm
            halt _strterm
            

{-
defn 
    proc distributor :: | => MemCell( [Char] | ),  StringTerminal, StringTerminal, StringTerminal =
        | => mem, _strterm0, _strterm1, _strterm2 -> 
            plug 
                redistribute( | passback => mem, _strterm0, _strterm1)
                passback( |                 => passback, _strterm2)
where

    proc redistribute :: | PasserBackward( | MemCell([Char] | )) => MemCell( [Char] | ),  StringTerminal, StringTerminal =
        | passbackward => mem, _strterm0, _strterm1 ->
            plug 
                p0( | passforward      => mem, _strterm0)
                p1( | passbackward => passforward, _strterm1)

    proc p0 :: | Passer( | MemCell([Char] | )) => MemCell( [Char] | ),  StringTerminal =
        | passforward => mem, _strterm -> do
            hput MemGet on mem
            get inp on mem

            hput StringTerminalPut on _strterm
            put "p0" on _strterm
            hput StringTerminalPut on _strterm
            put inp on _strterm
            hput StringTerminalGet on _strterm
            get ninp on _strterm

            hput MemPut on mem
            put ninp on mem

            hcase passforward of 
                PassForward -> fork passforward of
                    nmem with mem -> nmem |=| mem
                    nmemandnpasser with _strterm -> do
                        split nmemandnpasser into nmem,npasser
                        plug 
                            p0( | npasser => z, _strterm )
                            nmem => z -> nmem |=| z

                PassBackward -> fork passforward of
                    nmem with mem -> nmem |=| mem
                    nmemandnpasser with _strterm -> do
                        split nmemandnpasser into nmem,npasser
                        plug 
                            p0( | npasser => z, _strterm )
                            nmem => z -> nmem |=| neg z

    proc p1 :: | Passer( | MemCell([Char] | )) => Passer( | MemCell([Char] | )),  StringTerminal =
        | passbackward => passforward, _strterm -> do
            hput PassForward on passforward
            split passforward into mem, nmemandnpasser


            hput MemGet on mem
            get inp on mem

            hput StringTerminalPut on _strterm
            put "p0" on _strterm
            hput StringTerminalPut on _strterm
            put inp on _strterm
            hput StringTerminalGet on _strterm
            get ninp on _strterm

            hput MemPut on mem
            put ninp on mem
-}



                    




{-
defn 
    proc distributor :: | => MemCell( [Char] | ),  StringTerminal, StringTerminal, StringTerminal =
        | => mem, _strterm0, _strterm1, _strterm2 -> 
            plug 
                redistribute( | passback => mem, _strterm0, _strterm1)
                passback( |                 => passback, _strterm2)
where
    proc redistribute :: | Passer( | MemCell([Char] | )) => MemCell( [Char] | ),  StringTerminal, StringTerminal =
        | passthrough => mem, _strterm0, _strterm1 ->
            plug 
                p0( | passmem      => mem, _strterm0)
                p1( | passback => passmem, _strterm1)

    proc p0 :: | Passer( | MemCell([Char] | )) => MemCell( [Char] | ), StringTerminal = 
        | passer => mem, _strterm -> do
            hput MemGet on mem
            get inp on mem

            hput StringTerminalPut on _strterm
            put "p0" on _strterm
            hput StringTerminalPut on _strterm
            put inp on _strterm
            hput StringTerminalGet on _strterm
            get ninp on _strterm

            hput MemPut on mem
            put ninp on mem

            hcase passer of 
                Passer -> fork passer as 
                    nmem with mem -> nmem |=| mem
                    negmemandnpasser with _strterm -> do
                        split negmemandnpasser into negmem,npasser 
                        plug 
                            p0( | npasser => z, _strterm )
                            negmem, z => -> z |=| neg negmem
                            
                            
    proc p1 :: | Passer( | MemCell([Char] | )) => Passer( | MemCell([Char] | )), StringTerminal = 
        |  passback => passer, _strterm -> do
            hput Passer on passer 
            split passer into mem, negmemandnpasser

            hput MemGet on mem
            get inp on mem
                    
            hput StringTerminalPut on _strterm
            put "p1" on _strterm
            hput StringTerminalPut on _strterm
            put inp on _strterm
            hput StringTerminalGet on _strterm
            get ninp on _strterm

            hput MemPut on mem
            put ninp on mem

            fork negmemandnpasser as
                negmem with mem -> mem |=| neg negmem
                npasser with _strterm -> p1( | => npasser, _strterm)

    proc passback :: | => Passer( | MemCell([Char] | ))
                

proc run =
    | => _strterm0, _strterm1, _strterm2-> do
        plug 
            distributor( | => mem,  _strterm0, _strterm1, _strterm2)
            memCell( "a" | mem => )
-}

{-
proc run =
    | => _strterm0, _strterm1, _strterm2, _strterm3 -> do
        plug 
            p1( | passer3, passer0 => mem, _strterm0)
            p2( |                  => passer0, passer1, _strterm1)
            p3( | passer1          => passer2, _strterm2)
            p4( | passer2          => passer3, _strterm3)
            memCell( "a" | mem => )
-}
