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

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol Passer( | M ) => S =
    Passer :: M (+) (Neg(M) (*) S)  => S

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
        MemCls -> do
            halt ch
        
-- switching the order of @p1@ and @p2@ fixed the deadlock
-- probably because Im messing up the calling convention

proc p1 :: |  Passer( | MemCell([Char]|)) => MemCell([Char]| ), StringTerminal =
    | passer => mem, _strterm -> hcase passer of 
        Passer -> do 
            hput MemGet on mem
            get inp on mem

            hput StringTerminalPut on _strterm
            put inp on _strterm

            hput StringTerminalGet on _strterm
            get ninp on _strterm

            hput MemPut on mem
            put ninp on mem

            fork passer as 
                mmem with mem -> mmem |=| mem
                negmemandnpasser with _strterm -> do
                    split negmemandnpasser into negmem, npasser
                    plug 
                        p1( | npasser => z, _strterm)
                        z, negmem => -> negmem |=| neg z


proc p2 :: |   => Passer( | MemCell([Char]|)), StringTerminal =
    | => passer, _strterm -> do
        hput Passer on passer
        split passer into mem, negmemandnpasser

        hput MemGet on mem
        get inp on mem

        hput StringTerminalPut on _strterm
        put inp on _strterm

        hput StringTerminalGet on _strterm
        get ninp on _strterm

        hput MemPut on mem
        put ninp on mem

        fork negmemandnpasser as 
            negmem with mem -> negmem |=| neg mem 
            npasser with _strterm -> p2( | => npasser, _strterm )
                    
proc run =
    | => _strterm0, _strterm1 -> do
        plug 
            p2( |        => passer, _strterm0)
            p1( | passer => mem, _strterm1)
            memCell( "a" | mem => )

