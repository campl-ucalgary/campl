# Once again tracing this thing...
Recall the code is as follows

```
protocol IntTerminal => C =
    IntTerminalGet :: Get(Int | C) => C
    IntTerminalPut :: Put(Int | C) => C
    IntTerminalClose :: TopBot => C

protocol Mem(M|) => S =
    MemPut :: Put(M|S) => S
    MemGet :: Get(M|S) => S
    MemCls :: TopBot => S

protocol Passer(|P) => S =
    Passer :: P (+) (Neg(P) (*) S) => S

proc memory :: A | Mem(A|) => =
    x | ch => -> do
        hcase ch of
            MemPut -> do
                get y on ch
                memory(y | ch => )
            MemGet -> do
                put x on ch
                memory(x | ch => )
            MemCls -> do
                halt ch

proc p1 :: | => Passer(|Mem(Int|)), IntTerminal = 
    | => passer, inp -> do
        hput Passer on passer
        split passer into mm,nmpp
        hput MemGet on mm 
        get y on mm
        hput IntTerminalPut on inp
        put y on inp
        hput IntTerminalGet on inp
        get x on inp
        hput MemPut on mm
        put x on mm
        fork nmpp as
            nm with mm -> nm |=| neg mm
            pp with inp -> p1(| => pp, inp)

proc p2 :: | Passer(| Mem(Int|)) => IntTerminal, Mem(Int|) =
    | passer => inp, mem -> do
        hcase passer of
            Passer -> do
                hput MemGet on mem
                get y on mem
                hput IntTerminalPut on inp
                put y on inp
                hput IntTerminalGet on inp
                get x on inp
                hput MemPut on mem
                put x on mem
                fork passer as
                    mm with mem -> do
                        mm |=| mem
                    nmpp with inp -> do
                        split nmpp into nm,pp
                        plug
                            p2( | pp => inp,z)
                            z,nm => -> z |=| neg nm

proc run :: | => IntTerminal , IntTerminal =
    | => _inpterm0, _inpterm1 -> do
        plug 
            p1(| => passer, _inpterm0)
            p2(| passer => _inpterm1, mem)
            memory(100 | mem => )
```
We're literally going to hand evaluate everything until we really are at a deadlock...

# The machine instructions as given in the document
First, we execute ``run'' and open up the service channels.
So, the entire system state is:

```
-- Channel manager
( _inpterm0,  
    []
    |
    []
)

( _inpterm1,  
    []
    |
    []
)

-- Global translation
_inpterm0 ---> _inpterm0 
_inpterm1 ---> _inpterm1 

-- Running processes:
proc run :: | => IntTerminal , IntTerminal =
    | => _inpterm0, _inpterm1 -> do
        plug 
            p1(| => passer, _inpterm0)
            p2(| passer => _inpterm1, mem)
            memory(100 | mem => )
_inpterm0 --> _inpterm0 
_inpterm1 --> _inpterm1 
```
Then, we open up the channels in the ``plug'' command in ``run''
```
-- Channel manager
( _inpterm0,  
    []
    |
    []
)

( _inpterm1,  
    []
    |
    []
)
( passer,
    []
    |
    []
)

-- Global translation
_inpterm0 ---> _inpterm0 
_inpterm1 ---> _inpterm1 
passer    ---> passer 
mem       ---> mem

-- Running processes:
proc run :: | => IntTerminal , IntTerminal =
    | => _inpterm0, _inpterm1 -> do
        plug 
            p1(| => passer, _inpterm0)
            p2(| passer => _inpterm1, mem)
            memory(100 | mem => )
_inpterm0 --> _inpterm0 
_inpterm1 --> _inpterm1 
```

and we then 
