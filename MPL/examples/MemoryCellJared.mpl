protocol IntTerm  => P =
      GetInt   :: Get (Int|P) => P 
      PutInt   :: Put (Int|P) => P
      Close    :: TopBot    => P  

protocol MEM => P =
    PUT :: Put(Int|P) => P
    GET :: Get(Int|P) => P
    CLS :: TopBot => P

coprotocol CP => Console (Int) =
    GetIntC   :: CP => Put (Int|CP)  
    PutIntC   :: CP => Get (Int|CP) 
    CloseC    :: CP => TopBot 
    
protocol Passer(A) => P =
    Pass :: A (+) (Neg(A) (*) P) => P

proc memory :: Int | MEM(Int) => =
    x | ch => -> do
        hcase ch of
            PUT -> do get y on ch
                      memory(y | ch => )
            GET -> do put x on ch
                      memory(x | ch => )
            CLS -> do halt ch

proc p2 :: | Passer(MEM) => IntTerm, MEM =
    | p => in, mem -> do
        hcase p of
            Pass -> do
                    hput GET on mem
                    get y on mem

                    hput PutInt on in
                    put y on in

                    hput GetInt on in
                    get x on in

                    hput PUT on mem
                    put x on mem

                    fork p as
                        mm -> do mm |=| mem
                        nmpp -> do
                                    split nmpp into nm,pp
                                    plug 
                                        p2 (| pp => in, z)  -- z is output polarity
                                        z |=| neg nm        

proc p1 :: | => Passer(MEM), IntTerm =
    | => p, io -> do
        hput Pass on p

        split p into mm,nmpp

        hput GET on mm
        get y on mm

        hput PutInt on io
        put y on io
        hput GetInt on io
        get x on io

        hput PUT on mm
        put x on mm

        fork nmpp as
            nm -> do nm |=| neg mm

            pp -> do p1( | => pp, io)
                    


run :: Console(Int) => IntTerm, IntTerm = 
        console => intTerm1, intTerm2 -> do
            plug 
                p1 (| => ch,intTerm1)
                p2 (| ch => intTerm2, mem)
                memory(10 | mem => )
   
