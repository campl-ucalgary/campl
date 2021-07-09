

proc negtest :: | A,Neg(A) => =
    | ch0,ch1 => -> do
        ch0 |=| neg ch1 

proc negtest :: | Neg(A),A => =
    | ch0,ch1 => -> do
        ch1 |=| neg ch0 



proc negtest :: |  =>A,Neg(A) =
    |  => ch0,ch1 -> do
        ch0 |=| neg ch1 

proc negtest :: |  => Neg(A),A =
    |  => ch0,ch1 -> do 
        ch1 |=| neg ch0 
