{-# LANGUAGE QuasiQuotes #-}
module MplCompile where

import Text.RawString.QQ

import MplPasses.Parser.Parse
import MplPasses.Parser.BnfcParse

parsebnfc n = runParse' <$> runBnfc n


test = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data List(A) -> S =
    Cons :: A,S -> S
    Nil ::   -> S

data NegNat -> S =
    Pred :: S -> S
    NZero ::   -> S

fun undefined :: -> A=
    -> undefined


protocol Test(A| ) => S =
    Testing0 :: Get(A | S) => S 

proc v12a =
    |  => a,b -> do
        get _ on a
        get _ on b
        race 
            a -> do
                get _ on a
                get _ on b
                close b
                halt a
            b -> do
                get _ on a
                get _ on b
                close a
                halt b




{-
proc v12a  =
    | => b -> do
        get a on b 
        halt b
        -}

{-
proc v11 =
    | b =>a,c -> do
        get _ on a
        put Zero on a
        get _ on a
        close a 
        halt b
        -}
{-
proc no0 =
    | b => -> do
        get a on b
        no0(| b => )
-}

|]



{-
test = [r|
defn
    -- fun myfun :: Nat(),Nat() -> Nat() =
    fun myfun :: Nat() -> Nat() =
        Succ(a) -> a

    data Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

|]
-}

{-
-- add this test in later
codata S -> Potato(A) =
    Huh :: A,S -> S

fun myfun :: A,A -> A =
    Huh(a),b -> b
    -}


-- runPassesTester
--
