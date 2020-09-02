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


{-
protocol Test => S =
    Testing :: TopBot => S

proc v12a  =
    | => b -> do
        get a on b 
        halt b
        -}

proc no0 =
    | b => -> do
        get a on b
        no0(| b => )

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
