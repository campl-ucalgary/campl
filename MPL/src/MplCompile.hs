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

codata S -> Tuple(A,B) =
    P0 :: S -> A
    P1 :: S -> B

codata S -> Fun(A,B) =
    App :: A,S -> B

fun prj0 :: Fun(A,B), A -> B=
    (App := f), a -> f(a)

fun undefined :: -> A =
    -> undefined



{- TODO
fun foldtest =
    a -> fold a of
        Succ : b -> Succ(b)
        Zero : -> b
-}



protocol Test(A| ) => S =
    Testing0 :: Get(A | S) => S 

proc v12a :: | => Neg(A), A =
    |  => a,b -> do
        a |=| neg b

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
