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

{-
proc v12a =
    |  => a -> do
        plug
            => a,c -> do
                close a
                halt c
            c => b -> do
                close b
                halt c
            b => -> do
                halt b
-}
proc v30 =
    |  => -> do
        plug
            f => -> do
                halt f
            a => b,c -> do
                close a 
                close b 
                halt c
            b,c => d,e,f -> do
                close b 
                close c
                close d
                close e
                halt f
            e,d => -> do
                close d
                halt e
{- valid
proc v30 =
    |  => -> do
        plug
            a => b,c -> do
                close a 
                close b 
                halt c
            b,c => d,e,f -> do
                close b 
                close c
                close d
                close e
                halt f
            e,d => -> do
                close d
                halt e
            f => -> do
                halt f
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
