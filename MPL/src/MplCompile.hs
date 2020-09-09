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

data Wrapper(A) -> S =
    Wrapper :: A -> S

-- fun testing =
    -- Succ(a) -> Succ(Succ(a))
    -- a -> a


data Wrapper(A) -> S =
    Wrapper :: A -> S

data Unit -> S =
    Unit :: -> S


data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data NegNat -> S =
    Pred :: S -> S
    NZero ::   -> S

codata S -> Tuple(A,B) =
    P1 :: S -> A
    P2 :: S -> B

codata 
    S -> Zig(A,B) =
        HeadA :: S -> A
        TailA :: S -> T
    and 
    T -> Zag(A,B) =
        HeadB :: T -> B
        TailB :: T -> S
        

fun unfoldtest =
    -> unfold (P1 := -> NZero, P2 := -> Zero) of
        (P1 := n, P2 := i) of
            HeadA : -> n
            TailA : -> (P1 := -> i, P2 := -> Pred(n))
        (P1 := i, P2 := n) of
            HeadB : -> i
            TailB : -> (P1 := -> n, P2 := -> Succ(i))

|]



{-
-- add this test in later
codata S -> Potato(A) =
    Huh :: A,S -> S

fun myfun :: A,A -> A =
    Huh(a),b -> b
    -}


-- runPassesTester
--
