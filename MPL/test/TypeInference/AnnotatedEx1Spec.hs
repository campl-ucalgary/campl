{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.AnnotatedEx1Spec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

import MPLAST.MPLASTTranslate
import MPLAST.MPLASTTranslateErrors
import MPLAST.MPLTypeAST
import MPLAST.MPLProgI
import MPLAST.MPLASTCore
import MPLAST.MPLProg
import MPLAST.MPLProgGraph
import MPLCompile

import Language.AbsMPL
import Language.LayoutMPL
import Language.ParMPL
import Language.ErrM

import qualified Data.List.NonEmpty as NE

import Data.Either
import Data.Traversable

import TypeInference.GraphAssertions 

-- FUTURE TESTS:
-- Boolean satisfibility

-- Test cases to see if the annotated programs do indeed
-- type check...


spec :: Spec
spec = do
    mapM_ (`describeValidGraph` const (return ()))
        [ test1 
        , test2
        , test3
        , test4
        , test5
        , test6
        , test7
        , test8
        , test9
        , test10
        , test11
        , test12
        , test13
        , test14
        , test15
        , test16
        , test17
        , test18
        , test19
        , test20
        , test21
        , test22
        , test23
        , test24
        , test25
        , test26
        , test27
        , test28
        , test29
        , test30
        , test31
        , test32
        , test33
        , test34
        , test35
        , test36
        , test37
        , test38
        , test39
        , test40
        ]


-- Mutually recursvie data type (stolen from Prashant's thesis)
test1 = [r| 
data 
    Zig -> Z =
        Zig  :: Z,X -> Z
        Zigg ::    -> Z
    and
    Zag -> X =
        Zag :: X, Z -> X
        Zagg ::     -> X

fun test :: Zig, Zag -> Zig =
    a,b -> Zig(a, b)
|]

test1Err = [r| 
data 
    Zig -> Z =
        Zig  :: Z,X -> Z
        Zigg ::    -> Z
    and
    Zag -> X =
        Zag :: X, Z -> X
        Zagg ::     -> X

fun test :: Zig, Zig -> Zig =
    a,b -> Zig(a, b)
|]

-- silly mutually recursive tests...
test2 = [r| 
defn
    fun orange :: A -> B = 
        b -> tomato(b)
    fun tomato = 
        a -> orange(a)
|]

test3 = [r| 
defn
    fun tomato = 
        a -> orange(a)
    fun orange :: A -> B = 
        b -> tomato(b)
|]

test4 = [r| 
defn
    fun tomato :: B -> A= 
        a -> orange(a)
    fun orange :: A -> B = 
        b -> tomato(b)
|]

test5 = [r| 
defn
    fun tomato = 
        a -> orange(a)
    fun orange :: A -> A = 
        b -> tomato(b)
|]
test6 = [r| 
defn
    fun orange :: A -> A = 
        b -> tomato(b)
    fun tomato = 
        a -> orange(a)
|]

test7 = [r| 
defn
    fun tomato :: B -> B= 
        a -> orange(a)
    fun orange :: A -> A = 
        b -> tomato(b)
|]




-- classic append example
test8 = [r| 
data
    List(A) -> C =
        Cons :: A,C -> C
        Nil  :: -> C

fun append :: List(A), List(A) -> List(A) =
    Nil,list2 -> list2
    Cons(list1head,list1rest),list2 -> 
        Cons(list1head, append(list1rest,list2))
|]

-- Type checking codata in patterns..
test9 = [r| 
codata 
    S -> Tuple(A,B,C) =
        P1 :: S -> A
        P2 :: S -> B
        P3 :: S -> C
data 
    Unit -> C =
        Unit :: -> C
    
fun tomato :: Tuple(Tuple(A,B,C),B,C) -> A = 
    (P1 := (P1 := a, P2 := b, P3 := c), P2 := _, P3 := _) -> a
|]

test10 = [r| 
codata 
    S -> Function(Arg, Output) =
        Apply :: Arg,S -> Output

fun tomato :: Function(Arg,Output), Arg -> Output = 
    (Apply := app), arg -> app(arg)
    (Apply := app), arg -> app(arg)
|]

-- Type checking building co data (for the const function)..
test11 = [r| 
codata 
    S -> Function(Arg, Output) =
        Apply :: Arg,S -> Output

fun tomato :: A -> Function(B, A) = 
    b -> (Apply := a -> b)
|]

test12 = [r| 
codata 
    S -> Triple(A,B,C) =
        P1 :: S -> A
        P2 :: S -> B
        P3 :: S -> C

fun tomato :: A -> Triple(A,A,A) = 
    a -> (P1 := -> a, P2 := -> a, P3 := -> a)
|]

test13 = [r| 
codata 
    S -> Triple(A,B,C) =
        P1 :: S -> A
        P2 :: S -> B
        P3 :: S -> C

fun tomato :: A,B,C -> Triple(C,B,A) = 
    a,b,c -> (P1 := -> c, P2 := -> b, P3 := -> a)
|]

test14 = [r| 
codata
    S -> Function(Arg, Output) = 
        Apply :: Arg, S -> Output

data Unit -> S =
    Unit :: -> S

fun tomato :: Output -> Function(Unit, Output) =
    outt -> (Apply := Unit -> outt )
|]

test15 = [r| 
codata
    S -> Function(Arg, Output) = 
        Apply :: Arg, S -> Output

codata 
    S -> Triple(A,B,C) =
        P1 :: S -> A
        P2 :: S -> B
        P3 :: S -> C

data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data Unit -> S =
    Unit :: -> S

fun tomato :: -> Function(Triple(Nat,Unit,Nat),Nat) =
    -> (Apply := (P1 := Succ(a), P2 := Unit, P3 := Zero) -> a )
|]

test16 = [r| 
codata
    S -> Function(Arg, Output) = 
        Apply :: Arg, S -> Output

data Unit -> S =
    Unit :: -> S

fun tomato :: Output -> Output =
    outt -> Apply(Unit, (Apply := Unit -> outt ) )
|]

test17 = [r| 
codata 
    S -> Stream(A) =
        Head :: S -> A
        Tail :: S -> S

data 
    Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

fun genInfiniteNats :: Nat -> Stream(Nat)=
    n -> (Head := -> n, Tail := -> genInfiniteNats(Succ(n)) )
|]

-- testing a fold
test18 = [r| 
data 
    Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

fun addNat :: Nat, Nat -> Nat =
    a,b -> fold a of
        Succ : a -> Succ(a)
        Zero : -> b
|]

-- A strange test just for testing nesting folds
test19 = [r|
data 
    Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

data 
    Odds -> S =
        SuccSucc :: S -> S
        One ::   -> S

fun addNatAndOdds :: Nat, Odds -> Nat =
    a,b -> fold a of
        Succ : a -> Succ(a)
        Zero : -> fold b of
            SuccSucc : b -> Succ(Succ(b))
            One : -> Succ(Zero)
|]

-- A mutually recursive fold test..
test20 = [r|
data 
    Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

data 
    Odds -> S =
        SuccSucc :: S -> S
        One ::   -> S

data
    Tree(A) -> S =
        Branch :: A, T -> S
    and 
    Forest(A) -> T =
        Cons :: S,T -> T
        Nil  ::     -> T

fun addNatAndOdds :: Nat, Odds -> Nat =
    a,b -> fold a of
        Succ : a -> Succ(a)
        Zero : -> fold b of
            SuccSucc : b -> Succ(Succ(b))
            One : -> Succ(Zero)

fun addOddsAndNats ::  Nat, Odds -> Odds = 
    a,b -> fold a of
        Succ : a -> SuccSucc(a)
        Zero : -> b

fun strangeFoldTreeSum :: Tree(Nat) -> Nat =
    n -> fold n of
        Branch : a,b -> addNatAndOdds(a,b)

        Cons : a,b -> addOddsAndNats(a,b)
        Nil : -> One

|]

-- Scott's bottom
test21 = [r| 
fun scottsbottom :: -> A =
    -> scottsbottom
|]

-- Note: 
-- This test used to puzzingly fail to compile
-- because it scotts bottom is really of type
-- "-> A" ( NOTE THE ARROW "->")
-- To fix this, we added an arrow simplification
-- step inbetween stages of unification... so 
-- this "-> A" simplifies to "A"
test22 = [r| 
fun scottsbottom =
    -> scottsbottom
|]

-- unfold co data test..
test23 = [r| 
codata
    S -> Triple(A,B,C) =
        P1 :: S -> A
        P2 :: S -> B
        P3 :: S -> C

fun myTriple :: S -> Triple(S,S,S)= 
    a -> unfold a of
        b of
            P1 : -> b
            P2 : -> b
            P3 : -> b
|]

-- test case stolen from Prashant's thesis..
test24 = [r| 
codata
    S -> Stream(A) =
        Head :: S -> A
        Tail :: S -> S
data  
    Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

fun myInfList :: -> Stream(Nat) = 
    -> unfold Zero of
        n of
            Head : -> n
            Tail : -> Succ(n)
|]

-- test case stolen from Prashant's thesis testing 
-- type changing mutually recursive unfolds
test25 = [r| 
codata
    S -> Zig(A,B) =
        HeadA :: S -> A
        TailA :: S -> T
    and
    T -> Zag(A,B) =
        HeadB :: T -> B
        TailB :: T -> S

codata 
    S -> Tuple(A,B) =
        P1 :: S -> A
        P2 :: S -> B

data  
    Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

data  
    NegativeNat -> S =
        Pred :: S -> S
        NZero ::   -> S

fun myInfZigZag1 :: -> Zig(Nat, NegativeNat) = 
    -> unfold (P1 := -> Zero, P2 := -> NZero) of
        (P1 := p, P2 := n) of
            HeadA : -> p
            TailA : -> (P1 := -> n, P2 := -> Succ(p))
        (P1 := n, P2 := p) of
            HeadB : -> n
            TailB : -> (P1 := -> p, P2 := -> Pred(n))

fun myInfZigZag2 :: -> Zag(Nat, NegativeNat) = 
    -> unfold (P1 := -> NZero, P2 := -> Zero) of
        (P1 := n, P2 := p) of
            HeadB : -> n
            TailB : -> (P1 := -> p, P2 := -> Pred(n))
        (P1 := p, P2 := n) of
            HeadA : -> p
            TailA : -> (P1 := -> n, P2 := -> Succ(p))
|]

-- Higher order data tests..
test26 = [r|
data
    HigherOrder(A,B) -> C =
        Higher :: A,B -> C

fun genericHigherOrder :: A(B,C) -> A(B,C) =
    a -> a

fun callHigher :: HigherOrder(A,B) -> HigherOrder(B,A) =
    Higher(a,b) -> genericHigherOrder(Higher(b,a))
|]

test27 = [r|
data
    Fix(F) -> C =
        Fix :: F(C) -> C

fun test :: Fix(A) -> A(Fix(A)) =
    Fix(a) -> a
|]

-- Classic list paramerterized by a fixed point
-- data type.. Note that since we cannot partially
-- apply types e.g. If we have a tuple "Tuple(A,B)",
-- we cannot have "Tuple(A)" and use "Tuple(A)" as a 
-- partially applied function..
test28 = [r| 
defn 
    data
        Fix(F) -> S =
            Fix :: F(S) -> S

    data Nat -> S =
            Succ :: S -> S
            Zero ::   -> S

    data 
        ListF(F) -> S =
            ConsF  :: Nat,F -> S
            NilF   ::    -> S

fun add :: Nat,Nat -> Nat =
    a,b -> fold a of
        Succ : a -> Succ(a)
        Zero :   -> b

fun sum :: Fix(ListF) -> Nat =
    Fix(ConsF(a,b)) -> add(a, sum(b))
    Fix(NilF)       -> Zero

|]

-- Basic get/halt/close/put tests..
test29 = [r| 
proc testing :: | => Get(A | TopBot) =
    | => out -> do  
        get n on out
        halt out
|]

test30 = [r| 
proc testing :: | => TopBot =
    | => out -> do  
        halt out
|]

test31 = [r| 
data 
    Unit -> S =
        Unit :: -> S

proc testing :: | => Get (A | Get (B | Get (Unit | TopBot ))) = 
    | => out -> do  
        get a on out
        get b on out
        get Unit on out
        halt out
|]

test32 = [r| 
proc testing :: | Put(A | TopBot) => =
    | inch => -> do  
        get n on inch
        halt inch
|]

test33 = [r| 
proc testing :: | TopBot => =
    | inch => -> do  
        halt inch
|]

-- pattern matching against a unit type gives us a unit
test34 = [r| 
data 
    Unit -> S =
        Unit :: -> S

proc testing :: | Put (A | Put (B | Put (Unit | TopBot ))) => = 
    | inch => -> do  
        get a on inch
        get b on inch
        get Unit on inch
        halt inch
|]

test35 = [r| 
proc test35 :: A |  => Put (A | TopBot) = 
    a | => outch  -> do  
        put a on outch
        halt outch
|]

test36 = [r|
data 
    Wrapper(A)-> S =
        Wrapper :: A -> S

proc test36 :: Wrapper(A) | => Put(A|TopBot) =
    Wrapper(a) | => out -> do  
        put a on out
        halt out
|]

test37 = [r|
proc test37 :: A | => Put(A | Get(B | Put(B | TopBot))) =
    a | => out -> do  
        put a on out
        get b on out
        put b on out
        halt out
|]

test38 = [r|
proc test38 :: A | Get(A | Put(B | Get(B | TopBot))) =>  =
    a | inch => -> do  
        put a on inch
        get b on inch
        put b on inch
        halt inch
|]

test39 = [r|
proc test39 :: | TopBot => Get(A | TopBot)=
    | inn => out -> do  
        get a on out
        close inn
        halt out
|]

test40 = [r|

|]
