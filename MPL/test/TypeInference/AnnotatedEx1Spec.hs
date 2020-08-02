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


spec :: Spec
spec = do
    mapM_ (`describeValidGraph` const (return ()) )
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
        ]


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

test21 = [r| 
fun scottsbottom :: -> A =
    -> scottsbottom
|]

-- Note: 
-- This test used to puzzingly fail to compile
-- because it scotts bottom is really of type
-- -> A -- to fix this, we added an arrow simplification
-- step inbetween stages of unification...
test22 = [r| 
fun scottsbottom =
    -> scottsbottom
|]

test23 = [r| 
|]
