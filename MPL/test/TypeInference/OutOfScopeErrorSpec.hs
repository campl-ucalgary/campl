{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.OutOfScopeErrorSpec ( spec ) where

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
import MPLPasses.TieDefnsErrors
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
    mapM_ describeOutOfScope
        [ outOfScopeTest1
        , outOfScopeTest2
        , outOfScopeTest3
        , outOfScopeTest4
        , outOfScopeTest5
        , outOfScopeTest6
        , outOfScopeTest7
        , outOfScopeTest8
        , outOfScopeTest9
        , outOfScopeTest10
        ]

--------------------
-- Assertion helpers
--------------------

--------------------
-- Tests
--------------------

outOfScopeTest1 = [r|
fun foo =
    a -> b
|]

outOfScopeTest2 = [r|
defn
    fun foo = 
        a -> a
where
    data
        Unit -> C = 
            Unit :: -> C

fun bar =
    Unit -> Unit
|]

outOfScopeTest3 = [r|
defn
    fun foo = 
        a -> case a of
            b -> c
|]

outOfScopeTest4 = [r|
data
    Cabbage -> C =
        Cabbage :: Potato -> C

data Potato -> D =
    Potato :: Cabbage -> D
|]

outOfScopeTest5 = [r|
defn
    fun foo = 
        a -> case a of
            b -> b
            c -> b
|]


outOfScopeTest6 = [r|
fun foo = 
    a -> orange(a)
fun testing =
    a -> foo(a)
|]

outOfScopeTest7 = [r|
defn 
    fun foo = 
        a -> orange(a)

    fun orange =
        a -> out(a)

fun bar =
    a -> foo(a,a)
|]

outOfScopeTest8 = [r| 
fun tomato = 
    (Haha := no, CoData := inst) -> inst
|]

outOfScopeTest9 = [r| 
codata 
    S -> Tuple(A,B,C) =
        P1 :: S -> A
        P2 :: S -> B
        P3 :: S -> C

fun tomato = 
    (P1 := (HahaOutOfSCope := a, P2 := b, P3 := c), P2 := _, P3 := _) -> a
|]

outOfScopeTest10 = [r| 
data 
    Nat -> S =
        Succ :: S -> S
        Zero ::   -> S

fun double  = 
    a,b -> fold a of
        AA : a -> Succ(Succ(a))
        ZFF : -> b
|]
