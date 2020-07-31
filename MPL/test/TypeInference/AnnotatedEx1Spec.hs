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
        ]

    mapM_ describeForallMatchFailure
        [ testfail1
        , testfail2 ]


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

testfail1 = [r| 
defn
    fun tomato :: B -> B= 
        a -> orange(a)
    fun orange :: A -> B = 
        b -> tomato(b)
|]


testfail2 = [r| 
defn
    fun orange :: A -> B = 
        b -> tomato(b)
    fun tomato :: B -> B= 
        a -> orange(a)
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
