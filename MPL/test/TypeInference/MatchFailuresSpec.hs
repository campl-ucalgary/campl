{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.MatchFailuresSpec ( spec ) where

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
    mapM_ describeMatchFailure
        [ test1
        , test2 
        , test3 
        , test4 
        , test5 
        , test6 
        ]

test1 = [r| 
data
    HigherOrder(A,B) -> C =
        Higher :: A,B -> C
data
    Unit -> C =
        Unit :: -> C

fun genericHigherOrder :: A(B,C) -> A(B,C) =
    a -> a

fun badHigherOrderCall =
    -> genericHigherOrder(Unit)
|]

test2 = [r| 
fun totallyWhack :: A(B,C) -> A(B,C,D) =
    a -> a
|]

test3 = [r| 
data 
    Nat -> S =
        Succ :: S -> S
        Zero :: -> S
protocol 
    Test(A) => S =
        Test :: Get(A |TopBot) => S

proc test3 = 
    | => a -> do  
        hput Test on a
        put Zero on a
        halt a
|]

test4 = [r| 
protocol 
    Test => S =
        Test1 :: TopBot => S
        Test2 :: TopBot => S

proc test4 = 
    | inn => -> do  
        hcase inn of
            Test1 -> do
                halt inn
            Test2 -> do
                get n on inn
                halt inn
|]

test5 = [r|
protocol 
    Test => S =
        Test1 :: TopBot => S
        Test2 :: TopBot => S

proc test5 = 
    | a,inn => -> do  
        hcase inn of
            Test1 -> do
                close a
                halt inn
            Test2 -> do
                get n on a
                close a
                halt inn
|]

test6 = [r|
protocol 
    AA => S =
        AA0 :: TopBot => S
        AA1 :: T => S
    and

    BB => T =
        BB0 :: TopBot => T
    

proc test6 =
    | inn => -> do  
        hcase inn of
            AA0 -> do
                halt inn
            AA1 -> do
                halt inn
|]
