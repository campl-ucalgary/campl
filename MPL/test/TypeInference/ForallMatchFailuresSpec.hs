{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.ForallMatchFailuresSpec ( spec ) where

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
    mapM_ describeForallMatchFailure
        [ testfail1
        , testfail2 
        , testfail3 ]

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

testfail3 = [r| 
codata 
    S -> Triple(A,B,C) =
        P1 :: S -> A
        P2 :: S -> B
        P3 :: S -> C

fun tomato :: A -> Triple(A,B,A) = 
    a -> (P1 := -> a, P2 := -> a, P3 := -> a)
|]
