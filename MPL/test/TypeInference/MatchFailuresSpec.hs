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
        , test2 ]

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
