{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.HigherKindMatchFailureSpec ( spec ) where

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

-- tests for match failure with higher kinded data

spec :: Spec
spec = do
    mapM_ describeMatchFailure
        [ test1 
        , test2 
        , test3 
        ]

-- TODO, we can get better error messages here
-- like arity mismatch instead..
test1 = [r| 
data 
    Zig(A,B) -> Z =
        Zig :: A,B -> Z

fun test :: Zig -> Zig =
    a,b -> Zig(a, b)
|]


test2 = [r| 
data 
    Unit -> C =
        Unit :: -> C

fun test :: A(B,C) -> A(B,C) =
    a -> a

fun fail =
    a -> test(Unit)
|]

test3 = [r| 
data 
    Orange -> C =
        Orange :: -> C
data 
    Kartofler -> C =
        Kartofler :: -> C
data 
    HigherKind(A,B) -> C =
        HigherKind :: A,B -> C

fun test :: A(B,B) -> A(B,B) =
    a -> a

fun fail =
    a -> test(HigherKind(Orange, Kartofler))
|]
