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


spec :: Spec
spec = do
    mapM_ describeForallMatchFailure
        [ test1 ]

test1 = [r| 
fun test :: B -> C =
    a -> a
|]


test1Err = [r| 
data 
    Zig(A,B) -> Z =
        Zig :: A,B -> Z

data 
    Kartofler -> C =
        Kartofler :: -> C

data 
    Orange -> C =
        Orange :: -> C

fun test :: Zig -> Zig =
    a,b -> Zig(a, b)
|]
