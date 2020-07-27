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
        ]
    {-
    mapM_ describeMatchFailure
        [ test1Err ]
        -}


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
    fun tomato = 
        a -> orange(a)
    fun orange :: A -> B = 
        b -> tomato(b)
|]
