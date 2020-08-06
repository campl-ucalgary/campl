{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.ProcessErrorsSpec ( spec ) where

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
    mapM_ (describeAnyFailures (has (_TieDefnProcessError % _ForkNonDisjointChannels)) "ForkNonDisjointChannels")
        [ test1
        ]

--------------------
-- Assertion helpers
--------------------

--------------------
-- Tests
--------------------

test1 = [r|
proc test1 = 
    | inn => out  -> do  
        fork out as
            a -> do
                fork a as
                    a -> do
                        get n on a
                        close inn
                        split a into a,b
                        close b
                        halt a
                    b ->
                        halt b
            b -> do
                close inn
                halt b
|]


