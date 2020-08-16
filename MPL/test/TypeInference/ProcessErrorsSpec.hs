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
        [ forkdisjoint1
        ]

    mapM_ (describeAnyFailures 
        (has (_TieDefnProcessError % _IllegalLastInstruction)) "IllegalNonInstruction")
        [ nonlast1
        ]


    mapM_ (describeAnyFailures 
        (has (_TieDefnProcessError % _UnclosedChannels)) "UnclosedChannels")
        [ unclosed1
        ]
--------------------
-- Assertion helpers
--------------------

--------------------
-- Tests
--------------------

forkdisjoint1 = [r|
proc forkdisjoint1 = 
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

nonlast1 = [r|
proc nonlast1 =
    | => out -> do
        fork out as
            a -> close a
            b -> close b

|]


unclosed1 = [r|
protocol 
    Test => S =
        Test1 :: TopBot => S
        Test2 :: TopBot => S

proc unclosed1 :: | TopBot, Test => = 
    | a,inn => -> do  
        hcase inn of
            Test1 -> do
                close a
                halt inn
            Test2 -> do
                halt inn
|]
