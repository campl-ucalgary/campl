{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.UnannotatedNatNatToNatExSpec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import TypeInference.GraphAssertions

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


spec :: Spec
spec = do
    mapM_ natNatToNatTester
        [ natNatToNatTest1
        , natNatToNatTest2
        , natNatToNatTest3
        , natNatToNatTest4
        , natNatToNatTest5
        , natNatToNatTest6
        , natNatToNatTest7
        , natNatToNatTest8
        , natNatToNatTest9
        , natNatToNatTest10
        -- , natNatToNatTest11
        ]
    {-
    describe test1  $ do
        it "TODO" $ do
            assertEqual "" True
            -}

--------------------
-- Valid test cases...
--------------------
natNatToNatTester test = describeValidGraph test $ \prog -> do
    let natclause = view (clauseGraphSpine % to (head . NE.toList)) 
            $ head 
            $ progGQueryTypeClausesGraphs prog
        nattype = TypeWithArgs (natclause ^. typeClauseName) (TypeClauseCallDefKnot natclause) []
        functiontesttype = view funTypesFromTo
            $ head 
            $ progGQueryFunctions prog
    it "functiontest should have type Nat,Nat -> Nat" $ do
        assertBool ("Actual type: " ++ pprint functiontesttype) 
            (TypeSeq (TypeSeqArrF [nattype, nattype] nattype)
             == functiontesttype)
natNatToNatTest1 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        Nat(Nat(a)),Zero -> Zero
|]

natNatToNatTest2 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        Nat(a),Zero -> a
|]

natNatToNatTest3 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        Nat(a),Zero -> case a of
            b -> b
|]

natNatToNatTest4 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        Nat(a),Zero -> case a of
            b -> a
|]

natNatToNatTest5 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        Nat(a),Zero -> case a of
            b -> case b of
                c -> c
|]

natNatToNatTest6 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        Nat(a),Zero -> case a of
            Nat(b) -> case b of
                Zero -> a
                Nat(c) -> c
            Zero -> a
|]

natNatToNatTest7 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        a,Zero -> Zero
        a,Zero -> a
|]

natNatToNatTest8 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        a,Zero -> Nat(a)
        a,Zero -> a
|]

natNatToNatTest9 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        a,Zero -> Zero
        Zero,a -> Zero
|]

natNatToNatTest10 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        a,b -> case a of
            s -> Nat(s)
            Zero -> b
|]

{-
natNatToNatTest11 = [r|
data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        a,b -> 
            let
                fun natnattest =
                    Zero,b -> b
                    a,b -> a
            in natnattest(a,b)
|]
-}
