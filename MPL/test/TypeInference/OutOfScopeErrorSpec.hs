{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.OutOfScopeErrorSpec ( spec ) where

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

spec :: Spec
spec = do
    mapM_ describeOutOfScope
        [ outOfScopeTest1
        , outOfScopeTest2
        , outOfScopeTest3
        , outOfScopeTest4
        , outOfScopeTest5
        ]

--------------------
-- Assertion helpers
--------------------

describeOutOfScope prog = do
    describe ("Testing for out of scope error:\n" ++ prog) $ do
        prog' <- runIO $ unsafeTranslateParseLexGraph prog

        it "Testing for out of scope error." $ do
            case prog' of
                Right _ -> assertFailure "Program is valid when it should not be..."
                Left errs -> assertBool
                    ("Expected out of scope errors but got: " ++ show errs)
                    (allOf folded (has _NotInScope) errs)

--------------------
-- Tests
--------------------

outOfScopeTest1 = [r|
fun foo =
    a -> b
|]

outOfScopeTest2 = [r|
defn
    fun foo = 
        a -> a
where
    data
        Unit -> C = 
            Unit :: -> C

fun bar =
    Unit -> Unit
|]

outOfScopeTest3 = [r|
defn
    fun foo = 
        a -> case a of
            b -> c
|]

outOfScopeTest4 = [r|
data
    Cabbage -> C =
        Cabbage :: Potato -> C

data Potato -> D =
    Potato :: -> D
|]

outOfScopeTest5 = [r|
defn
    fun foo = 
        a -> case a of
            b -> b
            c -> b
|]
