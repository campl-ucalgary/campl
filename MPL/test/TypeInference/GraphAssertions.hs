{-# LANGUAGE QuasiQuotes #-} 
module TypeInference.GraphAssertions where

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
import MPLPasses.Unification
import MPLPasses.UnificationErrors
import MPLPasses.TieDefnsErrors
import MPLCompile

import Language.AbsMPL
import Language.LayoutMPL
import Language.ParMPL
import Language.ErrM

import qualified Data.List.NonEmpty as NE

import Data.Either
import Data.Traversable

describeValidGraph prog rst = do
    describe ("Testing the valid program:\n " ++ prog) $ do
        prog' <- runIO $ unsafeTranslateParseLexGraph prog

        it "should be a valid program" $ do
            case prog' of
                Right prog' -> return ()
                Left errs -> assertFailure (show errs)
        let Right prog'' = prog'
        it "testing if the program's functions (TODO change this for the entire program) does NOT throw exceptions" $ do
            assertBool "" 
                ( mconcat (pprintFunctionTypes prog'') 
                == mconcat (pprintFunctionTypes prog'') )
        rst prog''

describeAllFailures pred prog = do
    describe ("Testing for out of scope error:\n" ++ prog) $ do
        prog' <- runIO $ unsafeTranslateParseLexGraph prog

        it "Testing for out of scope error." $ do
            case prog' of
                Right _ -> assertFailure "Program is valid when it should not be..."
                Left errs -> assertBool
                    ("Expected out of scope errors but got: " ++ show errs)
                    (allOf folded pred errs)


describeOutOfScope = describeAllFailures (has _NotInScope)

describeForallMatchFailure = describeAllFailures 
    (has (_TieDefnUnificationError % _ForallMatchFailure))
