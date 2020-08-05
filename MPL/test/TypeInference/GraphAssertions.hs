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
        it "Should be a valid program that does NOT throw exceptions" $ do
            case prog' of
                Right prog'' -> do
                    assertBool "" ( mconcat (pprintFunctionTypes prog'') == mconcat (pprintFunctionTypes prog'') )
                    rst prog''
                    return ()

                Left errs -> assertFailure (show errs) >> return ()

describeAllFailures pred expectedmsg prog = do
    describe ("Testing for" ++ expectedmsg ++ " :\n" ++ prog) $ do
        prog' <- runIO $ unsafeTranslateParseLexGraph prog

        it ("Testing for " ++ expectedmsg ++ " error.") $ do
            case prog' of
                Right _ -> assertFailure "Program is valid when it should not be..."
                Left errs -> assertBool
                    ("Expected " ++ expectedmsg ++ " but got " ++ show errs)
                    (allOf folded pred errs)

describeOutOfScope = describeAllFailures 
    (has _NotInScope) "NotInScope"

describeForallMatchFailure = describeAllFailures 
    (has (_TieDefnUnificationError % _ForallMatchFailure))
    "ForallMatchFailure"

describeMatchFailure = describeAllFailures 
    (has (_TieDefnUnificationError % _MatchFailure))
    "MatchFailure"
