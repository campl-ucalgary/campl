{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MPLPasses.UnificationSpec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import MPLCompile
import MPLPasses.TieTypeClause
import MPLPasses.Unification
import MPLPasses.UnificationErrors

import MPLAST.MPLProgGraph
import MPLAST.MPLTypeAST
import MPLAST.MPLASTCore

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

import qualified Data.Set as Set
import Control.Arrow

unsafeSolveTypeEq :: TypeEqns TaggedBnfcIdent TypeTag -> Package TaggedBnfcIdent TypeTag
unsafeSolveTypeEq n = case solveTypeEq n of
    Right pkg -> pkg
    Left err -> error $ show (err :: UnificationError)

spec :: Spec
spec = do
    describe "Thesis examples.."  $ do
        it "Exists x1,x2. x0 == List(x1), x1 == x2, Exists x3,x4. x3 == List(x4), x2 == x3" $ do
            uncurry (assertEqual "Solution:") (second unsafeSolveTypeEq thesisEx)

-- thesis example...
thesisEx = (thesisExSol, thesisExEq)
  where
    thesisExSol = emptyPackage 
        & packageExisVar  .~ Set.fromList [x4]
        & packageFreeVars .~ Set.fromList [x0]
        & packageSubs     .~ [(x0, TypeWithArgs list TypeClauseLeaf [TypeWithArgs list TypeClauseLeaf [TypeVar x4]])]
    
    thesisExEq = TypeEqnsExist [x1, x2] [x0IsListx1, x1isx2, TypeEqnsExist [x3,x4] [x3IsListx4, x2isx3]]
    
    x0 = TypeTag $ UniqueTag 0
    x1 = TypeTag $ UniqueTag 1
    x2 = TypeTag $ UniqueTag 2
    x3 = TypeTag $ UniqueTag 3
    x4 = TypeTag $ UniqueTag 4
    
    x0IsListx1 = TypeEqnsEq 
        ( TypeVar x0
        , TypeWithArgs 
            list
            TypeClauseLeaf
            [TypeVar x1]
        )
    x1isx2 = TypeEqnsEq (TypeVar x1,TypeVar x2)
    
    x3IsListx4 = TypeEqnsEq 
        ( TypeVar x3
        , TypeWithArgs list TypeClauseLeaf [TypeVar x4])
    
    x2isx3 = TypeEqnsEq
        ( TypeVar x2
        , TypeVar x3 )
    
    list = TaggedBnfcIdent (BnfcIdent ("List", (-1,-1))) (UniqueTag 5)
