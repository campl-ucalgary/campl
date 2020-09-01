{-# LANGUAGE QuasiQuotes #-}
module MplPasses.TypeChecker.NonTypeErrorsSpec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import MplAST.MplCore
import MplPasses.Assertions

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

import Data.Either
import Data.Traversable

import qualified MplPasses.Parser.BnfcParse as B
import MplPasses.Parser.Parse
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R

import MplPasses.TypeChecker.TypeCheck

import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckErrorPkg 
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckErrors 

import MplPasses.Passes
import MplPasses.Env

spec = do
    mapM_ (`describeAnyErrors` ("Illegal expression codata call but got data instead", 
            _MplTypeCheckErrors 
            % _CallErrors 
            % _IllegalExprCodataCallGotDataInstead)
            )
        [ n0
        ]


n0 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun myConst =
    a -> (Succ := b -> b)
|]
