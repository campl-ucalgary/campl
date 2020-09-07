{-# LANGUAGE QuasiQuotes #-}
module MplPasses.TypeChecker.KindCheckSpec ( spec ) where

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

import MplPasses.Passes
import MplPasses.Env

-- Tests for overlapping declarations and out of scope errors 

spec = do
    mapM_ (`describeValidTypeCheck` const (return ()))
        [ v1 ]

    mapM_ (`describeAnyErrors` ("higher kinded variable failure", 
            _MplTypeCheckErrors 
            % _TypeCheckKindErrors 
            % _KindHigherKindedTypesAreNotAllowed))
        [ nh1
        , nh2 
        , nh3 
        , ns1
        ]

    mapM_ (`describeAnyErrors` ("Kind primitive mismatch", 
            _MplTypeCheckErrors 
            % _TypeCheckKindErrors 
            % _KindPrimtiveMismatchExpectedButGot))
        [ nk1 ]

    mapM_ (`describeAnyErrors` ("Kind seq arity mismatch", 
            _MplTypeCheckErrors 
            % _TypeCheckKindErrors 
            % _KindAritySeqMismatchExpectedButGot))
        [ ns1 ]

-- Valid tests  
----------------------------
v1 = [r|
|]


-- Invalid higher order types..
----------------------------
nh1 = [r|
data 
    Test(A,B) -> C =
        Testt :: A,B -> C
        Testtt :: A(B) -> C
|]

nh2 = [r|
defn 
    data 
        Strange(A,B) -> C =
            StrangeCts :: Test(A) -> C
    data 
        Test(A,B) -> C =
            Testtt :: A(B) -> C
|]

nh3 =[r|
data Nat(A) -> S =
    Succ :: S,A(A) -> S
    Zero ::   -> S

fun myfun :: Nat(A) -> Nat(A) =
    Succ(a), Succ(b) -> a
|]

-- Invalid higher order types..
----------------------------
nk1 = [r|
data 
    Test(A,B) -> C =
        Testtt :: TopBot -> C
|]

-- Kind seqarity
----------------------------
ns1 = [r|
defn 
    data 
        Strange(A,B) -> C =
            StrangeCts :: Test(A) -> C
    data 
        Test(A,B) -> C =
            Testtt :: A(B) -> C
|]
