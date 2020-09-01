{-# LANGUAGE QuasiQuotes #-}
module MplPasses.TypeChecker.TypeCheckSpec ( spec ) where

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
        [ v1 
        , v2
        , v3 
        , v4 
        , v5 
        , v6 
        , v7 
        , v8 
        , v9 
        , v10 
        , v11 
        , v12 
        , v13 
        , v14 
        ]

    mapM_ (`describeAnyErrors` ("Type unification for all failure", 
            _MplTypeCheckErrors 
            % _TypeCheckUnificationErrors 
            % _TypeForallMatchFailure))
        [ nf1
        , nf2
        , nf3
        ]

    mapM_ (`describeAnyErrors` ("Type match failure", 
            _MplTypeCheckErrors 
            % _TypeCheckUnificationErrors 
            % _TypeMatchFailure))
        [ nm1
        , nm2 
        , nm3
        , nm4 ]

    mapM_ (`describeAnyErrors` ("Type match failure", 
            _MplTypeCheckErrors 
            % _TypeCheckUnificationErrors 
            % _TypeOccursCheck))
        [ no0 ]

-- Valid tests  
----------------------------
v1 = [r|
fun v1 :: A -> A =
    a -> a
|]

v2 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun v2 :: Nat() -> Nat() =
    Succ(a) -> a
|]

v3 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun v3 :: Nat() -> Nat() =
    Succ(a) -> case a of
        b -> b
|]

v4 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun v4 :: Nat() -> Nat() =
    a -> Succ(a)
|]

v5 = [r|
data List(A) -> S =
    Cons :: A,S -> S
    Nil ::   -> S

fun v5 :: A,List(A) -> List(A) =
    a,b -> Cons(a,b)
|]

v6 = [r|
data List(A) -> S =
    Cons :: A,S -> S
    Nil ::   -> S

fun v6 :: List(A), List(List(A)) -> List(List(A)) =
    a,b -> Cons(a,b)
    Cons(a,b),c -> Nil
|]

v7 = [r|
fun v7 :: A -> B =
    a -> v7(a)
|]
        
v8 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun v8 :: A -> Nat() =
    a -> Succ(v8(a))
|]

v9 = [r|
defn
    fun fun0 :: A -> B =
        a -> fun1(a)
    fun fun1 :: B -> A =
        a -> fun0(a)
|]


v10 = [r|
codata S -> Fun(A,B) =
    App :: A, S -> B

fun myConst :: A -> Fun(B,B)  =
    a -> (App := b -> b)
|]

v11 = [r|
proc v11 :: | A => =
    | b => -> v11(| b => )
|]

v12 = [r|
defn
    proc v12a :: | Put(A | TopBot) =>  =
        | b => -> do
            get a on b 
            v12b( a | b => )
    proc v12b :: A | TopBot =>  =
        a | b => -> do
            halt b
|]

v13 = [r|
proc v13 :: | => Get(A|Get(A|TopBot)) =
    | => b -> do
        get a on b 
        get a on b
        halt b
|]

v14 =[r|
proc v14 :: | => Get(A|Put(A|TopBot)) =
    | => b -> do
        get a on b 
        put a on b
        halt b
|]

v15 =[r|
proc v15 :: | Put(A|Get(A|TopBot)) =>  =
    | b => -> do
        get a on b 
        put a on b
        halt b
|]

-- Invalid tests  
----------------------------
nf1 = [r|
fun nf1 :: A -> B =
    a -> a
|]

nf2 = [r|
fun nf1 :: B -> A =
    a -> a
|]

nf3 = [r|
defn
    fun fun0 :: A -> B =
        a -> fun1(a)
    fun fun1 :: B -> B =
        a -> fun0(a)
|]

nm1 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data NegNat -> S =
    Pred :: S -> S
    Zero ::   -> S

fun nm1 =
    Succ(a) -> a
    Pred(a) -> a
        
|]

nm2 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data NegNat -> S =
    Pred :: S -> S
    Zero ::   -> S

fun nm2 =
    Succ(a) -> case a of
        Pred(b) -> b
        
|]

nm3 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S
data NegNat -> S =
    Pred :: S -> S
    NZero ::   -> S

codata S -> Fun(A,B) =
    App1 :: A, S -> B
    App2 :: A,A,S -> B

fun testing =
    a -> (App1 := c -> Zero, App2 := a,b-> NZero)
|]

nm4 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data NegNat -> S =
    Pred :: S -> S
    NZero ::   -> S

proc myproc =
    Succ(a) | b => c -> myproc(a | b => c)
    _ | b => c -> myproc(NZero | b => c)
|]

no0 = [r|
proc myproc =
    | b => -> do
        get a on b
        myproc(| b => )
|]
