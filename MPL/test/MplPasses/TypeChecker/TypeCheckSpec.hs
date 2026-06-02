{-# LANGUAGE QuasiQuotes #-}
module MplPasses.TypeChecker.TypeCheckSpec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import MplAST.MplCore
import MplPasses.Assertions
import MplPasses.PassesErrors

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

import Control.Arrow
import System.Directory
import System.FilePath

spec :: SpecWith ()
spec = do
    let casesdir = "test/MplPasses/TypeChecker/cases/typecheck"
    poscases <- runIO $ do 
        let poscasesdir = casesdir </> "melikas"
        casesdir <- map (poscasesdir</>) <$> listDirectory poscasesdir
        namedcases <- mapM ( sequence . (id &&& readFile) ) casesdir
        return namedcases

    mapM_ describeValidTypeCheck poscases

    -- let casesdir = "test/MplPasses/TypeChecker/cases/typecheck"
    -- negcases <- runIO $ do 
    --     let negcasesdir = casesdir </> "negative"
    --     casesdir <- map (negcasesdir</>) <$> listDirectory negcasesdir
    --     namedcases <- mapM ( sequence . (id &&& readFile) ) casesdir
    --     return namedcases
    -- mapM_ (`describeAnyErrorsFile` ("General negative tests", 
    --         _MplTypeCheckErrors 
    --         -- % _TypeCheckUnificationErrors 
    --         -- % _TypeForallMatchFailure
    --         )
    --         ) negcases


    -- mapM_ (`describeAnyErrors` ("Type match failure", 
    --         _MplTypeCheckErrors 
    --         % _TypeCheckUnificationErrors 
    --         % _TypeMatchFailure))
    --     [ nm1
    --     , nm2 
    --     , nm3
    --     , nm4
    --     , nm5 
    --     , nm6
    --     , nm7
    --     , nm8
    --     , nm9
    --     , nm10
    --     , nm11
    --     , nm12
    --     , nm13
    --     , nm14
    --     ]

    -- mapM_ (`describeAnyErrors` ("Occurs check", 
    --         _MplTypeCheckErrors 
    --         % _TypeCheckUnificationErrors 
    --         % _TypeOccursCheck))
    --     [ no0 ]


-- Invalid tests  
----------------------------

-- Forall match failure
-------------
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


-- Match failures
-------------
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

proc nm4 =
    Succ(a) | b => c -> nm4(a | b => c)
    _ | b => c -> nm4(NZero | b => c)
|]

nm5 = [r|
protocol Test => S =
    -- Testing :: TopBot => S
    Testing0 :: TopBot => S
    Testing1 :: TopBot => S

proc nm5 =
    | a => other -> do
        hcase a of
            Testing0 -> do
                close other
                halt a
            Testing1 -> do
                get b on other
                close other
                halt a
|]

nm6 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data NegNat -> S =
    Pred :: S -> S
    NZero ::   -> S


protocol Test(A,B | ) => S =
    Testing0 :: Put(A | TopBot) => S
    Testing1 :: Put(A | TopBot) => S

proc nm6 :: | Test(A,A |) => = 
    | a => -> do
        hcase a of
            Testing0 -> do
                get NZero on a
                halt a
            Testing1 -> do
                get Zero on a
                halt a
|]

nm7 = [r|
protocol Test(A,B | ) => S =
    Testing0 :: Get(A | TopBot) => S
    Testing1 :: Get(A | TopBot) => S

proc nm7 =
    | => other -> do
        hput Testing0 on other
        get _ on other
        get _ on other
        halt other
|]

nm8 = [r|
proc nm8 =
    | a,b => -> do
        race 
            a -> do
                get _ on a
                close b
                halt a
            b -> do
                get _ on a
                get _ on b
                close a
                halt b
|]

nm9 = [r|
proc nm9 =
    |  => a -> do
        plug
            => a,c -> do
                close a
                halt c
            c => -> do
                get _ on c
                halt c
|]

nm10 =[r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data Negative -> S =
    NSucc :: S -> S
    NZero ::   -> S

proc nm10 :: Nat() | TopBot => =
    a |  b => -> 
        case a of
            Succ(a) -> do
                halt b
            NZero -> do
                halt b
|]
nm11 =[r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

proc nm11 :: Nat() | TopBot => =
    a |  b => -> 
        case a of
            Succ(a) -> do
                halt b
            Zero -> do
                get _ on b
                halt b
|]

nm12 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data Unit -> S = 
    Unit :: -> S

fun nm12 =
    a -> fold a of
        Succ : b -> Succ(b)
        Zero : -> Unit
|]

nm13 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data 
    Rose(A) -> S =
        Branches :: A, T -> S
    and
    Forest(A) -> T =
        ForestCons :: S,T -> T
        ForestNil ::      -> T

data Wrapper(A) -> S =
    Wrapper :: A -> S

fun nm13  =
    a -> fold a of
        Branches : v, rst -> rst
        ForestCons : a,Wrapper(b) -> fold a of
            Succ : a -> Succ(a)
            Zero : -> b
        ForestNil : -> Zero
|]

nm14 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data NegNat -> S =
    Pred :: S -> S
    NZero ::   -> S

codata S -> Tuple(A,B) =
    P1 :: S -> A
    P2 :: S -> B

codata 
    S -> Zig(A,B) =
        HeadA :: S -> A
        TailA :: S -> T
    and 
    T -> Zag(A,B) =
        HeadB :: T -> B
        TailB :: T -> S
        

fun nm14 =
    -> unfold (P1 := -> NZero, P2 := -> Zero) of
        (P1 := n, P2 := i) of
            HeadA : -> n
            TailA : -> (P1 := -> i, P2 := -> Pred(n))
        (P1 := n, P2 := i) of
            HeadB : -> i
            TailB : -> (P1 := -> n, P2 := -> Succ(i))
|]


-- Occurs checks
-------------
no0 = [r|
proc no0 =
    | b => -> do
        get a on b
        no0(| b => )
|]

