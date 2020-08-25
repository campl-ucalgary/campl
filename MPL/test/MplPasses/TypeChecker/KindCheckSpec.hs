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
import MplPasses.Passes
import MplPasses.Env

-- Tests for overlapping declarations and out of scope errors 

spec = do
    mapM_ (`describeValidRename` const (return ()))
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
        ]

    mapM_ (`describeAllErrors` ("out of scope", _MplRenameErrors % _OutOfScope))
        [ n1
        , n2
        , n3 
        , n4 
        , n5 
        , n6 
        , n7 
        , n8 
        , n9 
        ]


-- Valid tests  
----------------------------
v1 = [r|
proc v1 =
    | c => a -> do
        fork a as
            a -> do
                fork a as
                    a -> do
                        close a
                        halt c
                    b -> halt b
            b -> do
                halt b 
|]

v2 = [r|
proc v2 =
    | => a -> do
       fork a as
            a -> do
                halt a
            b -> do
                halt b 
|]

v3 = [r|
proc v3 =
    | => a -> do
        plug 
            => a,b -> do
                close b
                halt a
            b => a -> do
                close b
                halt a
|]

v4 = [r|
defn
    fun test =
        a -> hehemut(a)
    fun hehemut =
        a -> a
|]

v5 = [r|
codata 
    C -> App(A,B) =
        App :: A,C -> B
fun appwrapper =
    (App := f), a -> f(a)
|]

v6 = [r|
data
    MyData(A,B) -> C =
        MyData :: A,B -> C
fun appwrapper =
    a -> case a of
        MyData(a,b) -> a
        MyData(_,_) -> a
|]


v7 = [r|
data
    MyData(A,B) -> C =
        MyData :: A,B -> C

fun v7 :: B,MyData(A,A) -> A =
    b, a -> case a of
        MyData(a,b) -> a
        MyData(_,_) -> a
|]

v8 = [r|
data
    MyData(A,B,D) -> C =
        MyData1 :: A,B,D -> C
        MyData2 :: A,B,E -> C

    and 
    Other(A,B,D) -> E =
        Other :: A,B,C -> E
|]

v9 = [r|
fun testing =
    a -> testing(a)
|]

v10 = [r|
fun testing =
    a -> 
        let fun wow =
                b -> a
        in wow(a)
|]

v11 = [r|
defn 
    fun v11 =
        a -> pow(a)
where
    fun wow =
        a -> a

    fun pow =
        a -> wow(a)
|]


-- Invalid tests  
----------------------------
n1 = [r|
proc n1 =
    | => a -> do
       fork a as
            c -> do
                halt a
            b -> do
                halt b 
|]

n2 = [r|
proc n2 =
    | => a -> do
       fork a as
            c -> do
                halt c
            b -> do
                halt c 
|]

n3 = [r|
proc n3 =
    | => a -> do
        plug 
            => a,b -> do
                close b
                halt a
            b => a,c -> do
                close b
                close c
                halt a
            c => b -> do
                close b
                close a
                halt c
|]

n4 = [r|
fun test =
    a -> hehemut(a)

fun hehemut =
    a -> a
|]

n5 = [r|
fun n5 =
    a -> a
    b -> a
|]

n6 = [r|
data
    MyData(A,B) -> C =
        MyData1 :: A,B,D -> C
        MyData2 :: A,B,D -> C
|]

n7 = [r|
defn 
    fun n7 =
        a -> pow(a)
where
    fun pow =
        a -> wow(a)
    fun wow =
        a -> a
|]

n8 = [r|
defn 
    fun n8 =
        a -> pow(a)
where
    fun pow =
        a -> pow(a)

fun whereoutofscope =
    a -> pow(a)
|]

n9 = [r|
defn 
    fun n9 =
        a -> 
            let
                fun pow =
                    a,b -> a
            in pow(a)

fun testing =
    a -> pow(a)
|]
