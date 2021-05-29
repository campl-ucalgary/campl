{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Passes where

import Optics

import qualified MplPasses.Parser.BnfcParse as B
import MplPasses.Parser.Parse
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R

import MplPasses.TypeChecker.TypeCheck
import MplPasses.TypeChecker.TypeCheckErrors
import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckErrorPkg 
import MplPasses.TypeChecker.TypeCheckMplTypeSub

import MplPasses.PatternCompiler.PatternCompile 
import MplPasses.LambdaLifter.LambdaLift

import Data.Proxy

import MplPasses.PassesErrorsPprint

import MplPasses.Env

import MplAST.MplCore

import MplUtil.UniqueSupply
import Control.Monad

import Text.RawString.QQ

import Data.Word
import Data.Void
import Data.List

import MplPasses.PassesErrors

import Debug.Trace

{- This module conglomerates all the passes together and runs them all at
 - once producing one unified error data type (if the program is invalid) 
 - or a fully annotated AST tree.
 -}

data MplPassesEnv = MplPassesEnv {
    mplPassesEnvUniqueSupply :: UniqueSupply
    , mplPassesTopLevel :: TopLevel
}

mplPassesEnv :: IO MplPassesEnv 
mplPassesEnv = do
    uniqsup <- initUniqueSupply 0
    return $ MplPassesEnv uniqsup TopLevel

runPasses :: 
    MplPassesEnv -> 
    String -> 
    Either [MplPassesErrors] _
runPasses MplPassesEnv{mplPassesEnvUniqueSupply = supply, mplPassesTopLevel = toplvl} = 
    return . runLambdaLiftProg
    <=< runPatternCompile' (toplvl, rrs) 
    <=< fmap tracePprint . runTypeCheck' (toplvl, lrs) 
    -- <=< runTypeCheck' (toplvl, lrs) 
    -- TODO remove the trace in the future...
    <=< fmap tracePprint . runRename' (toplvl, ls)
    <=< runParse' 
    <=< B.runBnfc
    -- in runRename' (TopLevel, ls, rsymtab) <=< runParse' <=< B.runBnfc
  where
    (ls, rs) = split supply
    (lrs, rrs) = split rs

{-
runPassesTester ::
    String -> 
    IO ()
-}
runPassesTester str = do
    env <- mplPassesEnv
    case runPasses env str of
        Right v -> putStrLn $ pprint (Proxy :: Proxy MplRenamed) v
        Left v -> putStrLn $ show $ vsep $ map pprintMplPassesErrors v

freshhuh = [r|
data
    MyList(A) -> S =
        MyCons :: A,S -> S
        MyNil ::      -> S

fun fk =
    a,b -> 
        let defn
                fun myotherfun0 = 
                    -> myotherfun1(b)
                fun myotherfun1 = 
                    _ -> a
                fun myotherfun2 = 
                    -> myotherfun0
        in myotherfun0

proc dpg =
    MyCons(MyCons(_, _), rst) | ch0 => ch1 ->do
        close ch0
        halt ch1
    MyCons(_, rst) | ch0 => ch1 ->do
        close ch0
        halt ch1
    MyNil | ch2 => ch3 ->do
        close ch3
        halt ch2

proc run  =
    | => -> do
        plug
            => a -> do
                halt a
            a,l => b,k -> do
                close b
                halt a
            b,k => c,l -> do
                close b
                halt c
            c => -> do
                halt c
|]

huh = [r|
{-
proc v25 :: | => TopBot (*) TopBot, TopBot =
    | => a,other -> do
        fork a as
            s -> do
                close other 
                halt s
            t -> halt t
-}
{-
data 
    Unit -> S =
        Unit ::  -> S
data 
    Unitt -> S =
        Unitt ::  -> S
data
    Wrapper(A) -> S =
        Wrapper :: A -> S
-}

{-
fun cheat0 =
    a -> cheat0(a)

fun cheat1 =
    a -> cheat1(a)

fun cheat2 :: Bool -> Bool =
    True -> True
-}
{-
fun fkkk  =
    a,b,c ->  switch 
        a -> b
        c -> b
fun fkkk  =
    a,b,c ->  switch 
        a -> b
        c -> b

proc fkk = 
    | a => b ->
        switch
            True -> do
                close a
                halt b
            False -> do
                close a
                halt b

proc fkk = 
    | a => b ->
        if True
            then do
                close a
                halt b
            else do
                close a
                halt b
        
-}

defn 
    fun poop =
        (App := f), a -> f(a)

    codata
        S -> Fun(A,B) =
            App :: A,S -> B

{-
defn 
    fun poop =
        MyNil, (P0 := a) -> a
        MyCons(fk,fkkk), (P1 := a, P0 := b) -> b

    codata
        S -> MyTupleThing(A,B,C) =
            P0 :: S -> A
            P1 :: S -> B
            P2 :: S -> C

    data
        MyList(A) -> S =
            MyCons :: A,S -> S
            MyNil ::      -> S
-}

data
    MyUnit -> S =
        MyUnit :: -> S


defn
    {-
    fun testing =
        MyCons(a,b),MyNil -> a
        MyCons(a,b),MyCons(c,d) -> a
        MyNil, MyNil -> cheat()
        MyNil, MyCons(c,d) -> c

    fun testing =
        MyCons(a,b),c -> 
            MyCons(a, testing(b, c))
        MyNil,c -> c
    -}

    {-
    fun demo = 
        ss,ts -> case ss of
            MyCons(MyUnit, banan) -> ts
    -}


    {-
    fun demo = 
        f, MyNil, ys -> cheat0(ys)
        f, xs, MyNil -> cheat1(xs)
        f, MyCons(x,xs), MyCons(y,ys) -> cheat2(ys)
    -}

    {-
    fun lol = 
        a,_,_ -> case a of
            MyNil -> a
            a -> a
    -}
    fun lol = 
        a -> case a of
            MyNil -> a
            MyCons(_,_) -> a

    fun tupletest =
        MyNil,  (P0 := a, P1 := b) ->  a
        MyCons(_,_), (P0 := MyNil, P1 := b) ->  b


    fun tupletest2 =
        (P0 := MyCons(a,_), P1 := b) ->  a
        (P0 := MyNil, P1 := b) ->  b

    fun tupletest2 =
        (a,b) ->  MyNil
        (MyNil, b) ->  MyNil

    codata 
        S -> Tuple(A,B) =
            P0 :: S -> A
            P1 :: S -> B
    data
        MyList(A) -> S =
            MyCons :: A,S -> S
            MyNil ::      -> S

data 
    Identity(A) -> S =
        Identity :: A -> S

proc fuk = 
    | ch => -> do
        get MyCons(a,_) on ch
        put a on ch
        halt ch
    

{-
-- GET BACK TO FIXING THE DUPLCIATED EROR MESSAGE FOR THIS
fun testing =
    f,a -> f(a)
-}

{-
fun testing :: Int -> Char=
    "asdf" -> "asdf"
    [] -> "asdf"
    ['a'] -> [1,2,3]
    () -> ()
-}

{-
protocol Mem(M|) => S =
    MemPut :: Put(M|S) => S
    MemGet :: Get(M|S) => S
    MemCls :: TopBot => S

protocol InpTerm(I|) => S =
    InpPut :: Put(I|S) => S
    InpGet :: Get(I|S) => S
    InpCls :: TopBot => S

protocol Passer(|P) => S =
    Passer :: P (+) (Neg(P) (*) S) => S

proc memory :: A | Mem(A|) => =
    x | ch => -> do
        hcase ch of
            MemPut -> do
                get y on ch
                memory(y | ch => )
            MemGet -> do
                put x on ch
                memory(x | ch => )
            MemCls -> do
                halt ch

proc p1 :: | => Passer(|Mem(A|)), InpTerm(A|) = 
    | => passer, inp -> do
        hput Passer on passer
        split passer into mm,nmpp
        hput MemGet on mm 
        get y on mm
        hput InpPut on inp
        put y on inp
        hput InpGet on inp
        get x on inp
        hput MemPut on mm
        put x on mm
        fork nmpp as
            nm -> nm |=| neg mm
            pp -> p1(| => pp, inp)

proc p2 :: | Passer(| Mem(A|)) => InpTerm(A|), Mem(A|) =
    | passer => inp, mem -> do
        hcase passer of
            Passer -> do
                hput MemGet on mem
                get y on mem
                hput InpPut on inp
                put y on inp
                hput InpGet on inp
                get x on inp
                hput MemPut on mem
                put x on mem
                fork passer as
                    mm with mem -> do
                        mm |=| mem
                    nmpp with inp -> do
                        split nmpp into nm, pp
                        plug
                            p2( | pp => inp,z)
                            z,nm => -> z |=| neg nm

proc run :: | => InpTerm(Int |) , InpTerm(Int|) =
    | => inpterm0, inpterm1 -> do
        plug
            p1(| => passer, inpterm0)
            p2(| passer => inpterm1, mem)
            memory(100 | mem => )
-}

|]
