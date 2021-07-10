{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Passes 
    ( module MplPasses.Parser.Parse
    , module MplPasses.Renamer.Rename
    , module MplPasses.TypeChecker.TypeCheck
    , module MplPasses.PatternCompiler.PatternCompile
    , module MplPasses.LambdaLifter.LambdaLift
    , MplPassesEnv (..)
    , mplPassesEnv
    , runPasses

    -- for debugging
    , runPassesTester
    , fresher
    )
    where

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

import qualified Text.Pretty.Simple

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
    Either [MplPassesErrors] (MplProg MplLambdaLifted)
runPasses MplPassesEnv{mplPassesEnvUniqueSupply = supply, mplPassesTopLevel = toplvl} = 
    return . runLambdaLiftProg

    -- <=< fmap tracePprint . runPatternCompile' (toplvl, rrs) 
    <=< runPatternCompile' (toplvl, rrs) 

    -- <=< fmap tracePprint . runTypeCheck' (toplvl, lrs) 
    <=< runTypeCheck' (toplvl, lrs) 

    -- <=< fmap tracePprint . runRename' (toplvl, ls)
    <=< runRename' (toplvl, ls)

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
        Right v -> do
            -- Text.Pretty.Simple.pPrint v
            putStrLn $ pprint (Proxy :: Proxy MplRenamed) v
        Left v -> putStrLn $ show $ vsep $ map pprintMplPassesErrors v

fresher = [r|
fun n5 :: -> A =
    -> ()

|]

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
    MyCons(MyCons(_, _), rst) | ch0 => ch1 -> do
        close ch0
        halt ch1
    MyCons(_, rst) | ch0 => ch1 -> do
        close ch0
        halt ch1
    MyNil | ch2 => ch3 -> do
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

