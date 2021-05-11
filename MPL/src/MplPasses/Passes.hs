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
    runTypeCheck' (toplvl, rs) 
    -- TODO remove the trace in the future...
    <=< fmap tracePprint . runRename' (toplvl, ls)
    <=< runParse' 
    <=< B.runBnfc
    -- in runRename' (TopLevel, ls, rsymtab) <=< runParse' <=< B.runBnfc
  where
    (ls, rs) = split supply

tracePprint n = trace (pprint (Proxy :: Proxy MplRenamed) n) n

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

huh = [r|
{-
fun n6 =
    -> 
        let fun a = 
                -> 3 
        in let fun b =
                -> 4.3
           in a + b + 3
-}
{-

fun n6  :: -> Int =
    -> 3.4 + 3 
-}

{-
data 
    Fuk(A,B,C) -> S =
        Fukk :: Fuk(A) -> S
-}

codata 
    S -> Fun(A,B) =
        App :: Fun(A),S -> B

fun a5 =
    f -> App(a5(f), f)
{-
proc v26 :: | TopBot (+) TopBot,TopBot => =
    | a,other =>  -> do
        fork a as
            s -> do
                close a 
                halt s
            t -> halt t
-}
    

{-
proc fk = 
    | a => b -> do
        put 123423423423234234234243 on b
        close a
        halt b
-}
{-
fun n6 =
    -> 3 + 3.4
-}


|]
