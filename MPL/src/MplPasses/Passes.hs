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

fun testing :: Int -> Char=
    "asdf" -> "asdf"
    [] -> "asdf"
    ['a'] -> [1,2,3]
    () -> ()

-- memory cell example
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
proc p2 =
    b | => a -> do
        hput MemPut on a
        put b on a
        hput MemCls on a
        halt a

proc p2 :: | Passer(| Mem(A|)) => InpTerm(A|), Mem(A|) =
proc p2 =
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
-}
|]
