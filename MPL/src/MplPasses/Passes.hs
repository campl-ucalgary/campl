{-# LANGUAGE TemplateHaskell #-}
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

import MplPasses.Env

import MplAST.MplCore

import MplUtil.UniqueSupply
import Control.Monad

import Text.RawString.QQ

import Data.Word
import Data.Void
import Data.List

import Debug.Trace

{- This module conglomerates all the passes together and runs them all at
 - once producing one unified error data type (if the program is invalid) 
 - or a fully annotated AST tree.
 -}

data MplPassesErrors =
    MplBnfcErrors B.BnfcErrors
    | MplParseErrors ParseErrors
    | MplRenameErrors RenameErrors
    | MplTypeCheckErrors TypeCheckErrors
  deriving Show

$(makeClassyPrisms ''MplPassesErrors)

instance B.AsBnfcErrors MplPassesErrors where
    _BnfcErrors = _MplBnfcErrors

instance AsParseErrors MplPassesErrors where
    _ParseErrors = _MplParseErrors

instance AsRenameErrors MplPassesErrors where 
    _RenameErrors = _MplRenameErrors

instance AsTypeCheckErrors MplPassesErrors where
    _TypeCheckErrors = _MplTypeCheckErrors 

instance AsTypeCheckSemanticErrors MplPassesErrors where
    _TypeCheckSemanticErrors = _MplTypeCheckErrors % _TypeCheckSemanticErrors 

instance AsKindCheckErrors MplPassesErrors where
    _KindCheckErrors = _MplTypeCheckErrors % _TypeCheckKindErrors 

instance AsTypeCheckCallErrors MplPassesErrors where
    _TypeCheckCallErrors = _MplTypeCheckErrors % _TypeCheckCallErrors 

instance AsTypeUnificationError MplPassesErrors MplTypeSub where
    _TypeUnificationError = _MplTypeCheckErrors % _TypeUnificationError 

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

tracePprint n = trace (pprint n) n

runPassesTester ::
    String -> 
    IO ()
runPassesTester str = do
    env <- mplPassesEnv
    case runPasses env str of
        Right v -> putStrLn $ pprint v
        Left v -> putStrLn $ intercalate "\n" $ map show v

huh = [r|
-- codata S -> Tuple(A,B) =
    -- P0 :: S -> A
    -- P1 :: S -> B

-- fun prj0 :: Tuple(A,B) -> A =
    -- (P0 := a, P1 := b) -> a

{-
fun proj :: A,B -> B =
    a,b -> b
-}

{-
proc v14 :: | => Get(A|Put(A|TopBot)) =
    | => b -> do
        get a on b
        put a on b
        halt b
-}

{-
codata S -> Tuple(A,B) =
    P0 :: S -> A
    P1 :: S -> B


fun prj0 :: Tuple(A,B) -> A =
    (P0 := a, P1 := b) -> a
-}

fun n6 :: A -> B =
    a -> 
        let fun f = 
                b -> a
        in f(a)
{-
proc v30 :: | => A =
    |  => a -> do
        plug
            => a,c -> do
                close a
                halt c
            c => b -> do
                close b
                halt c
            b => -> do
                halt b
-}
{-
codata S -> Tuple(A,B,C) =
    P0 :: S -> A
    P1 :: S -> B
    P2 :: S -> C

fun v20 :: A,B -> Tuple(A, A, A)=
    a,b -> (P0 := -> a, P1 := -> a, P2 := -> a)
-}


{-
fun nf1 :: B -> A =
    a -> a

fun v7 :: A -> B =
    a -> v7(a)
proc v14 :: | => Get(A|Put(A|TopBot)) =
    | => b -> do
        get a on b
        put a on b
        halt b

-}
{-
protocol Test(A,B | ) => S =
    Testing0 :: Put(A | Get(B |TopBot)) => S
    Testing1 :: Put(B | TopBot) => S

proc v18 :: | Test(A,A |) => =
    | a => -> do
        hcase a of
            Testing0 -> do
                get res on a
                put res on a
                halt a
            Testing1 -> do
                get _ on a
                halt a
-}


|]
