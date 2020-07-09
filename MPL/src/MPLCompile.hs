{-# LANGUAGE QuasiQuotes #-}
module MPLCompile where

import Optics

import Control.Monad.State
import Control.Monad.Except

import Data.Either
import Control.Arrow
import MPLUtil.Data.Either.AccumEither

import MPLAST.MPLASTTranslate
import MPLAST.MPLASTTranslateErrors
import MPLAST.MPLProgI
import MPLPasses.ToGraph

import MPLPasses.MPLRunPasses

import Language.AbsMPL
import Language.LayoutMPL
import Language.ParMPL
import Language.ErrM

import Text.RawString.QQ

parseAndLex :: String -> Err MplProg
parseAndLex = pMplProg . resolveLayout True . myLexer

compile :: String -> ([TranslateBnfcErrors], ProgI BnfcIdent)
compile str = case parseAndLex str of
        Ok a -> translateBnfcMplToProg a
        Bad str -> error str

unsafecompile :: String -> ProgI BnfcIdent
unsafecompile = snd . compile

teststr :: String
teststr = [r|
defn
    data 
        POTATO(B,C) -> A  =
            Names, Names :: [([A],B,C(A,(B)))] -> A

    fun fUNCTION :: A,B -> A =
        _ -> a
where 
    data 
        A -> A =
            Names, Names :: A -> A
|]

testrecursive = [r|
data 
    Mutually(A,B) -> A =
        Constructor1 :: A -> A
        Constructor2 :: A -> A
    and 
    OtherMutually(A,B) -> A =
        Constructor1 :: A -> A
        Constructor2 :: A -> A
|]

{- $>
compile teststr
<$ -}

{-  
case compile teststr of
    (_, Prog stmts) -> stmts & foldMapOf ( 
                folded 
                % stmtDefns
                % folded
                % unDefnI
                % _DataDefn
                % folded
                % typeClauseDecDefTraversal
                ) pure :: [BnfcIdent]
-}

testing :: Int -> State Int (Either String Int)
testing n = do
    modify (+1)
    return $ Right n

testingg :: Int -> StateT Int (Either String) Int
testingg n = do
    modify (+1)
    return $ n


-- testt :: [Either Char Int]
-- testt = traverse _ [testing]

-- testt =  flip runState 0  $ runExceptT testing
-- testt :: ([Either Char Int], Int)
-- testt =  flip runState 0  $ runExceptT (traverse testing2 [1,2,3])
-- testt =  _ (traverse testing2 [1,2,3])
-- testt = flip runState 0  (traverse testingg [1,2,3])

testing1 :: Int -> StateT Char (Either String) Int 
testing1 n = do
    if n == 3 || n == 2
        then modify succ >> throwError "potato"
        else do
            modify succ
            return 3

-- this is how we want to sequencde ffects (something along the lines of this...)
test1 :: (Either String [Int], Char)
test1 =  
    first runAccumEither
    $ foldr f (liftAEither (Right []), 'a') 
        [1,2,3]
  where
    f a (acc,st) = case runStateT (testing1 a) st of
        Right (n, st') -> (liftAEither (Right [n]) <> acc, st')
        Left c -> (liftAEither (Left c) <> acc, st)
    
{- $>
    -- test1
<$ -}

testing2 :: Int -> ExceptT String (State Char) Int
testing2 n = do
    if n == 3 || n == 2
        then modify succ >> throwError "potato"
        else do
            modify succ
            return 3

test2 :: (Either String [Int], Char)
test2 =  
    first ( runAccumEither . foldr (<>) mempty . map (fmap pure) ) 
    $ flip runState 'a' 
    $ traverse (fmap liftAEither . runExceptT . testing2) 
        [1,2,3]

{- $>
    -- test2
<$ -}
