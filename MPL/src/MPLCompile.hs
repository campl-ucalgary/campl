{-# LANGUAGE QuasiQuotes #-}
module MPLCompile where

import Optics

import Control.Monad.State
import Control.Monad.Except

import Data.Either
import Data.Maybe
import Data.List
import qualified Data.List.NonEmpty as NE

import Control.Arrow
import MPLUtil.Data.Either.AccumEither

import MPLAST.MPLASTTranslate
import MPLAST.MPLASTTranslateErrors
import MPLAST.MPLProgI
import MPLAST.MPLASTCore

import MPLPasses.TieDefns
import MPLPasses.GraphGenCore
import MPLPasses.TieDefnsErrors

import MPLPasses.MPLRunPasses

import Language.AbsMPL
import Language.LayoutMPL
import Language.ParMPL
import Language.ErrM

import Text.RawString.QQ

parseAndLex :: String -> Err MplProg
parseAndLex = pMplProg . resolveLayout True . myLexer

unsafeTranslateParseLexGraph :: 
    String -> 
    IO (Either [TieDefnsError] (Prog (DefnG TaggedBnfcIdent TypeTag)))
unsafeTranslateParseLexGraph str = do
    st <- defaultGraphGenCoreState
    return $ progInterfaceToGraph ([], defaultGraphGenCoreEnv, st) (unsafeTranslateParseLex str) 

{-
progGTypes :: 
    (Prog (DefnG TaggedBnfcIdent TypeTag)) -> 
    [TypeGTypeTag]
progGTypes (Prog defsg) = concat $ map f defsg
  where
    f (Stmt defns wdefs) = mapMaybe g (NE.toList defns) ++ progGTypes (Prog wdefs)
    g (FunctionDecDefG defn) = Just $ defn ^. funTypesFromTo
    g _ = Nothing
    -}

pprintFunctionTypes :: 
    (Prog (DefnG TaggedBnfcIdent TypeTag)) -> 
    [String]
pprintFunctionTypes (Prog defsg) = concat $ map f defsg
  where
    f (Stmt defns wdefs) = mapMaybe g (NE.toList defns) ++ pprintFunctionTypes (Prog wdefs)
    g (FunctionDecDefG defn) = Just $ pprint defn
    g _ = Nothing

-- typeTester n = fmap (intercalate "\n" . map pprint . progGTypes) <$> unsafeTranslateParseLexGraph n 
typeTester n = do
    proggraph <- unsafeTranslateParseLexGraph n 
    case proggraph of
        Right n -> putStrLn $ mconcat $ pprintFunctionTypes n
        Left n -> putStrLn $ show n

testdata = [r|
defn
    data
        DataOne(A,B) -> Chicken =
            DataOne :: Beef -> Chicken
            DataOne ::   DataThree(A),B      -> Chicken
        and 
        DataTwo(A,B) -> Beef =
            DataTwo :: Chicken, Beef -> Beef

    data 
        DataThree(A) -> Pork =
            DataThree :: DataThree(Pork) -> Pork

|]

testnat = [r|
defn 
    fun functiontest =
        Nat(Nat(a)),Zero -> Zero
where
    data
        Nat -> STATEVAR =
            Nat :: STATEVAR -> STATEVAR
            Zero ::         -> STATEVAR
|]

testdataoutofscope = [r|
data 
    Unit -> S =
        Unit :: -> S

fun functiontest =
    Potato -> Unit

|]


testmutnat = [r|
data 
    Unit -> S =
        Unit :: -> S
defn 
    fun functiontest =
        -- Potato -> Twos(TwosEnd, Zeros (TwosEnd))
        -- Ones(Zeros(TwosEnd),b) -> Twos(TwosEnd, Zeros (TwosEnd))
        Ones(Zeros(TwosEnd),b) -> b

        -- Ones(Zeros(TwosEnd),b) -> case b of 
        --     Twos(TwosEnd, Zeros (TwosEnd))
        --     incorrect
        -- Ones(Zeros(Unit),b) -> Twos(TwosEnd, Zeros (TwosEnd))
where
    data
        Ones(A) -> ONES =
            Ones :: ONES, TWOS -> ONES
            Zeros :: A -> ONES
        and
        Twos(A) -> TWOS =
            Twos :: TWOS, ONES -> TWOS
            TwosEnd ::         -> TWOS

|]

testlet = [r|

data 
    Unit -> S =
        Unit :: -> S

fun lettest =   
    Unit -> 
        let
            data Uni2 -> S =
                Uni2 :: -> S
        in a

|]


unsafeTranslateParseLex :: String -> ProgI BnfcIdent
unsafeTranslateParseLex str = case parseAndLex str of
        Ok a -> case translateBnfcMplToProg a :: ([TranslateBnfcErrors], ProgI BnfcIdent) of
            ([], prog) -> prog
            (errs, _) -> error $ show errs
        Bad str -> error str

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
-- unsafeTranslateParseLex teststr
<$ -}

{-  
case unsafeTranslateParseLex teststr of
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
