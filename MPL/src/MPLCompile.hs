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
        Left n -> mapM_ print n

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

testscoppe = [r|
defn 
    fun foo = 
        a -> orange(a)

    fun orange =
        a -> out(a)

fun bar =
    a -> foo(a,a)
|]

test1Errrr = [r| 
{-
data 
    Zig(A,B) -> Z =
        Zig :: A,B -> Z

data 
    Kartofler -> C =
        Kartofler :: -> C

data 
    Orange -> C =
        Orange :: -> C

fun test :: A(A,B) -> A(A,B) =
    a,b -> Zig(a,b)
    -}
-- fun test :: Zig(B,C) -> Zig(C,B) =
fun test :: B -> C =
    a -> a
|]

testtomatoorange = [r| 
data
    KindTest(A,B) -> C =
        KindTest :: A,B -> C
fun tomato :: KindTest(B,C) -> KindTest(C,B) = 
    KindTest(a,b) -> KindTest(b,a)


|]



testcall = [r|
{-
data 
    Unit -> C =
        Unit :: -> C
        -}

data
    Nat -> STATEVAR =
        Nat :: STATEVAR -> STATEVAR
        Zero ::         -> STATEVAR
defn 
    fun functiontest =
        Nat(a) -> a -- case a of
            -- b -> b

{-
fun tomato :: A -> A = 
    a -> a
    -}
{-
defn
    fun tomato :: A -> B = 
        a -> orange(a)
    fun orange  = 
        b -> tomato(b)
        -}

{-
data 
    List(A) -> C =
        Nil :: -> C
        Cons :: A,C-> C

fun crazy =
    a,b -> case a of
        Nil -> b
        Cons(s,t) -> s(crazy(t,b))
        -- BROKEN (Because we need to update how the symbol table composition works)
        -}
|]

testinggg = [r| 
data 
    Zig -> Z =
        Zig  :: Z,X -> Z
        Zigg ::    -> Z
    and
    Zag -> X =
        Zag :: X, Z -> X
        Zagg ::     -> X

fun test :: Zig,Zag -> Zig =
    a,b -> Zig(a,b)
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

testtrivailforall = [r|
data 
    Unit(A,B) -> S =
        UnitP :: A,B -> S
        Unit :: -> S
fun testing :: -> Unit(A,A) = 
    -> Unit

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


