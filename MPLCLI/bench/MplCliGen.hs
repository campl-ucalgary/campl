{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
module MplCliGen where

import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec

import Data.Char
import Data.Bool
import Control.Arrow

import Data.Typeable
import Data.Data

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Language.Haskell.TH.Syntax as TH


{- | a type wrapper for a variable which is to be filled. -}
newtype MetaVal = MetaVal String
  deriving (Show, Eq, Ord, Data, Typeable, TH.Lift)

{- | a piece of an mpl program -- either we 
have an actual piece of the program with
'MplPiece', or we have a hole to be filled with
'Hole'
-}
data MetaPiece 
    = MplPiece String 
    | Hole MetaVal
  deriving (Show, Data, Typeable, TH.Lift)

{- | A 'MetaProg' is a list of either actual
strings which make the program, or pieces to
be filled in
-}
type MetaProg = [MetaPiece]

{- | An alias for a mapping of 'MetaVal's -}
type MetaValSubs = Map MetaVal String


{- | fills a meta program with the given fillers from the map -}
fillMetaProg ::
    -- | mapping of MetaVal to Strings to fill a hole with
    MetaValSubs ->
    -- | a meta program to have holes filled by.
    MetaProg ->
    -- | resulting string program filled in
    String 
fillMetaProg mp prog = concatMap go prog
  where
    go = \case
        MplPiece str -> str
        Hole mv -> case Map.lookup mv mp of
            Just v -> v 
            Nothing -> error $ concat 
                [ "Meta value "
                , show mv
                , " does not have a filler with "
                , show mp
                , " on program "
                , show prog
                ]
    


{- | 'pMetaProg' will parse a mpl program which
has "holes" indicated by "??<SOME-ALPHA-NUMERIC-STRING-HERE>"
to be filled in by provided values. This is to make
benchmarks easier.

For example, consider the following program..
@
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun decToZero :: Int -> [Char] =
    0 -> "wahoo!"
    n -> decToZero(n - 1)

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put decToZero( 1  ) on _console

        hput ConsoleClose on _console
        halt _console
@
we would probably like to benchmark this
for @decToZero( 10  ), decToZero( 100  ), .. @
and so on, so we would write
@
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun decToZero :: Int -> [Char] =
    0 -> "wahoo!"
    n -> decToZero(n - 1)

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put decToZero( ??n  ) on _console

        hput ConsoleClose on _console
        halt _console
@
and have @n=1,10,100,...@ and benchmark
each of those individually, where the
entire @??n@ gets replaced by 1,10,100 and 
so on.

N.B. This is a straight substitution of the holes e.g.
@
??a??b
@
with @a=0,b=1@ becomes
@
01
@
which may or may not be what you want.
-}
pMetaProg :: Parsec String () MetaProg
pMetaProg = pGo0
  where
    -- pGo0 = (:) <$> fmap MplPiece (manyTill anyChar (try (string "??"))) <*> pGo1
    pGo0 = go >>= \(piecestr, isend) -> let piece = MplPiece piecestr in bool ((piece:) <$> pGo1) (pure [piece]) isend
      where
        go = 
            do { fmap ([],) $ try (string "??" *> pure False <|> eof *> pure True) }
            <|> 
            do { c <- anyChar ; cs <- go ; return $ first (c:) cs }

    pGo1 = (:) <$> fmap (Hole . MetaVal) (many1 alphaNum) <*> pGo0

{- | helpful wrapper to parse a meta program for the template haskell quasi quoter in 'MplCliGenTH'.
-}
parseMetaProgM :: 
    ( Monad m 
    , MonadFail m ) =>
    (SourceName, Line, Column) ->
    String ->
    m MetaProg
parseMetaProgM (sn, l, c) str = case runParser pGo () "" str of
    Right res -> return res
    Left err -> fail $ show err
  where
    pGo = do
        pos <- getPosition
        setPosition 
            $ (flip setSourceName) sn
            $ (flip setSourceLine) l
            $ (flip setSourceLine) c
            $ pos
        pMetaProg
    

