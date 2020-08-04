{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
module MPLAST.MPLBuiltIn where

import Optics

import MPLAST.MPLExprAST
import MPLAST.MPLPatternAST
import MPLAST.MPLProcessCommandsAST
import MPLAST.MPLProg
import MPLAST.MPLProgGraph
import MPLAST.MPLProgTraversals
import MPLAST.MPLProgGraph
import MPLAST.MPLProgI
import MPLAST.MPLTypeAST 
import Text.RawString.QQ

import Control.Monad.State
import qualified Data.List.NonEmpty as NE


import Data.Maybe
import Data.Data

-- note: # is a special prefix for built in types.
-- the user cannot write # in front of any of their
-- code (otherwise parse error) TODO add this check in
{-
mplList = setBnfcPositionToNull $ unsafeTranslateParseLex [r|
data 
    #List(A) -> C =
        #Cons :: A,C -> C
        #Nil ::  -> C
|]

mplUnit = setBnfcPositionToNull $ unsafeTranslateParseLex [r|
data 
    #Unit -> C =
        #Unit :: -> C
|]

mplBool = setBnfcPositionToNull $ unsafeTranslateParseLex [r|
data 
    Bool -> C =
        False :: -> C
        True  :: -> C
|]

mplInt = setBnfcPositionToNull $ unsafeTranslateParseLex [r|
data 
    Int -> C =
        #Int :: -> C
|]

mplChar = setBnfcPositionToNull $ unsafeTranslateParseLex [r|
data 
    Char -> C =
        #Char :: -> C
|]

mplDouble = setBnfcPositionToNull $ unsafeTranslateParseLex [r|
data 
    Double -> C =
        #Double :: -> C
|]

setBnfcPositionToNull = setBnfcPositions (-1,-1)

setBnfcPositions :: 
    (Int, Int) ->
    ProgI BnfcIdent -> 
    ProgI BnfcIdent
setBnfcPositions pos progi = 
    everywhere f progi
  where
    f :: Data a => a -> a
    f n = case cast n :: Maybe (Int, Int) of
        Just _ -> fromJust $ cast pos
        Nothing -> n

-- SYB combinator..
everywhere :: 
    (forall a. Data a => a -> a) -> 
    (forall a. Data a => a -> a)
everywhere f = go
  where
    go :: forall a . Data a => a -> a
    go = f . gmapT go

{-
-- some utilities to help make writing this easier...
mkNullTaggedBnfcIdent :: 
    ( MonadState s m
    , HasUniqueTag s) =>
    String -> 
    m TaggedBnfcIdent
mkNullTaggedBnfcIdent str = 
    review _TaggedBnfcIdent . (mkNullBnfcIdent str,) 
    <$> freshUniqueTag

mkNullBnfcIdent :: String -> BnfcIdent
mkNullBnfcIdent str = BnfcIdent (str, (-1,-1))

-- List data type...
mplListClause = "#List"
mplListPhraseCons = "#Cons"
mplListPhraseNil = "#Nil"

mplList :: 
    ( MonadState s m
    , HasUniqueTag s) =>
    m (TypeClauseG TaggedBnfcIdent)
mplList = 
    TypeClause 
        <$> name 
        <*> args 
        <*> statevar 
        <*> phrases 
        <*> neighbors
  where
    name = mkNullTaggedBnfcIdent mplListClause
    args = sequenceA [mkNullTaggedBnfcIdent "A"]
    statevar = mkNullTaggedBnfcIdent "C"
    phrases = sequenceA
        [ TypePhrase 
            <$> (ClausePhraseKnot <$> mplList )
            <*> mkNullTaggedBnfcIdent mplListPhraseCons
            <*> sequenceA
                [ TypeVar <$> mkNullTaggedBnfcIdent "A"
                , TypeVar <$> mkNullTaggedBnfcIdent "C"]
            <*> (TypeVar <$> mkNullTaggedBnfcIdent "C")
        , TypePhrase 
            <$> (ClausePhraseKnot <$> mplList )
            <*> mkNullTaggedBnfcIdent mplListPhraseNil
            <*> sequenceA []
            <*> (TypeVar <$> mkNullTaggedBnfcIdent "C")

        ]
    neighbors = ClausesKnot <$> 
        ( ClausesGraph DataObj 
        <$> (sequenceA $ NE.fromList[mplList]))
    
-- Unit data type...
mplUnitClause = "#Unit"
mplUnitPhraseUnit = "#Unit"

mplUnit :: 
    ( MonadState s m
    , HasUniqueTag s) =>
    m (TypeClauseG TaggedBnfcIdent)
mplUnit = 
    TypeClause 
        <$> name 
        <*> args 
        <*> statevar 
        <*> phrases 
        <*> neighbors
  where
    name = mkNullTaggedBnfcIdent mplUnitClause
    args = sequenceA []
    statevar = mkNullTaggedBnfcIdent "C"
    phrases = sequenceA
        [ TypePhrase 
            <$> (ClausePhraseKnot <$> mplUnit )
            <*> mkNullTaggedBnfcIdent mplUnitPhraseUnit
            <*> sequenceA []
            <*> (TypeVar <$> mkNullTaggedBnfcIdent "C")
        ]
    neighbors = ClausesKnot <$> 
        ( ClausesGraph DataObj 
        <$> (sequenceA $ NE.fromList [mplUnit]))

-- Bool data type...
mplBoolClause = "#Bool"
mplBoolPhraseTrue = "#True"
mplBoolPhraseFalse = "#False"

mplBool :: 
    ( MonadState s m
    , HasUniqueTag s) =>
    m (TypeClauseG TaggedBnfcIdent)
mplBool = 
    TypeClause 
        <$> name 
        <*> args 
        <*> statevar 
        <*> phrases 
        <*> neighbors
  where
    name = mkNullTaggedBnfcIdent mplBoolClause
    args = sequenceA [mkNullTaggedBnfcIdent "A"]
    statevar = mkNullTaggedBnfcIdent "C"
    phrases = sequenceA
        [ TypePhrase 
            <$> (ClausePhraseKnot <$> mplBool )
            <*> mkNullTaggedBnfcIdent mplBoolPhraseTrue
            <*> sequenceA []
            <*> (TypeVar <$> mkNullTaggedBnfcIdent "C")
        , TypePhrase 
            <$> (ClausePhraseKnot <$> mplBool)
            <*> mkNullTaggedBnfcIdent mplBoolPhraseFalse
            <*> sequenceA []
            <*> (TypeVar <$> mkNullTaggedBnfcIdent "C")

        ]
    neighbors = ClausesKnot <$> 
        ( ClausesGraph DataObj 
        <$> (sequenceA $ NE.fromList [mplBool]))
        -}
        -}
