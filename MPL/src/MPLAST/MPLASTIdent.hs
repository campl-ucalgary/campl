{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module MPLAST.MPLASTIdent where

import Optics
import Optics.State.Operators

import Data.Data
import MPLUtil.UniqueSupply
import MPLUtil.Data.Stream
import MPLAST.MPLProg

import Data.Coerce
import Control.Monad.State
import Control.Arrow

type family Pos a where
    Pos BnfcIdent = (Int,Int)
    Pos BnfcIdent = (Int,Int)

newtype BnfcIdent = BnfcIdent { _stringPos :: (String, (Int, Int)) }
  deriving (Show, Read, Data)


data TaggedBnfcIdent = TaggedBnfcIdent {
    _taggedBnfcIdentBnfcIdent :: BnfcIdent
    , _taggedBnfcIdentTag :: UniqueTag
} deriving (Show, Read)

-- equality of tagged bnfcidents should depend only 
-- on equality of the unique tag
instance Eq TaggedBnfcIdent where
    TaggedBnfcIdent _ a == TaggedBnfcIdent _ b =  a == b

instance Ord TaggedBnfcIdent where
    TaggedBnfcIdent _ a <= TaggedBnfcIdent _ b =  a <= b

instance Eq BnfcIdent where
    BnfcIdent (str0, _) == BnfcIdent (str1, _) = str0 == str1 


newtype UniqueTag = UniqueTag { _unUniqueTag :: Unique }
  deriving (Show, Eq, Ord, Read, Enum)

newtype TypeTag = TypeTag UniqueTag
  deriving (Eq, Ord)


instance Show TypeTag where
    show (TypeTag (UniqueTag n)) = show n

data TaggedChIdent = TaggedChIdent {
    _taggedChIdentTaggedBnfcIdent :: TaggedBnfcIdent
    , _taggedChIdentPolarity :: Polarity
} deriving Show


$(makeClassy ''TaggedBnfcIdent)
$(makeClassy ''UniqueTag)
$(makeClassy ''BnfcIdent)
$(makeClassy ''TaggedChIdent)
$(makePrisms ''TaggedBnfcIdent)
$(makePrisms ''TaggedChIdent)

bnfcIdentName :: HasBnfcIdent a => Lens' a String
bnfcIdentName = lens get set
  where
    get n = n ^. stringPos % _1
    set n v = n & stringPos % _1 .~ v

bnfcIdentPos :: HasBnfcIdent a => Lens' a (Int, Int)
bnfcIdentPos = lens get set
  where
    get n = n ^. stringPos % _2
    set n v = n & stringPos % _2 .~ v


freshUniqueTag ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m UniqueTag
freshUniqueTag = 
    uniqueSupply %%= (first (UniqueTag . uniqueFromSupply) <<< split)

freshUniqueTags :: 
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m (Stream UniqueTag)
freshUniqueTags = do
    supply <- freshUniqueSupply
    return $ coerce $ uniquesFromSupply supply

freshTypeTags :: 
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m (Stream TypeTag)
freshTypeTags = coerce <$> freshUniqueTags

freshUniqueSupply ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m UniqueSupply
freshUniqueSupply =
    uniqueSupply %%= split

tagBnfcIdent ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    BnfcIdent ->
    m TaggedBnfcIdent
tagBnfcIdent ident = do
    review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag

freshTypeTag ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m TypeTag
freshTypeTag = TypeTag <$> freshUniqueTag

taggedBnfcIdentName :: Lens' TaggedBnfcIdent String
taggedBnfcIdentName = lens get set
  where
    get n = n ^. taggedBnfcIdentBnfcIdent % bnfcIdentName
    set n v = n & taggedBnfcIdentBnfcIdent % bnfcIdentName .~ v

taggedBnfcIdentPos :: Lens' TaggedBnfcIdent (Int, Int)
taggedBnfcIdentPos =  lens get set
  where
    get n = n ^. taggedBnfcIdentBnfcIdent % bnfcIdentPos
    set n v = n & taggedBnfcIdentBnfcIdent % bnfcIdentPos .~ v

instance HasUniqueTag TaggedBnfcIdent where
    uniqueTag = taggedBnfcIdentTag 

instance HasUniqueTag TaggedChIdent where
    uniqueTag = taggedChIdentTaggedBnfcIdent % taggedBnfcIdentTag 

instance HasBnfcIdent TaggedBnfcIdent where
    bnfcIdent = taggedBnfcIdentBnfcIdent 

instance HasTaggedBnfcIdent  TaggedChIdent where
    taggedBnfcIdent = taggedChIdentTaggedBnfcIdent 

instance HasBnfcIdent TaggedChIdent where
    bnfcIdent = taggedChIdentTaggedBnfcIdent % taggedBnfcIdentBnfcIdent


