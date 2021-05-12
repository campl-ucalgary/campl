{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module MplAST.MplIdent where

import Optics
import Optics.State.Operators

import Data.Data
import MplUtil.UniqueSupply
import MplUtil.Data.Stream


import Data.Coerce
import Data.Proxy
import Control.Monad.State
import Control.Arrow
import Data.Ix

{- Module that defines data types / utilities for how things 
 - in the AST is identified..
 -}

type family IdP x 

newtype Name = 
    Name String
  deriving (Show, Ord, Eq)

data Location = 
    Location (Int,Int)
    -- old version which was inconsistent
    -- Span (Int, Int) (Int, Int)
  deriving (Show, Eq)

-- | Span for computing bounding boxes of a location.
newtype Span = Span (Location, Location)

memptySpan :: Span
memptySpan = Span (Location (maxBound, maxBound), Location (minBound, minBound))

{- | Semigroup / monoid instances compute the bounding box of the text -}
instance Semigroup Span where
    Span (Location (l00, l01), Location (r00, r01)) <> Span (Location (l10, l11), Location (r10, r11))
        = Span (Location (min l00 l10, min l01 l11), Location (max r00 r10, max r01 r11))

instance Monoid Span where
    mempty = memptySpan

data NameOcc = NameOcc { 
    _nameOccName :: Name 
    , _nameOccLocation :: Location 
}  deriving Show

-- equality should not dependon the location of the name..
instance Eq NameOcc where
    NameOcc a _ == NameOcc b _ = a == b

newtype KeyWordNameOcc = KeyWordNameOcc NameOcc
  deriving Show


data Namespace = 
    TypeLevel
    | TermLevel
    | ChannelLevel
  deriving (Show, Eq, Ord)

newtype UniqueTag = UniqueTag Unique
  deriving (Show, Eq, Ord, Ix)
    

$(concat <$> traverse makeClassy 
    [ ''Name
    , ''NameOcc
    , ''Location
    , ''Namespace
    , ''UniqueTag
    ]
 )
$(concat <$> traverse makePrisms
    [ ''Name
    , ''NameOcc
    , ''Location
    , ''Namespace
    , ''UniqueTag
    ]
 )

instance HasName NameOcc where
    name = nameOccName

instance HasLocation NameOcc where
    location = nameOccLocation

instance HasLocation KeyWordNameOcc where
    location = (coerced :: Iso' KeyWordNameOcc NameOcc) % nameOccLocation

eqUniqueTag :: 
    HasUniqueTag a =>
    a -> a -> Bool
eqUniqueTag a b = a ^. uniqueTag == b ^. uniqueTag

compareUniqueTag :: 
    HasUniqueTag a =>
    a -> a -> Ordering
compareUniqueTag a b = compare (a ^. uniqueTag) (b ^. uniqueTag)


data Polarity = 
    Input
    | Output
  deriving (Show, Eq)
$(makeClassy ''Polarity)
$(makePrisms ''Polarity)

tupleName :: 
    -- | number of elements
    Int -> 
    -- | name of the tuple
    Name 
tupleName n = Name $ "(" ++ replicate (n-1) ',' ++ ")"


-- fold over the polarity
inputOutput :: Polarity -> a -> a -> a
inputOutput Input a _ = a
inputOutput Output _ b = b
