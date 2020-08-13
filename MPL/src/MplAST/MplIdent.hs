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

type family IdP x 

newtype Name = 
    Name String
  deriving (Show, Ord, Eq)

data Location = 
    Location (Int,Int)
    | Span (Int, Int) (Int, Int)
  deriving (Show, Eq)

data NameOcc = NameOcc { 
    _nameOccName :: Name 
    , _nameOccLocation :: Location 
}  deriving Show

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


inputOutput :: Polarity -> a -> a -> a
inputOutput Input a _ = a
inputOutput Output _ b = b
