{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module MplParse.Util where

import Optics.TH
import Data.Map (Map)
import qualified Data.Map as Map

data Pos 
    = Pos { 
        -- _absOffset :: !Int
        _row :: !Int
        , _col :: !Int 
    }
    | NoPos 
  deriving (Show, Eq, Ord)

$(makeLenses ''Pos)

data Span = Span 
    { _lowerSpan :: !Pos 
    , _upperSpan :: !Pos
    }
  deriving (Show, Eq, Ord)
$(makeLenses ''Span)

minPos :: Pos -> Pos -> Pos
{-
minPos l@(Pos offset0 _ _) r@(Pos offset1 _ _) 
    | offset0 == maxoffset =  l
    | otherwise = r
  where
    maxoffset = min offset0 offset1
-}
minPos (Pos r0 c0) (Pos r1 c1) 
    | r0 < r1 = Pos r0 c0
    | r0 == r1 = Pos r0 (min c0 c1)
    | otherwise = Pos r1 c1
minPos l NoPos = l
minPos NoPos r = r

maxPos :: Pos -> Pos -> Pos
{-
maxPos l@(Pos offset0 _ _) r@(Pos offset1 _ _) 
    | offset0 == maxoffset =  l
    | otherwise = r
  where
    maxoffset = max offset0 offset1
-}
maxPos (Pos r0 c0) (Pos r1 c1) 
    | r0 > r1 = Pos r0 c0
    | r0 == r1 = Pos r0 (max c0 c1)
    | otherwise = Pos r1 c1
maxPos l NoPos = l
maxPos NoPos r = r

-- TODO: Quick check tests so that this really is a monoid.
instance Semigroup Span where
    Span l0 r0 <> Span l1 r1 = Span 
        (minPos l0 l1) 
        (maxPos r0 r1)

instance Monoid Span where
    mempty = Span NoPos NoPos

data Spanned a = Spanned 
    { _spannedSpan :: !Span 
    , _spanned :: a
    }
  deriving (Show, Eq, Functor)

mapSpanned :: 
    (a -> b) ->
    Spanned a ->
    Spanned b
mapSpanned = fmap


$(makeLenses ''Spanned)
