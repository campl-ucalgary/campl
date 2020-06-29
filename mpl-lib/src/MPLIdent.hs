{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module MPLIdent where

import Data.Word

import Optics.Operators
import Optics.Optic
import Optics.Iso
import Optics.TH
import Optics.Re
import Data.Tuple.Optics

import Text.PrettyPrint.GenericPretty

data RowColPos = RowColPos {
        _row :: Int
        , _col :: Int
    } deriving (Read, Eq, Show, Generic, Out)

-- row col position of a definition
newtype DefRowColPos = DefRowColPos RowColPos
  deriving (Read, Eq, Show, Generic, Out)

newtype InstrIdent = InstrIdent Ident
    deriving (Read, Eq, Show, Generic, Out)

newtype NumArgs = NumArgs Word
  deriving (Read, Eq, Show)

data Ident = Ident {
    _name :: String
    , _pos :: RowColPos 
} deriving (Read, Eq, Show, Generic, Out)


$( concat <$> traverse makeLenses
    [ ''RowColPos
    , ''Ident ]
 )

lineColIso :: Iso' (Int, Int) RowColPos
lineColIso = iso to' from'
  where
    to' (a, b) = RowColPos { _row = a, _col = b }
    from' RowColPos { _row = a, _col = b } = (a,b)

identIso :: Iso' (String, RowColPos) Ident
identIso = iso to' from'
  where
    to' (str, pos) = Ident { _name = str, _pos = pos }
    from' n = (n ^. name, n ^. pos )

bnfcIdentIso :: Iso' ((Int,Int), String) Ident
bnfcIdentIso = iso to' from'
  where
    to' n = (n & _1 %~ (^. lineColIso )) ^. swapped % identIso
    from' n = (n ^. pos % re lineColIso , n ^. name)


nullTok :: Ident
nullTok = ("", nullRowColPos) ^. identIso

nullRowColPos :: RowColPos
nullRowColPos = (-1,-1) ^. lineColIso
