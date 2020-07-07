{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MPLAST.MPLProgII where

import Optics
import MPLUtil.Optics.TH

import MPLAST.MPLASTCore
import MPLAST.MPLProgI

import Data.List.NonEmpty

data Polarity = 
    Input
    | Output
  deriving (Show, Eq, Read, Ord)

newtype DefIdent = DefIdent UniqueTagAndBnfcIdent 
  deriving (Show, Eq, Read, Ord)

newtype VarIdent = VarIdent UniqueTagAndBnfcIdent 
  deriving (Show, Eq, Read, Ord)

data UniqueTagAndBnfcIdent = UniqueTagAndBnfcIdent {
    _uniqueTagAndBnfcIdentUniqueTag :: UniqueTag
    , _uniqueTagAndBnfcIdentBnfcIdent :: BnfcIdent
} deriving (Show, Eq, Read, Ord)

data ConcVarIdent = ConcVarIdent {
    _concVarIdent :: UniqueTag
    , _concVarBnfcIdent :: BnfcIdent
    , _concVarPolarity :: Polarity
} deriving (Show, Eq, Read, Ord)

newtype UniqueTag = UniqueTag Int
  deriving (Show, Eq, Ord, Read, Enum)

$(makeClassy ''UniqueTag)

$(concat <$> traverse makeLenses
    [ ''DefIdent
    , ''VarIdent
    , ''ConcVarIdent
    ]
 )
$(concat <$> traverse makePrisms
    [ ''DefIdent
    , ''VarIdent
    , ''ConcVarIdent
    ]
 )

-- type ProgII = Prog DefIdent
-- type PatternII = Pattern DefIdent VarIdent

type StmtII = Stmt DefnII
newtype DefnII = DefnII  {
        _unDefnII :: Defn (Pattern DefIdent VarIdent) (Stmt DefnII) 
            DefIdent DefIdent VarIdent VarIdent ConcVarIdent
    }

$( makeLenses ''DefnII )
$( concat <$> traverse makePrisms
    [ ''Polarity ]
 )

