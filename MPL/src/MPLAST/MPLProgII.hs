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

data DefIdent = DefIdent {
    _defIdent :: UniqueTag
    , _defBnfcIdent :: BnfcIdent
} deriving (Show, Eq, Read, Ord)

data VarIdent = VarIdent {
    _varIdent :: UniqueTag
    , _varBnfcIdent :: BnfcIdent
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

instance HasBnfcIdent DefIdent where
    bnfcIdent = defBnfcIdent

instance HasBnfcIdent VarIdent where
    bnfcIdent = varBnfcIdent

instance HasBnfcIdent ConcVarIdent where
    bnfcIdent = concVarBnfcIdent

instance HasUniqueTag DefIdent where
    uniqueTag = defIdent

instance HasUniqueTag VarIdent where
    uniqueTag = varIdent

instance HasUniqueTag ConcVarIdent where
    uniqueTag = concVarIdent

-- type ProgII = Prog DefIdent
-- type StmtII = Stmt DefnII
-- type PatternII = Pattern DefIdent VarIdent

{-
data DefnII =
    DataDefnII  { 
            _seqTypeClauseII :: NonEmpty (SeqTypeClause DefIdent VarIdent) 
        }
    | CodataDefnII  { 
            _seqTypeClauseII :: NonEmpty (SeqTypeClause DefIdent VarIdent) 
        }
    | ProtocolDefnII  { _concTypeClauseII :: NonEmpty (ConcTypeClause DefIdent VarIdent) }
    | CoprotocolDefnII  { _concTypeClauseII :: NonEmpty (ConcTypeClause DefIdent VarIdent) }

    | FunctionDefnII (FunctionDefn PatternII StmtII DefIdent VarIdent)

    | ProcessDefnII (ProcessDefn 
        PatternII 
        StmtII
        DefIdent 
        VarIdent
        ConcVarIdent)
        -}

{-
newtype DefnII = DefnII (Defn (Pattern DefIdent VarIdent) (Stmt DefnII) DefIdent VarIdent ConcVarIdent)
data Defn pattern letdef def var concvar =
    DataDefn{ 
            _seqTypeClause:: NonEmpty (SeqTypeClause def var) 
        }
    | CodataDefn{ 
            _seqTypeClause:: NonEmpty (SeqTypeClause def var) 
        }
    | ProtocolDefn{ _concTypeClause:: NonEmpty (ConcTypeClause def var) }
    | CoprotocolDefn{ _concTypeClause:: NonEmpty (ConcTypeClause def var) }

    | FunctionDefn(FunctionDefn pattern letdef def var)

    | ProcessDefn (ProcessDefn 
        pattern 
        letdef
        def 
        var
        concvar)
        -}

$( concat <$> traverse makePrisms
    [ ''Polarity ]
 )

