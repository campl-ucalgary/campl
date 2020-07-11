{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module MPLAST.MPLProcessCommandsAST where

import MPLUtil.Data.List.NonEmpty

import Optics.TH
import Optics.Prism
import Optics.Operators
import Data.Functor.Foldable.TH

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE 

import MPLAST.MPLExprAST
import MPLAST.MPLPatternAST

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )
--------------------------
-- Expr definition
--------------------------

type ProcessCommands pattern letdef typedef calleddef ident = NonEmpty (ProcessCommand pattern letdef typedef calleddef ident)

data ProcessCommand pattern letdef typedef calleddef ident =
    CRun { _cCalledProcess :: ident
        , _cSeqArgs :: [Expr pattern letdef typedef calleddef ident]
        , _cInChsArgs :: [ident]
        , _cOutChsArgs :: [ident] }
    | CClose { _cClose :: ident }
    | CHalt { _cHalt :: ident }

    | CGet { _cGet :: pattern, _cGetCh :: ident }
    | CPut { _cPut :: Expr pattern letdef typedef calleddef ident, _cPutCh :: ident }

    | CHCase { _cHCase :: ident, _cHCases :: NonEmpty (ident, calleddef, ProcessCommands pattern letdef typedef calleddef ident) }
    | CHPut  { _cHPut :: ident, _cHPutDef :: calleddef , _cHPutCh :: ident }

    | CSplit  { _cSplit :: ident, _cSplitInto :: (ident, ident) }
    | CFork  { _cFork :: ident, _cForkInto :: 
        ( (ident, [ident], ProcessCommands pattern letdef typedef calleddef ident)
        , (ident, [ident], ProcessCommands pattern letdef typedef calleddef ident) ) }

    | CId { _cIdLarg :: ident, _cIdRarg :: ident}
    | CIdNeg { _cIdLarg :: ident, _cIdNegArg :: ident}
    
    | CRace { _cRaces :: NonEmpty (ident, ProcessCommands pattern letdef typedef calleddef ident) }

    | CPlug { _cPlugs :: [ident]
        , _cPlugged :: [([ident], ProcessCommands pattern letdef typedef calleddef ident)] }

    | CCase { _cCase :: Expr pattern letdef typedef calleddef ident
        , _cCases :: [(pattern, ProcessCommands pattern letdef typedef calleddef ident)] }
    | CSwitch { _cSwitches :: NonEmpty (Expr pattern letdef typedef calleddef ident, ProcessCommands pattern letdef typedef calleddef ident) }
  deriving ( Read, Show, Generic, Out, Data, Eq )
    

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''ProcessCommand
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ProcessCommand ]
 )

$(makeBaseFunctor ''ProcessCommand)
    
