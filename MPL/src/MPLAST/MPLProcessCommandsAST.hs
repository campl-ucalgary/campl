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

type ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident = NonEmpty (ProcessCommand pattern letdef typedef seqcalleddef conccalleddef ident chident)

data ProcessCommand pattern letdef typedef 
    seqcalleddef 
    conccalleddef 
    ident 
    chident =
    CRun { _cCalledProcess :: ident
        , _cSeqArgs :: [Expr pattern letdef typedef seqcalleddef ident]
        , _cInChsArgs :: [ident]
        , _cOutChsArgs :: [ident] }
    | CClose { _cClose :: chident }
    | CHalt { _cHalt :: chident }

    | CGet { _cGet :: pattern, _cGetCh :: chident }
    | CPut { _cPut :: Expr pattern letdef typedef seqcalleddef ident, _cPutCh :: chident }

    | CHCase { _cHCase :: ident, _cHCases :: NonEmpty (ident, conccalleddef, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) }
    | CHPut  { _cHPut :: ident, _cHPutDef :: conccalleddef , _cHPutCh :: chident }

    | CSplit  { _cSplit :: chident, _cSplitInto :: (chident, chident) }
    | CFork  { _cFork :: ident, _cForkInto :: 
        ( (chident, [chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)
        , (chident, [chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) ) }

    | CId { _cIdLarg :: chident, _cIdRarg :: chident}
    | CIdNeg { _cIdLarg :: chident, _cIdNegArg :: chident}
    
    | CRace { _cRaces :: NonEmpty (chident, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) }

    | CPlug { _cPlugs :: [chident]
        , _cPlugged :: [([chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)] }

    | CCase { _cCase :: Expr pattern letdef typedef seqcalleddef ident
        , _cCases :: [(pattern, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)] }
    | CSwitch { _cSwitches :: NonEmpty (Expr pattern letdef typedef seqcalleddef ident, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) }
  deriving ( Read, Show, Generic, Out, Data, Eq )
    

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''ProcessCommand
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ProcessCommand ]
 )

$(makeBaseFunctor ''ProcessCommand)
    
