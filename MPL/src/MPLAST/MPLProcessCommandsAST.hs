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

import MPLIdent

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

type ProcessCommands pattern letdef def var concvar = NonEmpty (ProcessCommand pattern letdef def var concvar)

data ProcessCommand pattern letdef def var concvar =
    CRun { _cCalledProcess :: def
        , _cSeqArgs :: [Expr pattern letdef def var]
        , _cInChsArgs :: [concvar]
        , _cOutChsArgs :: [concvar] }
    | CClose { _cClose :: concvar }
    | CHalt { _cHalt :: concvar }

    | CGet { _cGet :: pattern, _cGetCh :: concvar }
    | CPut { _cPut :: Expr pattern letdef def var, _cPutCh :: concvar }

    | CHCase { _cHCase :: concvar, _cHCases :: NonEmpty (def, ProcessCommands pattern letdef def var concvar) }
    | CHPut  { _cHPut :: def, _cHPutCh :: concvar }

    | CSplit  { _cSplit :: concvar, _cSplitInto :: (concvar, concvar) }
    | CFork  { _cFork :: var, _cForkInto :: 
        ( (concvar, [concvar], ProcessCommands pattern letdef def var concvar)
        , (concvar, [concvar], ProcessCommands pattern letdef def var concvar) ) }

    | CId { _cIdLarg :: concvar, _cIdRarg :: concvar}
    | CIdNeg { _cIdLarg :: concvar, _cIdNegArg :: concvar}
    
    | CRace { _cRaces :: NonEmpty (concvar, ProcessCommands pattern letdef def var concvar) }

    | CPlug { _cPlugs :: [concvar]
        , _cPlugged :: [([concvar], ProcessCommands pattern letdef def var concvar)] }

    | CCase { _cCase :: Expr pattern letdef def var
        , _cCases :: [(pattern, ProcessCommands pattern letdef def var concvar)] }
    | CSwitch { _cSwitches :: NonEmpty (Expr pattern letdef def var, ProcessCommands pattern letdef def var concvar) }
  deriving ( Read, Show, Generic, Out, Data, Eq )
    

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''ProcessCommand
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ProcessCommand ]
 )

$(makeBaseFunctor ''ProcessCommand)
    
