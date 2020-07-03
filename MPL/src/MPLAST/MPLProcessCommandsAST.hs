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

type ProcessCommands letdef def var = NonEmpty (ProcessCommand letdef def var)

data ProcessCommand letdef def var =
    CRun { _cCalledProcess :: def
        , _cSeqArgs :: [Expr letdef def var]
        , _cInChsArgs :: [var]
        , _cOutChsArgs :: [var] }
    | CClose { _cClose :: var }
    | CHalt { _cHalt :: var }

    | CGet { _cGet :: Pattern def var, _cGetCh :: var }
    | CPut { _cPut :: Expr letdef def var, _cPutCh :: var }

    | CHCase { _cHCase :: Expr letdef def var, _cHCases :: NonEmpty (def, ProcessCommands letdef def var) }
    | CHPut  { _cHPut :: def, _cHPutCh :: var }

    | CSplit  { _cSplit :: var, _cSplitInto :: [var] }
    | CFork  { _cFork :: var, _cForkInto :: [(def, ProcessCommands letdef def var)] }

    | CId { _cIdLarg :: var, _cIdRarg :: var}
    | CIdNeg { _cIdLarg :: var, _cIdNegArg :: var}
    
    | CRace { _cRaces :: [(var, ProcessCommands letdef def var)] }

    | CPlug { _cPlugs :: [ProcessCommands letdef def var] }

    | CCase { _cCase :: Expr letdef def var, _cCases :: [(Pattern def var, ProcessCommands letdef def var)] }
    | CSwitch { _cSwitches :: [(Expr letdef def var, ProcessCommands letdef def var)] }
  deriving ( Read, Show, Generic, Out, Data )
    

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''ProcessCommand
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ProcessCommand ]
 )

$(makeBaseFunctor ''ProcessCommand)
    
