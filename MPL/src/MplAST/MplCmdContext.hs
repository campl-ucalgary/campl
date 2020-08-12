{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MplAST.MplCmdContext where

import Optics
import Optics.State.Operators
import Data.Void

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MplAST.MplParsed
import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplType
import MplAST.MplCmd
import MplAST.MplIdent
import MplAST.MplProg
import MplAST.MplExt
import MplUtil.UniqueSupply 

{- Small intermediate langauge that computes the variable closure
 - of the plug and fork command.
 -
 -}

-- Process Command
type instance XMplCmd MplCmdContext = MplCmd MplCmdContext
type instance XCRun MplCmdContext = ()
type instance XCClose MplCmdContext = IdP MplCmdContext
type instance XCHalt MplCmdContext = IdP MplCmdContext
type instance XCGet MplCmdContext = IdP MplCmdContext
type instance XCPut MplCmdContext = IdP MplCmdContext
type instance XCHCase MplCmdContext = IdP MplCmdContext
type instance XCHPut MplCmdContext = IdP MplCmdContext
type instance XCSplit MplCmdContext = IdP MplCmdContext
type instance XCFork MplCmdContext = IdP MplCmdContext
type instance XCId MplCmdContext = IdP MplCmdContext
type instance XCIdNeg MplCmdContext = IdP MplCmdContext
type instance XCRace MplCmdContext = IdP MplCmdContext
type instance XCPlug MplCmdContext = Void
type instance XCPlugs MplCmdContext = IdP MplCmdContext
type instance XCCase MplCmdContext = IdP MplCmdContext
type instance XCSwitch MplCmdContext = IdP MplCmdContext
type instance XCHCasePhrase MplCmdContext  = ()
type instance XCForkPhrase MplCmdContext  = [(ChP MplCmdContext)] 
type instance XCPlugPhrase MplCmdContext  = [ChP MplCmdContext] 
type instance XXCmd MplCmdContext = Void
