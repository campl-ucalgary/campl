{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
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
module MplAST.MplCmdBinders where

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
 - of the plug and fork command. This is put immediately before
 - renaming...
 -}

data MplCmdbinders

type instance IdP MplCmdBinders = IdP MplParsed
type instance ChP MplCmdBinders = ChP MplParsed

type instance XMplExpr MplCmdBinders = XMplExpr MplParsed
type instance XMplPattern MplCmdBinders = XMplPattern MplParsed

-- Process Command
type instance XMplCmd MplCmdBinders = MplCmd MplCmdBinders
type instance XCRun MplCmdBinders = XCRun MplParsed
type instance XCClose MplCmdBinders = IdP MplCmdBinders
type instance XCHalt MplCmdBinders = IdP MplCmdBinders
type instance XCGet MplCmdBinders = IdP MplCmdBinders
type instance XCPut MplCmdBinders = IdP MplCmdBinders
type instance XCHCase MplCmdBinders = IdP MplCmdBinders
type instance XCHPut MplCmdBinders = IdP MplCmdBinders
type instance XCSplit MplCmdBinders = IdP MplCmdBinders
type instance XCFork MplCmdBinders = IdP MplCmdBinders
type instance XCId MplCmdBinders = IdP MplCmdBinders
type instance XCIdNeg MplCmdBinders = IdP MplCmdBinders
type instance XCRace MplCmdBinders = IdP MplCmdBinders
type instance XCPlug MplCmdBinders = Void
type instance XCPlugs MplCmdBinders = (IdP MplCmdBinders, [IdP MplCmdBinders])
    -- plug command, plug scoped bound variables.
type instance XCCase MplCmdBinders = IdP MplCmdBinders
type instance XCSwitch MplCmdBinders = IdP MplCmdBinders
type instance XCHCasePhrase MplCmdBinders  = ()
type instance XCForkPhrase MplCmdBinders  = [IdP MplCmdBinders] 
type instance XCPlugPhrase MplCmdBinders  = [IdP MplCmdBinders] 
type instance XXCmd MplCmdBinders = Void


