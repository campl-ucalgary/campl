{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MplPasses.PatternCompiler.PatternCompileMplExprSub where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled

import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except

import Data.Functor.Foldable
import Data.Coerce
import Unsafe.Coerce

import Data.Void




-- | Substitutes an id in place of another id in an expression.
substituteVarIdent :: 
    -- | @(a,b)@ is @a maps to b@
    (IdP MplTypeChecked, IdP MplTypeChecked) ->
    -- | Expression to substitute
    MplExpr MplTypeChecked ->
    MplExpr MplTypeChecked
substituteVarIdent sub = cata go 
  where
    go :: MplExprF (MplTypeChecked) (MplExpr MplTypeChecked) -> MplExpr MplTypeChecked
    go = \case
        EVarF ann ident | ident == sub ^. _1 -> _EVar # (ann, sub ^.  _2)
        ELetF ann lets expr -> ELet ann (fmap f lets) expr
          where
            f :: MplStmt MplTypeChecked -> MplStmt MplTypeChecked 
            f stmt = stmt 
                & stmtWhereBindings % mapped %~ f
                & stmtDefns % mapped %~ g

            g :: MplDefn MplTypeChecked -> MplDefn MplTypeChecked
            g obj@(ObjectDefn _) = obj
            g (FunctionDefn fun) = FunctionDefn $ fun
                & funDefn % mapped % _2 %~ substituteVarIdent sub
            g (ProcessDefn proc) = error "pattern matching compilation for processes is not in yet"
            {-
            g (ProcessDefn proc) = ProcessDefn $ proc
                & funDefn % mapped % _2 %~ substituteVarIdent sub
            -}

        n -> embed n
  {-
  where
    go :: MplExprF (MplPass 'TypeChecked) (MplExpr MplExprSub) -> MplExpr MplExprSub
    go = \case 
        EPOpsF ann primop l r -> EPOps ann primop l r
        EVarF ann ident -> EVar (snd ann) ident
        EIntF ann n -> EInt ann n
        ECharF ann v -> EChar ann v
        EDoubleF ann v -> EDouble ann v
        ECaseF ann expr cases -> ECase ann expr cases
        EObjCallF ann ident exprs -> EObjCall ann ident exprs
        ECallF ann ident exprs -> ECall ann ident exprs 
        ERecordF ann phrases -> ERecord ann phrases
        EListF ann exprs -> EList ann exprs
        EStringF ann str -> EString ann str
        EUnitF ann -> EUnit ann
        ETupleF ann res -> ETuple ann res
        EBuiltInOpF ann op l r -> EBuiltInOp ann op l r
        EIfF ann iff thenf elsef -> EIf ann iff thenf elsef
        ELetF ann lets expr -> ELet ann (fmap f lets) expr
          where
            f :: MplStmt MplTypeChecked -> MplStmt MplExprSub
            f stmt = undefined

        EFoldF ann foldon phrases -> EFold ann foldon phrases
        EUnfoldF ann expr phrases -> EUnfold ann expr phrases
        ESwitchF ann res -> ESwitch ann res
        XExprF ann -> XExpr ann
        -}
