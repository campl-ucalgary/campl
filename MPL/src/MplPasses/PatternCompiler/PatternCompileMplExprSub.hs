{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
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

import Data.Tuple

import Data.Void


-- | Substitutes an id in place of another id in an expression. This is used in the compilation
-- of pattern matching algorithm
substituteVarIdentByIdent :: 
    -- | @(a,b)@ is @a maps to b@
    (IdP MplPatternCompiled, IdP MplPatternCompiled) ->
    -- | Expression to substitute
    MplExpr MplPatternCompiled ->
    MplExpr MplPatternCompiled
substituteVarIdentByIdent sub = cata go 
  where
    go :: MplExprF (MplPatternCompiled) (MplExpr MplPatternCompiled) -> MplExpr MplPatternCompiled
    go = \case
        EVarF ann ident | ident == sub ^. _1 -> _EVar # (ann, sub ^.  _2)
        ELetF ann lets expr -> ELet ann (fmap f lets) expr
          where
            f :: MplStmt MplPatternCompiled -> MplStmt MplPatternCompiled 
            f stmt = stmt 
                & stmtWhereBindings % mapped %~ f
                & stmtDefns % mapped %~ g

            g :: MplDefn MplPatternCompiled -> MplDefn MplPatternCompiled
            g obj@(ObjectDefn _) = obj
            g (FunctionDefn fun) = FunctionDefn $ fun
                & funDefn % mapped % _2 %~ substituteVarIdentByIdent sub
            g (ProcessDefn proc) = error "pattern matching compilation for processes is not in yet"
            {-
            g (ProcessDefn proc) = ProcessDefn $ proc
                & funDefn % mapped % _2 %~ substituteVarIdent sub
            -}

        n -> embed n

-- | This is specifically just used to substitute the record patterns out for the compilation of 
-- pattern matching algorithm
substituteCallIdentByRecord :: 
    -- | (variable to replace,(type phrase of the destructor, name of the pattern destructor), @u@ in the pattern matching algorithm). 
    (IdP MplPatternCompiled, 
        ( 
            ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
                -- type phrase of the destructor
            , IdentT
                -- name of the pattern destructor
            )
        , (IdentT, MplSeqType MplTypeChecked)
            -- the @u@ in the pattern matching algorithm
        )
    ) ->
    MplExpr MplPatternCompiled ->
    MplExpr MplPatternCompiled
substituteCallIdentByRecord sub@(_, ((_phrase, des), _)) = cata go 
  where
    uexpr :: MplExpr MplPatternCompiled
    uexpr = _EVar # (sub ^. _2 % _2 % to swap)

    go :: MplExprF (MplPatternCompiled) (MplExpr MplPatternCompiled) -> MplExpr MplPatternCompiled
    go = \case
        EVarF ann ident | ident == sub ^. _1 -> 
            EObjCall ann des $ [uexpr]
        ECallF ann ident args | ident == sub ^. _1 -> 
            EObjCall ann des $ args ++ [uexpr]

        ELetF ann lets expr -> ELet ann (fmap f lets) expr
          where
            f :: MplStmt MplPatternCompiled -> MplStmt MplPatternCompiled 
            f stmt = stmt 
                & stmtWhereBindings % mapped %~ f
                & stmtDefns % mapped %~ g

            g :: MplDefn MplPatternCompiled -> MplDefn MplPatternCompiled
            g obj@(ObjectDefn _) = obj
            g (FunctionDefn fun) = FunctionDefn $ fun
                & funDefn % mapped % _2 %~ substituteCallIdentByRecord sub
            g (ProcessDefn proc) = error "pattern matching compilation for processes is not in yet"
            {-
            g (ProcessDefn proc) = ProcessDefn $ proc
                & funDefn % mapped % _2 %~ substituteVarIdent sub
            -}

        n -> embed n

-- | Substitutes a 'EVar' by an expression. This is mainly used for translating
-- a case expression (not a pattern match) to simple constructors
substituteVarIdentByExpr ::
    (IdP MplPatternCompiled, MplExpr MplPatternCompiled) ->
    MplExpr MplPatternCompiled ->
    MplExpr MplPatternCompiled 
substituteVarIdentByExpr sub = cata go
  where
    go :: MplExprF (MplPass 'PatternCompiled) (MplExpr MplPatternCompiled) -> MplExpr MplPatternCompiled
    go = \case
        EVarF ann ident | ident == sub ^. _1 -> sub ^. _2

        -- duplicated code for the let case
        ELetF ann lets expr -> ELet ann (fmap f lets) expr
          where
            f :: MplStmt MplPatternCompiled -> MplStmt MplPatternCompiled 
            f stmt = stmt 
                & stmtWhereBindings % mapped %~ f
                & stmtDefns % mapped %~ g

            g :: MplDefn MplPatternCompiled -> MplDefn MplPatternCompiled
            g obj@(ObjectDefn _) = obj
            g (FunctionDefn fun) = FunctionDefn $ fun
                & funDefn % mapped % _2 %~ substituteVarIdentByExpr sub
            g (ProcessDefn proc) = error "pattern matching compilation for processes is not in yet"
        res -> embed res
