{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module MplPasses.PatternCompiler.PatternCompileUtils where

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

import Data.Coerce

import MplPasses.PatternCompiler.PatternCompileErrors

-- We really only need the unique supply
type PatternCompileEnv = Env () ()

type PatternCompile typechecked patterncompiled = 
    forall err m.
    ( MonadState PatternCompileEnv m 
    , MonadError [err] m 
    , AsPatternCompileErrors err )  => 
    typechecked -> m patterncompiled

freshExprVar :: 
    ( HasUniqueSupply s
    , MonadState s m ) =>
    MplSeqType MplTypeChecked ->
    m (MplExpr MplPatternCompiled)
freshExprVar ty = do
    uniq <- fmap uniqueFromSupply freshUniqueSupply
    return $ _EVar # (ty, undefined)


-- | Gets the type of a pattern
getExprType ::
    MplExpr MplTypeChecked -> 
    MplSeqType MplTypeChecked
getExprType = \case 
    EPOps ann _op _l _r -> ann
    EVar ann _ -> ann
    EInt ann _ -> snd ann
    EChar ann _ -> snd ann
    EDouble ann _ -> snd ann
    ECase ann _ _ -> ann
    EObjCall ann _ _ -> ann
    ECall ann _ _ -> ann
    ERecord ann _ -> snd ann
    EList ann _ -> snd ann
    EString ann _ -> snd ann
    EUnit ann -> snd ann
    ETuple ann _ -> snd ann
    EBuiltInOp ann _op _l _r -> snd ann
    EIf ann _iff _thenf _elsef -> ann
    ELet _ann _ expr -> getExprType expr
    EFold ann _ _ -> ann
    EUnfold ann _ _ -> ann
    ESwitch ann _ -> ann
    -- XExpr !(XXExpr x)
    

-- | Gets the type of a pattern
getPattType ::
    MplPattern MplTypeChecked -> 
    MplSeqType MplTypeChecked
getPattType = \case
    PConstructor ann _ _ -> snd ann
    PRecord ann _ -> snd ann
    PVar ann _ -> ann
    PNull ann -> snd ann

    PUnit ann -> snd ann
    PTuple ann _ -> snd ann
    PString ann _ -> snd ann
    PInt ann _ -> snd ann
    PChar ann _ -> snd ann
    PList ann _ -> snd ann
    PListCons ann _ _ -> snd ann

    {-
    \case
    PConstructor ann _ _ -> snd ann
    PRecord ann _ -> snd ann
    PVar ann _ -> ann
    PNull ann -> snd ann

    PUnit ann -> snd ann
    PTuple ann _ -> snd ann
    PString ann _ -> snd ann
    PInt ann _ -> snd ann
    PChar ann _ -> snd ann
    PList ann _ -> snd ann
    PListCons ann _ _ -> snd ann
    -}

-- | Creates a fresh IdP 
freshIdP :: 
    ( MonadState s m
    , HasUniqueSupply s ) =>
    m (IdP MplTypeChecked)
freshIdP = do
    uniq <- fmap uniqueFromSupply freshUniqueSupply
    let ident = _IdentR # (_IdentP # (_NameOcc # (coerce "u", invalidLocation), TermLevel), coerce uniq)
    return ident
