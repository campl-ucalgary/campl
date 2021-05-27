{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Functor.Foldable

import MplPasses.PatternCompiler.PatternCompileErrors

import Data.Functor.Identity

-- We really only need the unique supply
type PatternCompileEnv = Env () ()

type PatternCompile typechecked patterncompiled = 
    forall err m.
    ( MonadState PatternCompileEnv m 
    , MonadWriter [err] m 
    , AsPatternCompileErrors err ) => 
    typechecked -> m patterncompiled



-- | Gets the type of a pattern
getExprType ::
    MplExpr MplPatternCompiled -> 
    XMplType MplTypeChecked
getExprType = \case 
    EPOps ann _op _l _r -> ann
    EVar ann _ -> ann
    EInt ann _ -> ann
    EChar ann _ -> ann
    EDouble ann _ -> ann
    ECase ann _ _ -> ann
    EObjCall ann _ _ -> ann
    ECall ann _ _ -> ann
    ERecord ann _ -> ann
    {-
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
    -}
    -- XExpr !(XXExpr x)

-- | get type from a codata phrase when fully applied to its arguments
getCodataPhraseTypeResult :: 
    MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag) -> 
    (MplType MplTypeChecked)
getCodataPhraseTypeResult phrase = phrase ^. typePhraseTo

-- | get type from a data phrase when fully applied to its arguments
getDataPhraseTypeResult :: 
    MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag) -> 
    (MplType MplTypeChecked)
getDataPhraseTypeResult phrase = 
    TypeSeqWithArgs 
        (phrase ^. typePhraseExt % to DataDefn ) 
        (phrase ^. typePhraseName) 
        -- TODO
        [error "okay I need to give the type preservation some more thought TODO"]


    

-- | Gets the type of a pattern
getPattType ::
    MplPattern MplTypeChecked -> 
    XMplType MplTypeChecked
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
freshUIdP :: 
    ( MonadState s m
    , HasUniqueSupply s ) =>
    m (IdP MplPatternCompiled)
freshUIdP = do
    uniq <- fmap uniqueFromSupply freshUniqueSupply
    let ident = _IdentR # (_IdentP # (_NameOcc # (coerce "u", invalidLocation), TermLevel), coerce uniq)
    return ident


-- | This is used to map over all expressions over a program
class MapPatternCompiledExpr t where
    mapPatternCompiledExpr :: 
        (MplExpr MplPatternCompiled -> MplExpr MplPatternCompiled) -> t -> t

instance TraversePatternCompiledExpr t => MapPatternCompiledExpr t where
    mapPatternCompiledExpr f = runIdentity . traversePatternCompiledExpr k
      where
        k = Identity . f 

{-
instance MapPatternCompiledExpr (MplExpr MplPatternCompiled) where
    mapPatternCompiledExpr k = cata go
      where
        go = \case
            ELetF ann lets expr -> 
                k (ELet ann (fmap f lets) expr)
              where
                f :: MplStmt MplPatternCompiled -> MplStmt MplPatternCompiled 
                f stmt = stmt 
                    & stmtWhereBindings % mapped %~ f
                    & stmtDefns % mapped %~ g

                g :: MplDefn MplPatternCompiled -> MplDefn MplPatternCompiled
                g obj@(ObjectDefn _) = obj
                g (FunctionDefn fun) = FunctionDefn $ fun
                    & funDefn % mapped % _2 %~ mapPatternCompiledExpr k
                g (ProcessDefn proc) = ProcessDefn $ proc
                    & procDefn % mapped % _2 % mapped %~ mapPatternCompiledExpr k
            n -> k $ embed n

instance MapPatternCompiledExpr (MplCmd MplPatternCompiled) where
    mapPatternCompiledExpr k = cata go
      where
        go = \case
            CRunF ann idp seqs ins outs -> 
                CRun ann idp (map (mapPatternCompiledExpr k) seqs) ins outs
            CPutF ann expr chp -> 
                CPut ann (mapPatternCompiledExpr k expr) chp
            CCaseF ann expr cmds -> undefined
                CCase ann (mapPatternCompiledExpr k expr) cmds
            CIfF ann expr thenc elsec -> 
                CIf ann (mapPatternCompiledExpr k expr) thenc elsec
            n -> embed n
-}


class TraversePatternCompiledExpr t where
    traversePatternCompiledExpr :: 
        Monad m =>
        (MplExpr MplPatternCompiled -> m (MplExpr MplPatternCompiled)) -> 
        t -> 
        m t

instance TraversePatternCompiledExpr (MplExpr MplPatternCompiled) where
    traversePatternCompiledExpr k = cata go
      where
        go = \case
            n -> join $ (k . embed) <$> sequenceA n
            ELetF ann lets expr -> do
                lets' <- traverse f lets
                expr' <- expr
                k (ELet ann lets' expr')
              where
                -- f :: Monad m => MplStmt (MplPass 'PatternCompiled) -> m (MplStmt MplPatternCompiled)
                f stmt = traverseOf (stmtWhereBindings % traversed) f stmt
                        >>= traverseOf (stmtDefns % traversed) g

                -- g :: Monad m => MplDefn MplPatternCompiled -> m (MplDefn MplPatternCompiled)
                g obj@(ObjectDefn _) = return obj
                g (FunctionDefn fun) = FunctionDefn <$> 
                    traverseOf (funDefn % traversed % _2) (traversePatternCompiledExpr k) fun
                g (ProcessDefn proc) = ProcessDefn <$> 
                    traverseOf (procDefn % traversed % _2 % traversed) (traversePatternCompiledExpr k) proc

instance TraversePatternCompiledExpr (MplCmd MplPatternCompiled) where
    traversePatternCompiledExpr k = cata go
      where
        go = \case
            CRunF ann idp seqs ins outs -> do
                seqs' <- traverse (traversePatternCompiledExpr k) seqs 
                return $ CRun ann idp seqs' ins outs
            CPutF ann expr chp -> do
                expr' <- traversePatternCompiledExpr k expr
                return $ CPut ann expr' chp
            CCaseF ann expr cmds -> do
                expr' <- traversePatternCompiledExpr k expr
                cmds' <- sequenceOf (traversed % _2 % traversed) cmds
                return $ CCase ann expr' cmds'
            CIfF ann expr thenc elsec -> do
                expr' <- traversePatternCompiledExpr k expr
                thenc' <- sequenceA thenc
                elsec' <- sequenceA elsec
                return $ CIf ann expr' thenc' elsec'
            n -> embed <$> sequenceA n
