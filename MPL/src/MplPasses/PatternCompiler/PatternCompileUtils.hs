{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.List
import Data.Coerce
import Data.Functor.Foldable (cata)
import Data.Foldable

import MplPasses.PatternCompiler.PatternCompileErrors

import Data.Functor.Identity

{- | 
We mainly just need the unique supply, but we also need the context of 
variables in scope and channels in scope when compiling a process so we 
can memoize the case scrutinee by passing it in as an argument to a process.
-}

type ProcessCompileContext = 
     -- | sequentials, input channels, output channels
     ( [MplPattern MplPatternCompiled]
     , [ChP MplPatternCompiled]
     , [ChP MplPatternCompiled]
     )

type PatternCompileEnv = Env () ProcessCompileContext

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
    EBool ann _ -> ann
    EDouble ann _ -> ann
    ECase ann _ _ -> ann
    EObjCall ann _ _ -> snd ann
    ECall ann _ _ -> ann
    ERecord ann _ -> ann
    EList ann _ -> ann
    EString ann _ -> ann
    EUnit ann -> ann
    ETuple ann _ -> ann
    EIf ann _ _ _ -> ann
    EProj ann _ _ -> ann
    -- TODO: Should actually just store the type in the annotation information
    ELet _ann _ expr -> getExprType expr
    EIllegalInstr ann  -> ann
    {-
    EList ann _ -> snd ann
    EString ann _ -> snd ann
    EUnit ann -> snd ann
    ETuple ann _ -> snd ann
    EBuiltInOp ann _op _l _r -> snd ann
    EIf ann _iff _thenf _elsef -> ann
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
-- Why does this need more thought? Well the problem is that sometimes the 
-- type of the typephrase has substitutions which change the type from the
-- most general type.. this is honestly more of a shortcoming with the design of
-- the language than an issue on this end... ideally, a systemF like representation
-- or something would be nice so we wouldn't have to worry about these issues.
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
    PBool ann _ -> snd ann
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

freshIdP ::
    ( MonadState s m
    , HasUniqueSupply s ) =>
    String ->
    m (IdP MplPatternCompiled)
freshIdP str = do
    uniq <- fmap uniqueFromSupply freshUniqueSupply
    let ident = _IdentR # (_IdentP # (_NameOcc # (coerce str, invalidLocation), TermLevel), coerce uniq)
    return ident

-- | Creates a fresh IdP 
freshUIdP :: 
    ( MonadState s m
    , HasUniqueSupply s ) =>
    m (IdP MplPatternCompiled)
freshUIdP = freshIdP "u"

-- | Creates a fresh IdP 
freshCaseFunIdP :: 
    ( MonadState s m
    , HasUniqueSupply s ) =>
    m (IdP MplPatternCompiled)
freshCaseFunIdP = freshIdP "casef"


-- | Creates a fresh IdP 
freshCaseProcIdP :: 
    ( MonadState s m
    , HasUniqueSupply s ) =>
    m (IdP MplPatternCompiled)
freshCaseProcIdP = freshIdP "casep"


{- | collects all the pattern variables from a pattern and automatically converts it 
to an 'MplPatternCompiled'. This is useful for pattern compiling processes...
-}
collectPattVars :: 
    MplPattern MplTypeChecked ->
    [MplPattern MplPatternCompiled] 
collectPattVars = cata go
  where
    go = \case 
        PVarF ann v -> [PVar ann v]
        n -> fold n

{- | deletes a channel from the context. -}
deleteChFromContext ::
    PatternCompile 
        ChIdentT 
        ()
deleteChFromContext ch = void $ envLcl %= go
  where
    go = case ch ^. polarity of 
        Input  -> over (_2) (delete ch)
        Output -> over (_3) (delete ch)

{- | adds a channel from the context. -}
insertChInContext ::
    PatternCompile 
        ChIdentT 
        ()
insertChInContext ch = void $ envLcl %= go
  where
    go = case ch ^. polarity of 
        Input  -> over (_2) (ch:)
        Output -> over (_3) (ch:)

{- | adds a pattern in the context -}
insertPattInContext ::
    PatternCompile 
        (MplPattern MplPatternCompiled)
        ()
insertPattInContext patt = void $ envLcl % _1 %= (patt:)
