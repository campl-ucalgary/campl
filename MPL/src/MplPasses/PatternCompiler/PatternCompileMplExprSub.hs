{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
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
import MplAST.MplProgUtil

import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Arrow

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except

import Control.Exception
import Data.Functor.Foldable
import Data.Coerce
import Unsafe.Coerce
import MplPasses.PatternCompiler.PatternCompileUtils

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Tuple

import Data.Void
import Debug.Trace

data Substitutable
     -- | substitution for a variable.
     = VarSub (IdentT, XMplType MplTypeChecked)
     -- | substitution for a a record. The datum is as follows.
     --     - @u@ in the pattern matching algorithm
     --     - type phrase of the destructor / name of the pattern destructor
     | RecordSub 
        -- (IdentT, XMplType MplTypeChecked) 
        Substitutable
        ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
        , IdentT
        )
     -- | substitution for a tuple. The data is as follows.
     --     - @u@ in the pattern matching algorithm
     --     - Type of the resulting projected element / projection number e.g. \pi_0, \pi_1, ..
     | TupleSub 
        Substitutable
        (XMplType MplTypeChecked, Int)
$(makePrisms ''Substitutable)

-- | gets the expresion out of a 'Substitutable'.
getExprFromSubstitutable ::
    Substitutable -> 
    MplExpr MplPatternCompiled
getExprFromSubstitutable = \case 
    VarSub val -> _EVar # swap val
    RecordSub u (phrase, des) -> assert (has (typePhraseFrom % _1 % _Empty) phrase) $ 
        -- TODO: this get type isn't totally correct -- i.e. if the type is specialized from the actual type, this
        -- won't have the specializaiotn e.g. Type should be @MyType(Int)@ but this will say it is @MyType(A)@
        EObjCall (CodataDefn phrase, getCodataPhraseTypeResult phrase) des $ [getExprFromSubstitutable u]

    TupleSub  u (ty, proj) -> EProj ty proj (getExprFromSubstitutable u)

-- | @substitute (s, t)@ replaces all occurances of @s@ by @t@; the substitution
-- is given by the 'Substitutable'
substitute  ::
    MapMplExpr t MplPatternCompiled =>
    (IdP MplPatternCompiled, Substitutable) ->
    t MplPatternCompiled ->
    t MplPatternCompiled
substitute (s, VarSub t) = substituteVarIdentByExpr (s, _EVar # swap t)
substitute (s, RecordSub u t) = substituteCallIdentByRecord (s, (u,t))
substitute (s, TupleSub u t) = substituteVarIdentByTuple (s, (u,t))

substituteExpr :: 
    (IdP MplPatternCompiled, Substitutable) ->
    MplExpr MplPatternCompiled ->
    MplExpr MplPatternCompiled
substituteExpr = substitute

-- | Substitutes a record for a destructor
substituteCallIdentByRecord ::
    MapMplExpr t MplPatternCompiled =>
    ( IdentT
    , 
        ( Substitutable
            -- @u@
        , (MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag), IdentT)
            -- destructor information
        )
    ) 
    -> t MplPatternCompiled
    -> t MplPatternCompiled
substituteCallIdentByRecord sub@(s, (u, (phrase, des))) = mapMplExpr k
  where
    uexpr = getExprFromSubstitutable u
    k = \case
            EVar ann ident | ident == s -> 
                EObjCall (CodataDefn phrase, ann) des $ [uexpr]
            ECall ann ident args | ident == s -> 
                EObjCall (CodataDefn phrase, ann) des $ args ++ [uexpr]
            n -> n

-- | Substitutes a 'EVar' by an expression.
substituteVarIdentByExpr ::
    MapMplExpr t MplPatternCompiled =>
    (IdP MplPatternCompiled, MplExpr MplPatternCompiled) ->
    t MplPatternCompiled ->
    t MplPatternCompiled
substituteVarIdentByExpr sub = mapMplExpr k
  where
    k = \case
            EVar ann ident | ident == sub ^. _1 -> sub ^. _2
            n -> n

-- | substitutes tuples
substituteVarIdentByTuple :: 
    MapMplExpr t MplPatternCompiled =>
    (IdP MplPatternCompiled, (Substitutable, (XMplType MplTypeChecked, Int))) ->
    t MplPatternCompiled ->
    t MplPatternCompiled
substituteVarIdentByTuple sub@(s, t) = mapMplExpr k
  where
    k = \case
        EVar ann ident | ident == sub ^. _1 -> 
            getExprFromSubstitutable (uncurry TupleSub t)
        n -> n

-- | substitute channel names (rename this module to just substitutions)
substituteCh :: 
    -- | @(s,t)@ means substitute all occurences of @s@ by @t@
    (ChP MplPatternCompiled, ChP MplPatternCompiled) -> 
    MplCmd MplPatternCompiled -> 
    MplCmd MplPatternCompiled 
substituteCh sub@(s,t) = cata go 
  where
    subMap ch 
        | ch == s = t
        | otherwise = ch

    go = \case 
        CRunF ann idp seqs ins outs ->
            CRun ann idp seqs (map subMap ins) (map subMap outs) 
        CCloseF ann ch -> CClose ann $ subMap ch
        CHaltF ann ch -> CHalt ann $ subMap ch
        CGetF ann patt ch -> CGet ann patt $ subMap ch
        CPutF ann expr ch -> CPut ann expr $ subMap ch
        CHCaseF ann ch cmds ->
            CHCase ann (subMap ch) cmds'
          where
            cmds' = cmds & mapped % _3 % mapped %~ substituteCh sub
        CHPutF ann idp ch ->
            CHPut ann idp (subMap ch)
        CSplitF ann ch (lch, rch) -> 
            CSplit ann (subMap ch) (subMap lch, subMap rch) 
        CForkF ann ch (lphrase, rphrase) ->
            CFork ann (subMap ch) (f lphrase, f rphrase)
          where
            f (ch, chs, cmds) = 
                ( subMap ch
                , map subMap chs
                , fmap (substituteCh sub) cmds
                )
        CIdF ann (lch, rch) ->
            CId ann (subMap lch, subMap rch)
        CIdNegF ann (lch, rch) ->
            CIdNeg ann (subMap lch, subMap rch) 
        CRaceF ann cmds ->
            CRace ann $ fmap (subMap *** fmap (substituteCh sub)) cmds
        -- CPlug !(XCPlug x) (CPlugPhrase x, CPlugPhrase x)
        CPlugsF ann (phrase0, phrase1, phrases) ->
            CPlugs ann (f phrase0, f phrase1, map f phrases)
          where
            f (ann', (lch, rch), cmds) = 
                (ann', (map subMap lch, map subMap rch), fmap (substituteCh sub) cmds)
        CCaseF ann expr cmds -> 
            CCase ann expr $ fmap (second $ fmap $ substituteCh sub) cmds

        CIfF ann expr thenc elsec -> 
            CIf ann expr (fmap (substituteCh sub) thenc) (fmap (substituteCh sub) elsec)
        CIllegalInstrF ann -> CIllegalInstr ann

