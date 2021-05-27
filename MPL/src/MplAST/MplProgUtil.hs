{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MplAST.MplProgUtil where

import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplCmd
import MplAST.MplIdent
import MplAST.MplProg
import MplAST.MplType
import MplAST.MplExt
import MplAST.MplParsed
import MplAST.MplRenamed

import Optics

import GHC.Generics 
import Data.Void

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Data
import Data.Kind

import Data.Foldable
import Data.Functor.Foldable (Base, cata)
import Data.Function

{- Module for generalized utility functions over the AST
 -
 -
 -}

class MplProgUtil x where
    mplStmtTopLevelIdents :: MplStmt x -> NonEmpty (IdP x)
    mplDefnIdents :: MplDefn x -> NonEmpty (IdP x)

instance MplProgUtil MplParsed where
    mplStmtTopLevelIdents (MplStmt defs _) = 
        NE.fromList $ fold $ fmap (NE.toList . mplDefnIdents) defs

    mplDefnIdents = NE.fromList . f
      where
        f (ObjectDefn def) = case def of
            SeqObjDefn def -> case def of
                DataDefn n -> g n
                CodataDefn n -> g n
            ConcObjDefn def -> case def of
                ProtocolDefn n -> g n
                CoprotocolDefn n -> g n
        f (FunctionDefn n) = [n ^. funName]
        f (ProcessDefn n) = [n ^. procName]
    
        g (MplTypeClauseSpine spine _) =
            fold $ fmap h spine
          where
            h n = n ^. typeClauseName : map (view typePhraseName) (n ^. typeClausePhrases)


mplTypeCollectTypeP :: MplType x -> [TypeP x]
mplTypeCollectTypeP = cata f
  where
    f (TypeVarF _ n) = [n]

    f (TypeWithNoArgsF cxt tp) = mempty

    f (TypeSeqWithArgsF _ n acc) = concat acc
    f (TypeSeqVarWithArgsF _ n acc) = n : concat acc

    f (TypeConcWithArgsF _ n (acc0, acc1)) = concat acc0 ++ concat acc1
    f (TypeConcVarWithArgsF _ n (acc0, acc1)) = n : concat acc0 ++ concat acc1

    f (TypeBuiltInF n) = fold n

    f (XTypeF _) = mempty

{-
class SubstituteVarIdentByExpr t where
    substituteVarIdentByExpr ::
        (IdP MplPatternCompiled, MplExpr MplPatternCompiled) ->
        t ->
        t 

instance SubstituteVarIdentByExpr (MplExpr MplPatternCompiled) where
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

instance SubstituteVarIdentByExpr (NonEmpty (MplCmd MplPatternCompiled)) where
    substituteVarIdentByExpr sub = fmap (substituteVarIdentByExpr sub)

instance SubstituteVarIdentByExpr (MplCmd MplPatternCompiled) where
    substituteVarIdentByExpr sub = cata go
      where
        go :: MplCmdF (MplPass 'PatternCompiled) (MplCmd MplPatternCompiled) -> MplCmd MplPatternCompiled
        go = \case
            CRunF ann idp seqs ins outs -> 
                CRun ann idp (map (substituteVarIdentByExpr sub) seqs) ins outs
            CPutF ann expr chp -> 
                CPut ann (substituteVarIdentByExpr sub expr) chp
            CCaseF ann expr cmds -> undefined
                CCase ann (substituteVarIdentByExpr sub expr) cmds
            CIfF ann expr thenc elsec -> 
                CIf ann (substituteVarIdentByExpr sub expr) thenc elsec
            n -> embed n
-}
