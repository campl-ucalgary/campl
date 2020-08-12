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

class MplProgUtil x where
    mplStmtTopLevelIdents :: MplStmt x -> NonEmpty (IdP x)
    mplDefnIdents :: MplDefn x -> NonEmpty (IdP x)


instance MplProgUtil MplParsed where
    mplStmtTopLevelIdents (MplStmt defs _) = 
        NE.fromList $ fold $ fmap (NE.toList . mplDefnIdents) defs

    mplDefnIdents = NE.fromList . f
      where
        f (ObjectDefn def) = case def of
            DataDefn n -> g n
            CodataDefn n -> g n
            ProtocolDefn n -> g n
            CoprotocolDefn n -> g n
        f (FunctionDefn n) = [n ^. funName]
        f (ProcessDefn n) = [n ^. procName]
    
        g (MplTypeClauseSpine spine _) =
            fold $ fmap h spine
          where
            h n = n ^. typeClauseName : map (view typePhraseName) (n ^. typeClausePhrases)


mplTypeCollectTypeVars :: MplType x -> [(XTypeVar x, TypeP x, [MplType x])]
mplTypeCollectTypeVars = cata f
  where
    f = undefined
