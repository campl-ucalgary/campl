{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
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
import Data.Functor.Identity
import Data.Functor.Foldable (Base, cata, embed)
import Control.Monad
import Data.Foldable
import Data.Function

{- Module for generalized utility functions over the AST
 -
 -
 -}

-- | TODO move this out into the renamer..
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


{- | Type class for traversing through all elements of an mpl expression -}
class TraverseMplExpr t x where
    traverseMplExpr :: 
        ( Monad m 
        , XFunctionDefn x ~  MplFunction x
        , XProcessDefn x ~  MplProcess x 
        , XMplExpr x ~ MplExpr x
        , XMplCmd x ~ MplCmd x
        ) => 
        (MplExpr x -> m (MplExpr x)) -> 
        t x -> 
        m (t x)

instance TraverseMplExpr MplExpr x where
    traverseMplExpr ::
        forall m.
        ( Monad m 
        , XFunctionDefn x ~ MplFunction x
        , XProcessDefn x ~ MplProcess x 
        , XMplExpr x ~ MplExpr x
        , XMplCmd x ~ MplCmd x
        ) => 
        (MplExpr x -> m (MplExpr x)) -> 
        MplExpr x -> 
        m (MplExpr x)
    traverseMplExpr k = cata go
      where
        go :: Monad m => MplExprF x (m (MplExpr x)) -> m (MplExpr x)
        go = \case
            ELetF ann lets expr -> do
                lets' <- traverse f lets
                expr' <- expr
                k (ELet ann lets' expr')
              where
                f :: Monad m => MplStmt x -> m (MplStmt x)
                f stmt = traverseOf (stmtWhereBindings % traversed) f stmt
                        >>= traverseOf (stmtDefns % traversed) g

                g :: Monad m => MplDefn x -> m (MplDefn x)
                g obj@(ObjectDefn _) = return obj
                g (FunctionDefn fun) = FunctionDefn <$> traverseOf (funDefn % traversed % _2) (traverseMplExpr k) fun
                g (ProcessDefn proc) = ProcessDefn <$> traverseOf (procDefn % traversed % _2 % traversed) (traverseMplExpr k) proc
            res -> join $ (k . embed) <$> sequenceA res


instance TraverseMplExpr MplCmd x where
    traverseMplExpr ::
        forall m.
        ( Monad m 
        , XFunctionDefn x ~ MplFunction x
        , XProcessDefn x ~ MplProcess x 
        , XMplExpr x ~ MplExpr x
        , XMplCmd x ~ MplCmd x
        ) => 
        (MplExpr x -> m (MplExpr x)) -> 
        MplCmd x -> 
        m (MplCmd x)
    traverseMplExpr k = cata go
      where
        go = \case
            CRunF ann idp seqs ins outs -> do
                seqs' <- traverse (traverseMplExpr k) seqs 
                return $ CRun ann idp seqs' ins outs
            CPutF ann expr chp -> do
                expr' <- traverseMplExpr k expr
                return $ CPut ann expr' chp
            CCaseF ann expr cmds -> do
                expr' <- traverseMplExpr k expr
                cmds' <- sequenceOf (traversed % _2 % traversed) cmds
                return $ CCase ann expr' cmds'
            CIfF ann expr thenc elsec -> do
                expr' <- traverseMplExpr k expr
                thenc' <- sequenceA thenc
                elsec' <- sequenceA elsec
                return $ CIf ann expr' thenc' elsec'
            CSwitchF ann switches -> do
                traverseOf (traversed % _1) (traverseMplExpr k) switches 
                    >>= sequenceOf (traversed % _2 % traversed) 
                    >>= return . CSwitch ann 
            n -> embed <$> sequenceA n

class MapMplExpr t x where
    mapMplExpr ::
        ( XFunctionDefn x ~  MplFunction x
        , XProcessDefn x ~  MplProcess x 
        , XMplExpr x ~ MplExpr x
        , XMplCmd x ~ MplCmd x ) =>
        (MplExpr x -> MplExpr x) ->
        t x ->
        t x 

instance TraverseMplExpr t x => MapMplExpr t x where
    mapMplExpr f = runIdentity . traverseMplExpr (Identity . f)
