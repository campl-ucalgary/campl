{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.KindCheck where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.Env

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Bool

import Data.Functor.Foldable (Base, cata, para)

data KindCheckEnv = KindCheckEnv {
    _kindCheckExpectedPrimitiveKind :: 
        MplPrimitiveKind MplTypeChecked
    , _kindCheckEnvMap :: Map 
        UniqueTag (MplPrimitiveKind MplTypeChecked)
}

data KindCheckErrors = 
    KindArityMismatchExpectedButGot
        (IdP MplRenamed, [IdP MplRenamed]) 
            [IdP MplRenamed]
    | KindPrimtiveMismatchExpectedButGot
        (MplPrimitiveKind MplTypeChecked) (MplPrimitiveKind MplTypeChecked) (MplType MplRenamed)
    | KindExpectedAConcurrentClauseButGotSequentialClause
        (IdP MplRenamed, [IdP MplRenamed])
    | KindExpectedASequentialClauseButGotConcurrentClause
        (IdP MplRenamed, ([IdP MplRenamed], [IdP MplRenamed]))
        

$(makeLenses ''KindCheckEnv)
$(makePrisms ''KindCheckEnv) 
$(makeClassyPrisms ''KindCheckErrors) 

    {-
kindCheckProcessType :: 
    TypeCheck 
        ([TypeP MplRenamed], [MplType MplRenamed], [MplType MplRenamed], [MplType MplRenamed]) 
        (Maybe ([TypeP MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]))
kindCheckProcessType proctype@(varsyms, seqs, ins, outs) = do
    symtab <- guse (envLcl % typeInfoSymTab)
    
    ~(res, st) <- (`runStateT` KindCheckEnv (SeqKind ()) mempty) 
        . (`runReaderT` ((Map.fromList (undefined varsyms)) <> symtab)) $ do
            seqs' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= SeqKind ()
                    kindCheck mpltype) seqs
            ins' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= ConcKind ()
                    kindCheck mpltype) ins
            outs' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= ConcKind ()
                    kindCheck mpltype) outs
            return $ (,,) <$> sequenceA seqs' <*> sequenceA ins' <*> sequenceA outs'

    return $ do
        ~(seqs',ins',outs') <- res
        return ( st ^. kindCheckEnvMap % to (collectTypePs . Map.toList)
               , seqs', ins', outs')
    undefined


  where
    collectTypePs = undefined
    -}

kindCheck ::
    forall e m .
    ( AsKindCheckErrors e
    , MonadState KindCheckEnv m
    , MonadReader SymTab m
    , MonadError e m ) => 
    MplType MplRenamed ->
    m (MplType MplTypeChecked)
kindCheck = para f 
  where
    f :: Base (MplType MplRenamed) (MplType MplRenamed, m (MplType MplTypeChecked)) -> m (MplType MplTypeChecked)
    f (TypeVarF cxt n) = do
        ekd <- guse kindCheckExpectedPrimitiveKind 
        klkup <- guse $ kindCheckEnvMap % ix (n ^. uniqueTag)
        let klkup' = fromJust klkup

        if isJust klkup && ekd /= klkup'
            then throwError $ 
                _KindPrimtiveMismatchExpectedButGot # 
                    (ekd, klkup', TypeVar cxt n)
            else return $ TypeVar ekd n 

    f (TypeSeqWithArgsF cxt tp args) = do
        ekd <- guse kindCheckExpectedPrimitiveKind 

        {-
        tell $ bool 
            [_KindPrimtiveMismatchExpectedButGot # (ekd, SeqKind (), TypeSeqWithArgs cxt tp $ map fst args)] [] 
            $ SeqKind () == ekd
            -}

        undefined
