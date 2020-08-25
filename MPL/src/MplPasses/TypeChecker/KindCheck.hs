{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
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

import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.Env

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Bool

import Data.Functor.Foldable (Base, cata, para)

import Debug.Trace

type KindCheckEnvMap = Map 
        UniqueTag (Maybe (MplPrimitiveKind MplTypeChecked))

data KindCheckEnv = KindCheckEnv {
    _kindCheckExpectedPrimitiveKind :: 
        MplPrimitiveKind MplTypeChecked
    , _kindCheckEnvMap :: KindCheckEnvMap
}


collectKindCheckEnvSeqs :: [IdentR] -> KindCheckEnvMap
collectKindCheckEnvSeqs = Map.fromList . map f
  where
    f n = (n ^. uniqueTag, Just $ SeqKind ())

collectKindCheckEnvConcs :: [IdentR] -> KindCheckEnvMap
collectKindCheckEnvConcs = Map.fromList . map f
  where
    f n = (n ^. uniqueTag, Just $ ConcKind ())

data KindCheckErrors = 
    KindAritySeqMismatchExpectedButGot
        (IdP MplRenamed, [IdP MplRenamed]) 
        (IdP MplRenamed, [MplType MplRenamed])
    | KindArityConcMismatchExpectedButGot
        (IdP MplRenamed, ([IdP MplRenamed], [IdP MplRenamed]))
        (IdP MplRenamed, ([MplType MplRenamed], [MplType MplRenamed]))
    | KindPrimtiveMismatchExpectedButGot
        (MplPrimitiveKind MplTypeChecked) (MplPrimitiveKind MplTypeChecked) (MplType MplRenamed)
    | KindHigherKindedTypesAreNotAllowed (MplType MplRenamed)

    | KindGivenAConcurrentClauseButGotASequentialClause
       (IdP MplRenamed, ([MplType MplRenamed], [MplType MplRenamed]))  
       (IdP MplRenamed, [IdP MplRenamed])
    | KindGivenASequentialClauseButGotAConcurrentClause
        (IdP MplRenamed, [MplType MplRenamed]) (IdP MplRenamed, ([IdP MplRenamed], [IdP MplRenamed]))

    | KindBadLookup
  deriving Show

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
                    primitiveKindCheck mpltype) seqs
            ins' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= ConcKind ()
                    primitiveKindCheck mpltype) ins
            outs' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= ConcKind ()
                    primitiveKindCheck mpltype) outs
            return $ (,,) <$> sequenceA seqs' <*> sequenceA ins' <*> sequenceA outs'

    return $ do
        ~(seqs',ins',outs') <- res
        return ( st ^. kindCheckEnvMap % to (collectTypePs . Map.toList)
               , seqs', ins', outs')
    undefined


  where
    collectTypePs = undefined
    -}

primitiveKindCheck ::
    forall e m .
    ( AsKindCheckErrors e
    , MonadState KindCheckEnv m
    , MonadReader SymTabType m
    , MonadWriter [e] m ) => 
    MplType MplRenamed ->
    m (Maybe (MplType MplTypeChecked))
primitiveKindCheck = para f 
  where
    f :: Base (MplType MplRenamed) 
        (MplType MplRenamed, m (Maybe (MplType MplTypeChecked))) -> 
            m (Maybe (MplType MplTypeChecked))
    f (TypeVarF cxt n) = do
        ekd <- guse kindCheckExpectedPrimitiveKind 
        klkup <- guses (kindCheckEnvMap % at (n ^. uniqueTag)) fromJust
        let kindmismatch = isJust klkup && ekd /= klkup'
            klkup' = fromJust klkup

        tell $ bool [] 
            [_KindPrimtiveMismatchExpectedButGot # 
                (ekd, klkup', TypeVar cxt n)]
            $ kindmismatch

        let kd = fromMaybe ekd klkup
        kindCheckEnvMap % at (n ^. uniqueTag) % _Just ?= kd
            
        return $ bool (_Just %_TypeVar # (kd, n)) Nothing kindmismatch

    f (TypeSeqWithArgsF cxt tp args) = do
        ekd <- guse kindCheckExpectedPrimitiveKind 
        clauselkup <- gview $ ix (tp ^. uniqueTag)

        let rargs = map fst args
        noerrs <- fmap (null . snd) $ listen $ do 
            tell $ flip (maybe [ _KindBadLookup # () ]) clauselkup $ \case
                SeqObjDefn seqclause -> 
                    let (clausename, clauseargs) = case seqclause of
                            DataDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                            CodataDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                    in bool [] [_KindAritySeqMismatchExpectedButGot # 
                        ( (clausename, clauseargs), (tp, rargs) )
                        ] $ length clauseargs /= length rargs 
                ConcObjDefn concclause ->
                    let (clausename, clauseargs) = case concclause of
                            ProtocolDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                            CoprotocolDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                    in [ _KindGivenASequentialClauseButGotAConcurrentClause #
                        ((tp,rargs), (clausename, clauseargs)) ]
                        
            -- checking if this should be a sequential kind
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , SeqKind ()
                    , TypeSeqWithArgs cxt tp rargs)
                ] [] $ SeqKind () == ekd

        rargs <- traverse 
            (\n -> do
                kindCheckExpectedPrimitiveKind .= SeqKind ()
                snd n) args

        return $ flip (bool Nothing) noerrs $ do
            clause <- clauselkup ^? _Just % _SeqObjDefn 
            rargs' <- sequenceA rargs
            return $ _TypeSeqWithArgs # 
                ( clause, tp, rargs' )

    f (TypeSeqVarWithArgsF cxt tp args) = do
        tell [ _KindHigherKindedTypesAreNotAllowed #
            _TypeSeqVarWithArgs # (cxt, tp, map fst args) ]
        return Nothing

    f (TypeConcWithArgsF cxt tp (seqs,concs)) = do
        ekd <- guse kindCheckExpectedPrimitiveKind 
        clauselkup <- gview $ ix (tp ^. uniqueTag)

        let rseqs = map fst seqs
            rconcs = map fst concs
        noerrs <- fmap (null . snd) $ listen $ do 
            tell $ flip (maybe []) clauselkup $ \case
                ConcObjDefn seqclause -> 
                    let (clausename, clauseargs@(clauseseqs, clauseconcs)) = case seqclause of
                            ProtocolDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                            CoprotocolDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                    in bool [] [ _KindArityConcMismatchExpectedButGot # 
                        ( (clausename, clauseargs), (tp, (rseqs,rconcs)) )
                        ] $ length clauseseqs /= length rseqs  
                            && length clauseconcs /= length rconcs  
                SeqObjDefn concclause ->
                    let (clausename, clauseargs) = case concclause of
                            DataDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                            CodataDefn clause -> 
                                ( clause ^. typeClauseName
                                , clause ^. typeClauseArgs)
                    in [ _KindGivenAConcurrentClauseButGotASequentialClause #
                        ((tp,(rseqs, rconcs)), (clausename, clauseargs)) ]
                        
            -- checking if this should be a concurrent kind
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , ConcKind ()
                    , TypeConcWithArgs cxt tp (rseqs, rconcs))
                ] [] $ ConcKind () == ekd

        rseqs <- traverse 
            (\n -> do
                kindCheckExpectedPrimitiveKind .= SeqKind ()
                snd n) seqs
        rconcs <- traverse 
            (\n -> do
                kindCheckExpectedPrimitiveKind .= ConcKind ()
                snd n) concs

        return $ flip (bool Nothing) noerrs $ do
            clause <- clauselkup ^? _Just % _ConcObjDefn 
            rseqs' <- sequenceA rseqs
            rconcs' <- sequenceA rconcs
            return $ _TypeConcWithArgs # 
                ( clause, tp, (rseqs', rconcs') )

    f (TypeConcVarWithArgsF cxt tp args) = do
        tell [ _KindHigherKindedTypesAreNotAllowed #
            _TypeConcVarWithArgs # (cxt, tp, map fst *** map fst $ args) ]
        return Nothing
