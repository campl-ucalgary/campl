{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
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

  deriving Show

$(makeLenses ''KindCheckEnv)
$(makePrisms ''KindCheckEnv) 
$(makeClassyPrisms ''KindCheckErrors) 


type KindCheck from to = 
    forall e m .
    ( AsKindCheckErrors e
    , MonadState KindCheckEnv m
    , MonadReader SymTabType m
    , MonadWriter [e] m ) => 
    from ->
    m to

primitiveKindCheck ::
    KindCheck (MplType MplRenamed) (Maybe (MplType MplTypeChecked))
primitiveKindCheck = para f 
  where
    f :: Base (MplType MplRenamed) 
        (MplType MplRenamed, _ (Maybe (MplType MplTypeChecked))) -> 
            _ (Maybe (MplType MplTypeChecked))
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
        ~clauselkup <- fmap fromJust $ gview (ix (tp ^. uniqueTag))

        let rargs = map fst args
        ~noerrs <- fmap (null . snd) $ listen $ do 
            tell $ case clauselkup of
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
            ~clause <- clauselkup ^? _SeqObjDefn 
            rargs' <- sequenceA rargs
            return $ _TypeSeqWithArgs # ( clause, tp, rargs' )

    f (TypeSeqVarWithArgsF cxt tp args) = do
        tell [ _KindHigherKindedTypesAreNotAllowed #
            _TypeSeqVarWithArgs # (cxt, tp, map fst args) ]
        return Nothing

    f (TypeConcWithArgsF cxt tp (seqs,concs)) = do
        ekd <- guse kindCheckExpectedPrimitiveKind 
        -- clauselkup <- gview $ ix (tp ^. uniqueTag)
        clauselkup <- undefined

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

    f (TypeBuiltInF n) = case n of
        TypeIntF cxt -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = SeqKind () == ekd
            -- checking if this should be a sequential kind
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , SeqKind ()
                    , _TypeIntF # cxt)
                ] [] $ noerr

            return $ bool Nothing (_Just % _TypeIntF # cxt) noerr

        -- duplciated code..
        TypeCharF cxt -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = SeqKind () == ekd
            -- checking if this should be a sequential kind
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , SeqKind ()
                    , _TypeCharF # cxt)
                ] [] $ noerr

            return $ bool Nothing (_Just % _TypeCharF # cxt) noerr

        -- duplciated code
        TypeDoubleF cxt -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = SeqKind () == ekd
            -- checking if this should be a sequential kind
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , SeqKind ()
                    , _TypeDoubleF # cxt)
                ] [] $ noerr

            return $ bool Nothing (_Just % _TypeDoubleF # cxt) noerr

        TypeGetF ann (lr, l) (rr, r) -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = ekd == _ConcKind # ()
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , _ConcKind # ()
                    , _TypeGetF # (ann, lr, rr))
                ] [] $ noerr

            kindCheckExpectedPrimitiveKind .= _SeqKind # ()
            (l', llg) <- listen l

            kindCheckExpectedPrimitiveKind .= _ConcKind # ()
            (r', rlg) <- listen r

            return $ bool Nothing
                (review _TypeGetF <$> ((ann,,) <$> l' <*> r'))
                $ noerr && null llg && null rlg 

        -- duplciated code
        TypePutF ann (lr, l) (rr, r) -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = ekd == _ConcKind # ()
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , _ConcKind # ()
                    , _TypePutF # (ann, lr, rr))
                ] [] $ noerr

            kindCheckExpectedPrimitiveKind .= _SeqKind # ()
            (l', llg) <- listen l

            kindCheckExpectedPrimitiveKind .= _ConcKind # ()
            (r', rlg) <- listen r

            return $ bool Nothing
                (review _TypePutF <$> ((ann,,) <$> l' <*> r'))
                $ noerr && null llg && null rlg 

        TypeTensorF ann (lr, l) (rr, r) -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = ekd == _ConcKind # ()
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , _ConcKind # ()
                    , _TypeTensorF # (ann, lr, rr))
                ] [] $ noerr

            kindCheckExpectedPrimitiveKind .= _ConcKind # ()
            (l', llg) <- listen l

            kindCheckExpectedPrimitiveKind .= _ConcKind # ()
            (r', rlg) <- listen r

            return $ bool Nothing
                (review _TypeTensorF <$> ((ann,,) <$> l' <*> r'))
                $ noerr && null llg && null rlg 

        -- duplicated code
        TypeParF ann (lr, l) (rr, r) -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = ekd == _ConcKind # ()
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , _ConcKind # ()
                    , _TypeParF # (ann, lr, rr))
                ] [] $ noerr

            kindCheckExpectedPrimitiveKind .= _ConcKind # ()
            (l', llg) <- listen l

            kindCheckExpectedPrimitiveKind .= _ConcKind # ()
            (r', rlg) <- listen r

            return $ bool Nothing
                (review _TypeParF <$> ((ann,,) <$> l' <*> r'))
                $ noerr && null llg && null rlg 
                
        TypeNegF ann (lr, l)  -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = ekd == _ConcKind # ()
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , _ConcKind # ()
                    , _TypeNegF # (ann, lr))
                ] [] $ noerr

            kindCheckExpectedPrimitiveKind .= _ConcKind # ()
            (l', llg) <- listen l

            return $ bool Nothing
                (review _TypeNegF <$> ((ann,) <$> l'))
                $ noerr && null llg 
                
        TypeTopBotF cxt  -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            let noerr = ekd == _ConcKind # ()
            tell $ bool 
                [_KindPrimtiveMismatchExpectedButGot # 
                    ( ekd
                    , _ConcKind # ()
                    , _TypeTopBotF # cxt)
                ] [] $ noerr

            return $ bool Nothing
                (_Just % _TypeTopBotF # cxt)
                $ noerr 

