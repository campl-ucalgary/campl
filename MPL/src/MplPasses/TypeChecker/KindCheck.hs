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
import MplPasses.TypeChecker.TypeCheckErrorPkg
import MplPasses.TypeChecker.TypeCheckCallErrors
import MplPasses.Env

import Data.Proxy

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Control.Arrow hiding ((<+>))
import Data.Foldable
import Data.Traversable
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Bool

import Data.Functor.Foldable (Base, cata, para)
import MplPasses.PassesErrorsPprint

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
    forall e0 e1 m.
    ( AsKindCheckErrors e0
    , AsTypeCheckCallErrors e1
    , MonadState KindCheckEnv m
    , MonadReader SymTabType m
    , MonadWriter (TypeCheckErrorPkg e0 e1) m ) => 
    from -> m to


lookupSymType :: 
    KindCheck (IdP MplRenamed) (MplObjectDefn MplTypeCheckedClause)
lookupSymType n = do
    res <- gview (ix (n ^. uniqueTag)) 
    tell $ review _InternalError $ maybe [_CannotCallTypeCts # n] mempty res
    return $ fromJust res

primitiveKindCheck ::
    KindCheck (MplType MplRenamed) (Maybe (MplType MplTypeChecked))
primitiveKindCheck = para f 

  where
    f :: Base (MplType MplRenamed) 
        (MplType MplRenamed, _ (Maybe (MplType MplTypeChecked))) -> 
            _ (Maybe (MplType MplTypeChecked))
    f = \case
        TypeVarF cxt n -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            klkup <- guses (kindCheckEnvMap % at (n ^. uniqueTag)) fromJust
            let kindmismatch = isJust klkup && ekd /= klkup'
                klkup' = fromJust klkup

            tell $ review _ExternalError $ bool [] 
                [_KindPrimtiveMismatchExpectedButGot # 
                    (ekd, klkup', TypeVar cxt n)]
                $ kindmismatch

            let kd = fromMaybe ekd klkup
            kindCheckEnvMap % at (n ^. uniqueTag) % _Just ?= kd
                
            return $ bool (_Just % _TypeVar # (Just kd, NamedType n)) Nothing kindmismatch


        TypeWithNoArgsF cxt tp -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            ~clauselkup <- lookupSymType tp

            -- duplciated code...
            ~noerrs <- fmap (has _Empty . snd) $ listen $ do 
                tell $ _ExternalError # case clauselkup of
                    SeqObjDefn seqclause
                        | has _ConcKind ekd -> 
                            [ _KindPrimtiveMismatchExpectedButGot # (ekd, _SeqKind # (), TypeWithNoArgs cxt tp) ]
                        | otherwise  -> 
                            let (clausename, clauseargs) = case seqclause of
                                    DataDefn clause -> 
                                        ( clause ^. typeClauseName
                                        , clause ^. typeClauseArgs)
                                    CodataDefn clause -> 
                                        ( clause ^. typeClauseName
                                        , clause ^. typeClauseArgs)
                            in bool [] [_KindAritySeqMismatchExpectedButGot # 
                                ( (clausename, clauseargs), (tp, mempty) )
                                ] $ hasn't _Empty clauseargs
                    ConcObjDefn concclause
                        | has _SeqKind ekd -> 
                            [ _KindPrimtiveMismatchExpectedButGot # (ekd, _ConcKind # (), TypeWithNoArgs cxt tp) ]
                        | otherwise  -> 
                            let ~(clausename, clauseargs) = case concclause of
                                    ProtocolDefn clause -> 
                                        ( clause ^. typeClauseName
                                        , clause ^. typeClauseArgs)
                                    CoprotocolDefn clause -> 
                                        ( clause ^. typeClauseName
                                        , clause ^. typeClauseArgs)
                            in bool [] [_KindArityConcMismatchExpectedButGot # 
                                ( (clausename, clauseargs), (tp, mempty) )
                                ] $ hasn't _Empty clauseargs

            return $ flip (bool Nothing) noerrs $ return $ _TypeWithNoArgs # (clauselkup, tp)


        TypeSeqWithArgsF cxt tp args -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                ~clauselkup <- lookupSymType tp

                let rargs = map fst args
                -- duplciated code...
                ~noerrs <- fmap (has _Empty . snd) $ listen $ do 
                    tell $ _ExternalError # case clauselkup of
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
                            let ~(clausename, clauseargs) = case concclause of
                                    ProtocolDefn clause -> 
                                        ( clause ^. typeClauseName
                                        , clause ^. typeClauseArgs)
                                    CoprotocolDefn clause -> 
                                        ( clause ^. typeClauseName
                                        , clause ^. typeClauseArgs)
                            in [ _KindGivenASequentialClauseButGotAConcurrentClause #
                                ((tp,rargs), (clausename, clauseargs)) ]
                                
                    -- checking if this should be a sequential kind
                    tell $ review _ExternalError $ bool 
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

        TypeSeqVarWithArgsF cxt tp args -> do
            tell $ _ExternalError # [ _KindHigherKindedTypesAreNotAllowed #
                _TypeSeqVarWithArgs # (cxt, tp, map fst args) ]
            return Nothing

        TypeConcWithArgsF cxt tp (seqs,concs) -> do
            ekd <- guse kindCheckExpectedPrimitiveKind 
            -- clauselkup <- gview $ ix (tp ^. uniqueTag)
            ~clauselkup <- lookupSymType tp

            let rseqs = map fst seqs
                rconcs = map fst concs
            -- duplciated code...
            ~noerrs <- fmap (has _Empty . snd) $ listen $ do 
                tell $ review _ExternalError $ case clauselkup of
                    ConcObjDefn concclause -> 
                        let (clausename, clauseargs@(clauseseqs, clauseconcs)) = case concclause of
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
                    SeqObjDefn seqclause ->
                        let (clausename, clauseargs) = case seqclause of
                                DataDefn clause -> 
                                    ( clause ^. typeClauseName
                                    , clause ^. typeClauseArgs)
                                CodataDefn clause -> 
                                    ( clause ^. typeClauseName
                                    , clause ^. typeClauseArgs)
                        in [ _KindGivenAConcurrentClauseButGotASequentialClause #
                            ((tp,(rseqs, rconcs)), (clausename, clauseargs)) ]
                            
                -- checking if this should be a concurrent kind
                tell $ review _ExternalError $ bool 
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
                clause <- clauselkup ^?  _ConcObjDefn 
                rseqs' <- sequenceA rseqs
                rconcs' <- sequenceA rconcs
                return $ _TypeConcWithArgs # 
                    ( clause, tp, (rseqs', rconcs') )

        TypeConcVarWithArgsF cxt tp args -> do
            tell $ _ExternalError # [ _KindHigherKindedTypesAreNotAllowed #
                _TypeConcVarWithArgs # (cxt, tp, map fst *** map fst $ args) ]
            return Nothing

        TypeBuiltInF n -> case n of
            TypeIntF cxt -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = SeqKind () == ekd
                -- checking if this should be a sequential kind
                tell $ review _ExternalError $ bool
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , SeqKind ()
                        , _TypeIntF # cxt)
                    ] [] $ noerr

                return $ bool Nothing (_Just % _TypeIntF # Just cxt) noerr

            -- duplciated code
            TypeUnitF cxt -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = SeqKind () == ekd
                -- checking if this should be a sequential kind
                tell $ review _ExternalError $ bool
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , SeqKind ()
                        , _TypeUnitF # cxt)
                    ] [] $ noerr

                return $ bool Nothing (_Just % _TypeUnitF # Just cxt) noerr
    

            -- duplciated code..
            TypeCharF cxt -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = SeqKind () == ekd
                -- checking if this should be a sequential kind
                tell $ review _ExternalError $ bool
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , SeqKind ()
                        , _TypeCharF # cxt)
                    ] [] $ noerr

                return $ bool Nothing (_Just % _TypeCharF # Just cxt) noerr

            -- duplciated code
            TypeDoubleF cxt -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = SeqKind () == ekd
                -- checking if this should be a sequential kind
                tell $ review _ExternalError $ bool
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , SeqKind ()
                        , _TypeDoubleF # cxt)
                    ] [] $ noerr

                return $ bool Nothing (_Just % _TypeDoubleF # Just cxt) noerr

            -- duplciated code
            TypeBoolF cxt -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = SeqKind () == ekd
                -- checking if this should be a sequential kind
                tell $ review _ExternalError $ bool
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , SeqKind ()
                        , _TypeBoolF # cxt)
                    ] [] $ noerr

                return $ bool Nothing (_Just % _TypeBoolF # Just cxt) noerr

            TypeTupleF cxt (t0,t1,ts) -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                
                -- check if kind error 
                let noerr = SeqKind () == ekd
                tell $ review _ExternalError $ bool
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , SeqKind ()
                        , _TypeTupleF # 
                            ( cxt
                            , 
                                ( fst t0
                                , fst t1
                                , map fst ts
                                )
                            )
                        )
                    ] [] $ noerr

                ~((t0':t1':ts'), lg) <- fmap (second and . unzip) $ for (t0:t1:ts) $ \t -> do
                    kindCheckExpectedPrimitiveKind .= _SeqKind # ()
                    (t', tlg) <- listen $ snd t
                    return (t', has _Empty tlg)

                return $ bool 
                    Nothing
                    (review _TypeTupleF . (Just cxt,) <$> ((,,) <$> t0' <*> t1' <*> sequenceA ts'))
                    $ noerr && lg

            -- similar to tuple
            TypeListF cxt rst -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = SeqKind () == ekd
                -- checking if this should be a sequential kind
                tell $ review _ExternalError $ bool
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , SeqKind ()
                        , _TypeListF # (cxt, fst rst)
                        )
                    ] [] $ noerr

                ~(rst', lg) <- listen $ snd rst

                return $ bool Nothing (review _TypeListF . (Just cxt,) <$> rst') 
                    $ noerr && has _Empty lg

            TypeGetF ann (lr, l) (rr, r) -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = ekd == _ConcKind # ()
                tell $ review _ExternalError $ bool
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
                    (review _TypeGetF <$> ((Just ann,,) <$> l' <*> r'))
                    $ noerr && has _Empty llg && has _Empty rlg 

            -- duplciated code
            TypePutF ann (lr, l) (rr, r) -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = ekd == _ConcKind # ()
                tell $ review _ExternalError $ bool
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
                    (review _TypePutF <$> ((Just ann,,) <$> l' <*> r'))
                    $ noerr && has _Empty llg && has _Empty rlg 

            TypeTensorF ann (lr, l) (rr, r) -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = ekd == _ConcKind # ()
                tell $ review _ExternalError $ bool
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
                    (review _TypeTensorF <$> ((Just ann,,) <$> l' <*> r'))
                    $ noerr && has _Empty llg && has _Empty rlg 

            -- duplicated code
            TypeParF ann (lr, l) (rr, r) -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = ekd == _ConcKind # ()
                tell $ review _ExternalError $ bool 
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
                    (review _TypeParF <$> ((Just ann,,) <$> l' <*> r'))
                    $ noerr && has _Empty llg && has _Empty rlg 
                    
            TypeNegF ann (lr, l)  -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = ekd == _ConcKind # ()
                tell $ review _ExternalError $ bool 
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , _ConcKind # ()
                        , _TypeNegF # (ann, lr))
                    ] [] $ noerr

                kindCheckExpectedPrimitiveKind .= _ConcKind # ()
                (l', llg) <- listen l

                return $ bool Nothing
                    (review _TypeNegF <$> ((Just ann,) <$> l'))
                    $ noerr && has _Empty llg 
                    
            TypeTopBotF cxt  -> do
                ekd <- guse kindCheckExpectedPrimitiveKind 
                let noerr = ekd == _ConcKind # ()
                tell $ review _ExternalError $ bool 
                    [_KindPrimtiveMismatchExpectedButGot # 
                        ( ekd
                        , _ConcKind # ()
                        , _TypeTopBotF # cxt)
                    ] [] $ noerr

                return $ bool Nothing
                    (_Just % _TypeTopBotF # (Just cxt))
                    $ noerr 

pprintKindCheckErrors :: KindCheckErrors -> MplDoc
pprintKindCheckErrors = go
  where
    go :: KindCheckErrors -> MplDoc
    go = \case 
        KindAritySeqMismatchExpectedButGot expected got -> fold
            [ pretty "Sequential arity mismatch for type. Expected call to be of the form"
            , codeblock 
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ uncurry (TypeSeqWithArgs ()) 
                $ second (map (TypeVar ())) 
                $ expected
            , pretty "but got"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ got'
            , pretty "at"  
                <+> pprintSpan (typeLocationSpan got')
            ]
          where
            got' = uncurry (TypeSeqWithArgs ()) $ got
        KindArityConcMismatchExpectedButGot expected got -> fold
            [ pretty "Concurrent arity mismatch for type. Expected call to be of the form"
            , codeblock 
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ uncurry (TypeConcWithArgs ()) 
                $ second (map (TypeVar ()) *** map (TypeVar ()))
                $ expected
            , pretty "but got"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ got'
            , pretty "at"  
                <+> pprintSpan (typeLocationSpan got')
            ]
          where
            got' = uncurry (TypeConcWithArgs ()) $ got
        KindPrimtiveMismatchExpectedButGot expectedkind gotkind tpinquestion -> fold
            [ pretty "Kind primitive mismatch. Expected a" 
                <+> pprintKind expectedkind
                <+> pretty "but got"
                <+> pprintKind gotkind
                <+> pretty "with type"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ tpinquestion
            , pretty "at" 
                <+> pprintSpan (typeLocationSpan tpinquestion)
            ]

        KindHigherKindedTypesAreNotAllowed highertp -> fold
            [ pretty "Illegal higher kinded type."
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ highertp
            , pretty "at"
                <+> pprintSpan (typeLocationSpan highertp)
            ]

        KindGivenAConcurrentClauseButGotASequentialClause given got -> fold
            [ pretty "Illegal sequential type occuring in concurrent type in the type"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ given'
            , pretty "at" 
                <+> pprintSpan (typeLocationSpan given')
                <+> pretty "but got an occurence of the sequential type"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ got'
            ]
          where
            given' = uncurry (TypeConcWithArgs ()) $ given
            got' = uncurry (TypeSeqWithArgs ()) $ second (map (TypeVar ())) $ got 

        KindGivenASequentialClauseButGotAConcurrentClause given got -> fold
            [ pretty "Illegal concurrent type occuring in sequential type in the type"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ given'
            , pretty "at" 
                <+> pprintSpan (typeLocationSpan given')
                <+> pretty "but got an occurence of the sequential type"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ got'
            ]
          where
            given' = uncurry (TypeSeqWithArgs ()) $ given
            got' = uncurry (TypeConcWithArgs ()) $ second (map (TypeVar ()) *** map (TypeVar ())) $ got 



    pprintKind :: MplPrimitiveKind MplTypeChecked -> MplDoc
    pprintKind = \case
        SeqKind _ -> pretty "sequential kind"
        ConcKind _ -> pretty "concurrent kind"

            
  {-
  | KindGivenASequentialClauseButGotAConcurrentClause (IdP
                                                         MplRenamed,
                                                       [MplAST.MplCore.MplType MplRenamed])
                                                      (IdP MplRenamed,
                                                       ([IdP MplRenamed], [IdP MplRenamed]))
    -}
