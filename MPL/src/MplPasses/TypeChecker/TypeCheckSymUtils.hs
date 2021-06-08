{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheckSymUtils where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplTypeChecked
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSemanticErrors
import MplPasses.TypeChecker.TypeCheckCallErrors
import MplPasses.TypeChecker.TypeCheckErrorPkg
import MplPasses.Env
import MplAST.MplProgUtil


import Control.Monad.State
import Control.Monad.Writer

import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Data.Maybe
import Control.Arrow

import Data.Bool

import Debug.Trace


{- Module for utilities in looking things up in the symbol
 - table..
 -}

type TypeCheckSymLookup from to =
    forall e0 e1 m0 m1 n. 
    ( AsTypeCheckCallErrors e1 
    , MonadWriter (TypeCheckErrorPkg e0 e1) n 
    , MonadWriter (TypeCheckErrorPkg e0 e1) m0
    , MonadWriter (TypeCheckErrorPkg e0 e1) m1
    , MonadState SymTab n 
    , SymZooms m0 m1 n
    ) =>
    from -> n to

runZoomedLookup m = m >>= return 
    -- remark: we need this strange m >>= return after to get the correct lazy
    -- actions and we do not encounter bottom... Strangely, I guess the
    -- pressence of zoom does not make the monad transformer stack a Monad?
    -- i.e., it violates the right identity law m >>= return === m
    -- Actually, apparently the state monad just doesn't satisfy this law.. such is
    -- life when working with bottom values.

-- we can generalize all of these expression lookups and make
-- it more compositional with zoom..
lookupSymExpr :: 
    TypeCheckSymLookup 
        (IdP MplRenamed) 
        (Maybe (SymEntry SymSeqType SymExprInfo))
lookupSymExpr ident = runZoomedLookup $ zoomSymExpr ident (guse equality) 

zoomSymExpr :: 
    ( AsTypeCheckCallErrors e1 
    -- , HasUniqueTag ident 
    , MonadWriter (TypeCheckErrorPkg e0 e1) m
    , Zoom m n (Maybe (SymEntry SymSeqType SymExprInfo)) SymTab 
    ) => IdP MplRenamed -> m a -> n a
zoomSymExpr ident k = zoom (symTabExpr % at (ident ^. uniqueTag)) $ do 
    ~res <- guse equality
    tell $ review _InternalError $ maybe [_CannotCallTerm # ident] mempty res
    k

zoomSymExprSeqPhrase ::
    ( AsTypeCheckCallErrors e1 
    -- , HasUniqueTag ident 
    , MonadWriter (TypeCheckErrorPkg e0 e1) m
    , MonadWriter (TypeCheckErrorPkg e0 e1) n
    , Zoom m n 
        (MplSeqObjDefn MplTypeCheckedPhrase) 
        (Maybe (SymEntry SymSeqType SymExprInfo))
    ) => m a -> n (Maybe a)
zoomSymExprSeqPhrase k = do
    symentry <- guse equality
    censor (bool id mempty $ isNothing symentry ) 
        $ zoomMaybe (_Just % symEntryInfo % _SymSeqPhraseCall) $ k

lookupSymExprCodataPhrase :: 
    TypeCheckSymLookup 
        (IdP MplRenamed, MplExpr MplRenamed) 
        (Maybe (SymEntry SymSeqType (MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag))))
lookupSymExprCodataPhrase (ident, expr) = runZoomedLookup $ 
    zoomSymExpr ident $ do
        entry <- guse equality 
        defn <- zoomSymExprSeqPhrase $ do   
            objdefn <- guse equality 
            tell $ review _InternalError $ maybeToList $ 
                objdefn ^? _DataDefn 
                    % to (review _IllegalExprCodataCallGotDataInstead . (expr,))
            return objdefn
        return $ do
            lkuptp <- entry ^? _Just % symEntryType
            defn <- defn ^? _Just % _CodataDefn
            return $ SymEntry lkuptp defn

-- duplciated code
lookupSymExprDataPhrase :: 
    TypeCheckSymLookup 
        (IdP MplRenamed, MplExpr MplRenamed) 
        (Maybe (SymEntry SymSeqType (MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag))))
lookupSymExprDataPhrase (ident, expr) = runZoomedLookup $ 
    zoomSymExpr ident $ do
        entry <- guse equality 
        defn <- zoomSymExprSeqPhrase $ do   
            objdefn <- guse equality 
            tell $ review _InternalError $ maybeToList $ 
                objdefn ^? _CodataDefn 
                    % to (review _IllegalExprDataCallGotCodataInstead . (expr,))
            return objdefn
        return $ do
            lkuptp <- entry ^? _Just % symEntryType
            defn <- defn ^? _Just % _DataDefn
            return $ SymEntry lkuptp defn

{-
lookupSymExprDataPhrase :: 
    TypeCheckSymLookup (IdP MplRenamed, MplExpr MplRenamed) (SymEntry SymSeqType SymExprInfo)
lookupSymExprDataPhrase n = do
    res <- guse $ symTabExpr % at (n ^. uniqueTag)
    tell $ review _InternalError $ maybe [_CannotCallTerm # n] mempty res
    return $ fromJust res
-}

lookupSymType :: 
    TypeCheckSymLookup (IdP MplRenamed) (MplObjectDefn MplTypeCheckedClause)
lookupSymType n = do
    res <- guse $ symTabType % at (n ^. uniqueTag)
    tell $ review _InternalError $ maybe [_CannotCallTerm # n] mempty res
    return $ fromJust res

lookupSymCh :: 
    TypeCheckSymLookup (ChP MplRenamed) (SymEntry TypeTag ChIdentR)
lookupSymCh n = do
    res <- guse $ symTabCh % at (n ^. uniqueTag)
    tell $ review _InternalError $ maybe [_CannotCallCh # n] mempty res
    return $ fromJust res 

lookupSymConc :: TypeCheckSymLookup (IdP MplRenamed) (SymEntry SymConcType SymConcInfo)
lookupSymConc n = do
    res <- guse $ symTabConc % at (n ^. uniqueTag)
    tell $ review _InternalError $ maybe [_CannotCallTerm # n] mempty res
    return $ fromJust res 

{- | This is used when searching for the final sequential type AFTER type checking has 
been completed of an expression. Something to note:
    - The types it look up will ONLY include the codomain type i.e.,
        given @a,b,c -> d@, this will just return @d@ since we may assume
        that everything is fully applied in this language.

-}
lookupInferredSeqTypeExpr :: 
    TypeTag ->
    Map TypeTag SymTypeEntry ->
    Maybe (XMplType MplTypeChecked)
lookupInferredSeqTypeExpr ttype =  preview (at ttype % _Just % _SymTypeSeq % _3)

{- | Similar to the above, but for channels.
-}
lookupInferredTypeCh :: 
    TypeTag ->
    Map TypeTag SymTypeEntry ->
    Maybe (XMplType MplTypeChecked)
lookupInferredTypeCh ttypech =  preview (at ttypech % _Just % _SymTypeCh % _2)


class CollectSeqSymObj (t :: SeqObjDefnTag) where
    collectSeqSymObj :: MplTypeClauseSpine MplTypeChecked (SeqObjTag t) -> [(UniqueTag, SymEntry SymSeqType SymExprInfo)]

instance CollectSeqSymObj DataDefnTag where
    collectSeqSymObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
                 (pure . f) spine 
      where
        -- should call nub here? although doesn't really matter..
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . view typeClauseArgs) spine
                <> foldMapOf (typeClauseSpineClauses % folded) (pure . NamedType . view typeClauseStateVar) spine
        stsubs = typeClauseSpineStateVarClauseSubs spine
        f phrase = 
                ( phrase ^. typePhraseName % uniqueTag
                , SymEntry 
                    ( _SymDataPhrase % _SymSeqPhraseType #
                        ( ( tpvars
                            , phrase ^. typePhraseFrom % to (fmap (substituteTypeVars stsubs))
                            , phrase ^. typePhraseTo % to (substituteTypeVars stsubs))
                        , ( tpvars, phrase ^. typePhraseFrom, phrase ^. typePhraseTo)
                        )
                    ) 
                    $ _SymSeqPhraseCall % _DataDefn # phrase
                )

-- more or less duplicated code
instance CollectSeqSymObj CodataDefnTag where
    collectSeqSymObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
                 (pure . f) spine 
      where
        -- should call nub here? although doesn't really matter..
        -- TODO: for each phrase, compute the the free variables, append the 
        -- tpvars after and call nub on that.. This ensures that we will get the variables
        -- binding closest to the clause while including some of the ``out of scope" variables
        -- which should just error anyways...
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . view typeClauseArgs) spine
                <> foldMapOf (typeClauseSpineClauses % folded) (pure . NamedType . view typeClauseStateVar) spine
        stsubs = typeClauseSpineStateVarClauseSubs spine
        f phrase = 
                ( phrase ^. typePhraseName % uniqueTag
                , SymEntry 
                    ( _SymCodataPhrase % _SymSeqPhraseType #
                        ( ( tpvars
                            , phrase ^. typePhraseFrom % to 
                                (fmap (substituteTypeVars stsubs)
                                 *** substituteTypeVars stsubs )
                            , phrase ^. typePhraseTo % to (substituteTypeVars stsubs))
                        , ( tpvars, phrase ^. typePhraseFrom, phrase ^. typePhraseTo)
                        )
                    ) 
                    $ _SymSeqPhraseCall % _CodataDefn # phrase
                )

class CollectConcSymObj (t :: ConcObjDefnTag) where
    collectConcSymObj :: MplTypeClauseSpine MplTypeChecked (ConcObjTag t) -> [(UniqueTag, SymEntry SymConcType SymConcInfo)]

instance CollectConcSymObj ProtocolDefnTag where
    collectConcSymObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
        (pure . f) spine
      where
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . uncurry (<>) . view typeClauseArgs) spine
        stsubs = typeClauseSpineStateVarClauseSubs spine
        f phrase = 
            ( phrase ^. typePhraseName ^. uniqueTag
            , _SymEntry #
                ( _SymConcPhrase # 
                    ( tpvars
                    , phrase ^. typePhraseFrom % to (substituteTypeVars stsubs)
                    -- we only care about the unwrapped type... the original
                    -- type is recovered from the state var.
                    )
                , _SymConcPhraseCall % _ProtocolDefn # phrase
                )
            )

-- more or less duplicated code
instance CollectConcSymObj CoprotocolDefnTag where
    collectConcSymObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
        (pure . f) spine
      where
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . uncurry (<>) . view typeClauseArgs) spine
        stsubs = typeClauseSpineStateVarClauseSubs spine
        f phrase = 
            ( phrase ^. typePhraseName ^. uniqueTag
            , _SymEntry #
                ( _SymConcPhrase # 
                    ( tpvars
                    , phrase ^. typePhraseTo % to (substituteTypeVars stsubs)
                    )
                , _SymConcPhraseCall % _CoprotocolDefn # phrase
                )
            )


-- | initially collects the symbol table definitions given
-- a recursive group of declarations..
collectSymTabDefn ::
    MonadState SymTab m => MplDefn MplTypeChecked -> m SymTab
collectSymTabDefn def = do
    -- tsymtab <- guse $ envLcl % typeInfoSymTab % symTabExpr
    -- csymtab <- guse $ envLcl % typeInfoSymTab % symTabConc
    tsymtab <- guse symTabExpr
    csymtab <- guse symTabConc
    let ~syms = mempty 
            & symTabExpr .~ symterms 
            & symTabType .~ symtypes
            & symTabConc .~ symconcs
        ~symtypes = Map.fromList $ case def of
            ObjectDefn def -> case def of
                SeqObjDefn def -> case def of
                    DataDefn spine -> spine ^. typeClauseSpineClauses 
                        % to ( map (view (typeClauseName % uniqueTag)
                                &&& review _DataDefn  ) 
                            . NE.toList)
                    CodataDefn spine -> spine ^. typeClauseSpineClauses 
                        % to ( map (view (typeClauseName % uniqueTag)
                                &&& review _CodataDefn ) . NE.toList )
                ConcObjDefn def -> case def of
                    ProtocolDefn spine -> spine ^. typeClauseSpineClauses 
                        % to ( map (view (typeClauseName % uniqueTag)
                                &&& review _ProtocolDefn ) . NE.toList )
                    CoprotocolDefn spine -> spine ^. typeClauseSpineClauses 
                        % to ( map (view (typeClauseName % uniqueTag)
                                &&& review _CoprotocolDefn ) . NE.toList )
            _ -> mempty
        ~symconcs = Map.fromList $ case def of
            ObjectDefn def -> case def of
                ConcObjDefn def -> case def of
                    ProtocolDefn spine -> collectConcSymObj spine
                    CoprotocolDefn spine -> collectConcSymObj spine
                _ -> mempty
            ProcessDefn def -> pure 
                ( def ^. procName % uniqueTag
                , _SymEntry # 
                    ( fromJust $ csymtab ^? at (def ^. procName % uniqueTag) % _Just % symEntryType
                    , _SymRunInfo # def
                    )
                )
            _ -> mempty 

        ~symterms = Map.fromList $ case def of 
            ObjectDefn def -> case def of
                SeqObjDefn def -> case def of
                    DataDefn spine -> collectSeqSymObj spine
                    CodataDefn spine -> collectSeqSymObj spine
                _ -> mempty
            FunctionDefn def -> pure 
                ( def ^. funName % uniqueTag
                , SymEntry 
                    ( fromJust $ tsymtab ^? at (def ^. funName % uniqueTag) % _Just % symEntryType) 
                    $ _SymSeqCall % _ExprCallFun # def)
            _ -> mempty

    return syms


{- | This will get all of the symbol table variables which are not scoped. This is needed
because given a program like
@ 
f a = let g b = a in g a
@
we need to make sure that the type of @g@ is @b -> a@ where @a@ is the same type as 
the value @a@ in the parameters of @f@.
-}
collectNotScopedSymTabTypeVariables ::
    ( MonadState SymTab m ) => 
    m [TypeT]
collectNotScopedSymTabTypeVariables = do
    implicitseqcollected <- guse $ symTabExpr % folded % to collectexpr
    _ <- guse symTabConc
    return implicitseqcollected
  where
    collectexpr :: SymEntry SymSeqType SymExprInfo -> [TypeT]
    collectexpr symentry = case symentry ^? symEntryType % _SymSeqCallType of
        Just calltype -> case calltype of
            SymImplicit ty -> map typeIdentTToTypeT $ mplTypeCollectTypeP ty
            SymExplicit (scoped, args, res) -> filter (`notElem` scoped) $ 
                concat [ concatMap mplTypeCollectTypeP args, mplTypeCollectTypeP res ]
        Nothing -> []

-- | recollects symbol table definitions after a recursive group
-- is defined.. Note that object definitions do not need recollection
-- but recursive function/process calls need to be recollected....
-- We do this since after the type is inferred, we now get an explicit type to work with.
recollectSymTabDefn ::
    ( MonadState SymTab m ) => 
    MplDefn MplTypeChecked -> 
    m ()
recollectSymTabDefn defn = do
    let go = case defn of
            (ObjectDefn _) -> return ()
            (FunctionDefn (MplFunction name tp bdy))  ->
                symTabExpr % at (name ^. uniqueTag) % _Just % symEntryType .= _SymExplicit # tp
            (ProcessDefn (MplProcess name tp bdy)) ->
                symTabConc % at (name ^. uniqueTag) % _Just % symEntryType 
                    .= _SymExplicit # tp
    go 

class EliminateSymTabObj (t :: ObjectDefnTag) where
    eliminateSymTabObj :: ( MonadState SymTab m ) =>
         (MplTypeClauseSpine MplTypeChecked t) -> m ()

instance EliminateSymTabObj (t :: ObjectDefnTag) where
    eliminateSymTabObj spine = do
        -- eliminate the types
        forOf_ 
            ( typeClauseSpineClauses 
            % traversed 
            % typeClauseName 
            % uniqueTag )
            spine $ \n -> symTabType % at n .= Nothing

        -- eliminate the expresssions
        forOf_ 
            ( typeClauseSpineClauses 
            % traversed 
            % typeClausePhrases 
            % traversed 
            % typePhraseName 
            % uniqueTag )
            spine $ \n -> symTabExpr % at n .= Nothing

        -- eliminate the concurrent definitons
        forOf_ 
            ( typeClauseSpineClauses 
            % traversed 
            % typeClausePhrases 
            % traversed 
            % typePhraseName 
            % uniqueTag )
            spine $ \n -> symTabConc % at n .= Nothing
    

-- | eliminates the symbol table definitions of 
eliminateSymTabDefn ::
    ( MonadState SymTab m ) =>
    (MplDefn MplTypeChecked) -> m ()
eliminateSymTabDefn (ObjectDefn obj) = case obj of
    SeqObjDefn def -> case def of
        DataDefn spine -> eliminateSymTabObj spine
        CodataDefn spine -> eliminateSymTabObj spine
    ConcObjDefn def -> case def of
        ProtocolDefn spine -> eliminateSymTabObj spine
        CoprotocolDefn spine -> eliminateSymTabObj spine
eliminateSymTabDefn (FunctionDefn (MplFunction name tp bdy)) = 
    symTabExpr % at (name ^. uniqueTag) .= Nothing
eliminateSymTabDefn (ProcessDefn (MplProcess name tp bdy)) = 
    symTabConc % at (name ^. uniqueTag) .= Nothing

instance InstantiateArrType a => InstantiateArrType (SymCallType a) where
    instantiateArrType ann (SymImplicit a) = return a
    instantiateArrType ann (SymExplicit a) = instantiateArrType ann a
