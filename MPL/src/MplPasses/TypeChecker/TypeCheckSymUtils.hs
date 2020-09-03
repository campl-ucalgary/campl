{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import Control.Monad.State
import Control.Monad.Writer

import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Data.Maybe
import Control.Arrow

import Debug.Trace

type TypeCheckSymLookup from to =
    forall e0 e1 n. 
    ( AsTypeCheckCallErrors e1 
    , MonadWriter (TypeCheckErrorPkg e0 e1) n 
    , MonadState SymTab n ) =>
    from -> n to

lookupSymExpr :: 
    TypeCheckSymLookup (IdP MplRenamed) (SymEntry SymSeqType SymExprInfo)
lookupSymExpr = fmap fromJust . lookupSymExprM

lookupSymExprM :: 
    TypeCheckSymLookup (IdP MplRenamed) (Maybe (SymEntry SymSeqType SymExprInfo))
lookupSymExprM n = guse (symTabExpr % at (n ^. uniqueTag)) 

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


class CollectSeqSymObj (t :: SeqObjDefnTag) where
    collecySeqSymObj :: MplTypeClauseSpine MplTypeChecked (SeqObjTag t) -> [(UniqueTag, SymEntry SymSeqType SymExprInfo)]

instance CollectSeqSymObj DataDefnTag where
    collecySeqSymObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
                 (pure . f) spine 
      where
        -- should call nub here? although doesn't really matter..
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . view typeClauseArgs) spine
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
    collecySeqSymObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
                 (pure . f) spine 
      where
        -- should call nub here? although doesn't really matter..
        -- TODO: for each phrase, compute the the free variables, append the 
        -- tpvars after and call nub on that.. This ensures that we will get the variables
        -- binding closest to the clause while including some of the ``out of scope" variables
        -- which should just error anyways...
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . view typeClauseArgs) spine
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
    MonadState (Env SymTab TypeInfoEnv) m => MplDefn MplTypeChecked -> m ()
collectSymTabDefn def = do
    tsymtab <- guse $ envLcl % typeInfoSymTab % symTabExpr
    csymtab <- guse $ envLcl % typeInfoSymTab % symTabConc
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
                    DataDefn spine -> collecySeqSymObj spine
                    CodataDefn spine -> collecySeqSymObj spine
                _ -> mempty
            FunctionDefn def -> pure 
                ( def ^. funName % uniqueTag
                , SymEntry (fromJust $ tsymtab ^? at (def ^. funName % uniqueTag) % _Just % symEntryType) 
                    $ _SymSeqCall % _ExprCallFun # def)
            _ -> mempty


    envGbl %= (syms<>)

-- | recollects symbol table definitions after a recursive group
-- is defined.. Note that object definitions do not need recollection
-- but recursive function/process calls need to be recollected....
recollectSymTabDefn ::
    ( MonadState SymTab m ) => (MplDefn MplTypeChecked) -> m ()
recollectSymTabDefn (ObjectDefn _) = return ()
recollectSymTabDefn (FunctionDefn (MplFunction name tp bdy)) = 
    symTabExpr % at (name ^. uniqueTag) % _Just % symEntryType .= _SymExplicit # tp
recollectSymTabDefn (ProcessDefn (MplProcess name tp bdy)) = 
    symTabConc % at (name ^. uniqueTag) % _Just % symEntryType .= _SymExplicit # tp

class EliminateSymTabObj (t :: ObjectDefnTag) where
    eliminateSymTabObj :: ( MonadState SymTab m ) =>
         (MplTypeClauseSpine MplTypeChecked t) -> m ()

instance EliminateSymTabObj (t :: ObjectDefnTag) where
    eliminateSymTabObj spine = do
        forOf_ 
            ( typeClauseSpineClauses 
            % traversed 
            % typeClauseName 
            % uniqueTag )
            spine $ \n -> symTabType % at n .= Nothing

        forOf_ 
            ( typeClauseSpineClauses 
            % traversed 
            % typeClausePhrases 
            % traversed 
            % typePhraseName 
            % uniqueTag )
            spine $ \n -> symTabExpr % at n .= Nothing
    

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
