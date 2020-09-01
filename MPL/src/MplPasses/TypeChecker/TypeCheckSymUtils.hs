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

lookupSymTerm :: 
    TypeCheckSymLookup (IdP MplRenamed) (SymEntry SymType SymTermInfo)
lookupSymTerm = fmap fromJust . lookupSymTermM

lookupSymTermM :: 
    TypeCheckSymLookup (IdP MplRenamed) (Maybe (SymEntry SymType SymTermInfo))
lookupSymTermM n = guse (symTabTerm % at (n ^. uniqueTag)) 

lookupSymType :: 
    TypeCheckSymLookup (IdP MplRenamed) (MplObjectDefn MplTypeCheckedClause)
lookupSymType n = do
    res <- guse (symTabType % at (n ^. uniqueTag)) 
    tell $ review _InternalError $ maybe [_CannotCallTerm # n] mempty res
    return $ fromJust res

lookupSymCh :: 
    TypeCheckSymLookup (ChP MplRenamed) (SymEntry (MplType MplTypeSub) ChIdentR)
lookupSymCh n = do
    res <- guse (symTabCh % at (n ^. uniqueTag))
    tell $ review _InternalError $ maybe [_CannotCallCh # n] mempty res
    return $ fromJust res 


class CollectSymTermObj (t :: ObjectDefnTag) where
    collectSymTermObj :: MplTypeClauseSpine MplTypeChecked t -> [(UniqueTag, SymEntry SymType SymTermInfo)]

instance CollectSymTermObj (SeqObjTag DataDefnTag) where
    collectSymTermObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
                 (pure . f) spine 
      where
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . view typeClauseArgs) spine
        stsubs = typeClauseSpineStateVarClauseSubs spine
        f phrase = 
                ( phrase ^. typePhraseName % uniqueTag
                , SymEntry 
                    ( _SymDataPhrase % _SymPhraseType #
                        ( ( tpvars
                            , phrase ^. typePhraseFrom % to (fmap (substituteTypeVars stsubs))
                            , phrase ^. typePhraseTo % to (substituteTypeVars stsubs))
                        , ( tpvars, phrase ^. typePhraseFrom, phrase ^. typePhraseTo)
                        )
                    ) 
                    $ _SymSeqPhraseCall % _DataDefn # phrase
                )

-- more or less duplicated code
instance CollectSymTermObj (SeqObjTag CodataDefnTag) where
    collectSymTermObj spine = foldMapOf (typeClauseSpineClauses % folded % typeClausePhrases % folded)
                 (pure . f) spine 
      where
        tpvars = foldMapOf (typeClauseSpineClauses % folded) (map NamedType . view typeClauseArgs) spine
        stsubs = typeClauseSpineStateVarClauseSubs spine
        f phrase = 
                ( phrase ^. typePhraseName % uniqueTag
                , SymEntry 
                    ( _SymCodataPhrase % _SymPhraseType #
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
-- | initially collects the symbol table definitions given
-- a recursive group of declarations..
collectSymTabDefn ::
    MonadState (Env SymTab TypeInfoEnv) m => MplDefn MplTypeChecked -> m ()
collectSymTabDefn def = do
    tsymtab <- guse $ envLcl % typeInfoSymTab % symTabTerm
    let ~syms = mempty 
            & symTabTerm .~ symterms 
            & symTabType .~ symtypes
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

        ~symterms = Map.fromList $ case def of 
            ObjectDefn def -> case def of
                SeqObjDefn def -> case def of
                    DataDefn spine -> collectSymTermObj spine
                    CodataDefn spine -> collectSymTermObj spine
            FunctionDefn def -> pure 
                ( def ^. funName % uniqueTag
                , SymEntry (fromJust $ tsymtab ^? at (def ^. funName % uniqueTag) % _Just % symEntryType) 
                    $ _SymSeqCall % _ExprCallFun # def)
            ProcessDefn def -> pure 
                ( def ^. procName % uniqueTag
                , SymEntry (fromJust $ tsymtab ^? at (def ^. procName % uniqueTag) % _Just % symEntryType) 
                    $ _SymRunInfo # def)


    envGbl %= (syms<>)

-- | recollects symbol table definitions after a recursive group
-- is defined.. Note that object definitions do not need recollection
-- but recursive function/process calls need to be recollected....
recollectSymTabDefn ::
    ( MonadState SymTab m ) => (MplDefn MplTypeChecked) -> m ()
recollectSymTabDefn (ObjectDefn _) = return ()
recollectSymTabDefn (FunctionDefn (MplFunction name tp bdy)) = 
    symTabTerm % at (name ^. uniqueTag) % _Just % symEntryType .= _SymFun # tp
recollectSymTabDefn (ProcessDefn (MplProcess name tp bdy)) = 
    symTabTerm % at (name ^. uniqueTag) % _Just % symEntryType .= _SymProc # tp

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
            spine $ \n -> symTabTerm % at n .= Nothing
    

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
    symTabTerm % at (name ^. uniqueTag) .= Nothing
eliminateSymTabDefn (ProcessDefn (MplProcess name tp bdy)) = 
    symTabTerm % at (name ^. uniqueTag) .= Nothing
