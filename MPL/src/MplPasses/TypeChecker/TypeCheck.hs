{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheck where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeEqns
import MplPasses.TypeChecker.TypeCheckObj 
import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeCheckPatt 
import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import Control.Arrow
import Control.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Bool

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List


import Debug.Trace


import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

runTypeCheck' ::
    ( AsTypeCheckErrors err 
    , AsKindCheckErrors err ) =>
    (TopLevel, UniqueSupply) ->
    MplProg MplRenamed ->
    Either [err] (MplProg MplTypeChecked)
runTypeCheck' ~(top, sup) = 
    \case 
        (res, []) -> Right res
        (_, errs) -> Left errs
    . runWriter 
    . (`evalStateT` ( _Env # 
            ( top
            , lsup
            , mempty
            , TypeInfoEnv mempty tag mempty)
        )
      )
    . runTypeCheck
  where
    ~(lsup, rsup) = split sup
    tag = evalState freshTypeTag rsup

runTypeCheck ::
    TypeCheck (MplProg MplRenamed) (MplProg MplTypeChecked)
runTypeCheck (MplProg stmts) = 
    MplProg <$> traverse typeCheckStmts stmts

typeCheckStmts ::
    TypeCheck (MplStmt MplRenamed) (MplStmt MplTypeChecked)
typeCheckStmts (MplStmt defns wheres) = do
    wheres' <- traverse typeCheckStmts wheres
  
    -- (defns', eqns) <- fmap (NE.unzip . NE.fromList) 
    
    (defns, eqns) <- fmap (NE.unzip . NE.fromList) $ typeCheckDefns $ NE.toList defns

    return $ MplStmt defns wheres'

typeCheckDefns ::
    TypeCheck
        [MplDefn MplRenamed] 
        [ ( MplDefn MplTypeChecked
            , ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub]))
        ]
-- same as the rename step.. need to do the magic recursive do in order
-- to get the recursive declarations together properly.
typeCheckDefns (defn : defns) = do
    rec ~defn' <- collectSymTabDefn $ envLcl % typeInfoSymTab .= symtab 
                    >> fmap snd (withFreshTypeTag (typeCheckDefn defn))
        -- envGbl %= (collectSymTab (fmap fst defn')<>)
        defns' <- typeCheckDefns defns
        ~symtab <- guse envGbl
    return $ defn' : defns'
typeCheckDefns [] = return []

collectSymTabDefn ::
    ( AsTypeCheckErrors e 
    , AsKindCheckErrors e
    , MonadWriter [e] m 
    , MonadState TypeCheckEnv m ) => 
    m (MplDefn MplTypeChecked, ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub])) -> 
    m (MplDefn MplTypeChecked, ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub]))
collectSymTabDefn act = do
    ((def, typeeqns), errs) <- listen act
    let syms = SymTab symterms symtypes
        exists = null errs
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
        

        symterms = flip (bool mempty) (null errs) $ Map.fromList $ case def of 
            ObjectDefn def -> case def of
                _ -> undefined
            FunctionDefn def -> undefined

    envGbl %= (syms<>)
    return (def, typeeqns)

typeCheckDefn ::
    TypeCheck (MplDefn MplRenamed) 
            ( (MplDefn MplTypeChecked), ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub]))
typeCheckDefn (ObjectDefn obj) = (((,mempty) . ObjectDefn)) <$> case obj of
        SeqObjDefn obj -> SeqObjDefn <$> case obj of
            DataDefn n -> DataDefn <$> typeCheckTypeClauseSpine n
            CodataDefn n -> CodataDefn <$> typeCheckTypeClauseSpine n
        ConcObjDefn obj -> ConcObjDefn <$> case obj of
            ProtocolDefn n -> ProtocolDefn <$> typeCheckTypeClauseSpine n
            CoprotocolDefn n -> CoprotocolDefn <$> typeCheckTypeClauseSpine n
    {-
typeCheckDefn (FunctionDefn (MplFunction name funtype defn)) = do
    undefined
    funtype' <- flip (maybe (return Nothing)) funtype $ \(froms, to) -> do
        lcl <- guse envLcl
        (bds, froms') <- unzip <$> traverse typeCheckType froms
        (bd, to') <- typeCheckType to 
        envLcl .= lcl
        return $ Just (bd ++ fold bds, froms', to')
    
    defn' <- traverse typeCheckPattsExpr defn

    return $ FunctionDefn $ _MplFunction # (name', funtype', defn')
-}

typeCheckDefn (ProcessDefn proc@(MplProcess name proctype defn)) = do
    st <- guse equality
    sup <- freshUniqueSupply
    ((foralls, proctype') :: ([TypeP MplTypeSub], MplType MplTypeSub)) <- (`evalStateT` (st & uniqueSupply .~ sup)) $ case proctype of    
        Just tp -> do
            ~tp <- fmap fromJust $ kindCheckProcessType tp
            instantiateType tp
        Nothing -> do
            tag <- freshTypeTag
            return $ ([], annotateTypeTag tag proc)

    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypemap <- guse (envLcl % typeInfoEnvMap)
        
    rec envLcl % typeInfoSymTab % symTabTerm % at (name ^. uniqueTag) ?= 
            _SymEntry # (SymSub proctype', _SymRunInfo # proc')

        (ttypephrases, (defn', acceqns)) <- second NE.unzip . NE.unzip <$> 
            traverse (withFreshTypeTag . typeCheckProcessBody) defn

        let proc' = MplProcess name (fromJust $ ttypemap ^? at ttype % _Just % _SymTypeProc) defn'
            ttype' = annotateTypeTag ttype proc
            ttypephrases' = annotateTypeTagToTypePs (NE.toList ttypephrases) $ NE.toList defn
            eqns = TypeEqnsExist ttypephrases' $
                [ TypeEqnsEq (ttype', proctype') ]
                <> map (TypeEqnsEq . (ttype',)) (annotateTypeTags (NE.toList ttypephrases) $ ttypephrases' )
                <> (sconcat acceqns)

    return $ (ProcessDefn proc', (foralls, ttypephrases', [eqns]))


-------------------------
-- Type checking process...
-------------------------
typeCheckProcessBody ::
    TypeCheck
    ( ([MplPattern MplRenamed], [ChIdentR], [ChIdentR])
        , NonEmpty (MplCmd MplRenamed) )
    ( (([MplPattern MplTypeChecked], [ChIdentT], [ChIdentT])
        , NonEmpty (MplCmd MplTypeChecked))
    , [TypeEqns MplTypeSub])
typeCheckProcessBody ((patts, ins, outs), cmds) = do
    ttype <- guse (envLcl % typeInfoEnvTypeTag)

    (ttypepatts, (patts', acceqns)) <- second NE.unzip . NE.unzip <$> 
        traverse (withFreshTypeTag . typeCheckPattern) patts 

    ttypeins <- traverse addChToSymTab ins
    ttypeouts <- traverse addChToSymTab outs

    let (ttypeppatts, ttypeppatts') = annotatesTags ttypepatts patts
        (ttypepins, ttypepins') = annotatesTags ttypeins ins
        (ttypepouts, ttypepouts') = annotatesTags ttypeouts outs

    -- envLcl % typeInfoSymTab % symTabTerm %=
        -- ((Map.fromList $ map (view uniqueTag &&&) ins )<>)
    undefined

-------------------------
-- Kind checking
-------------------------
kindCheckProcessType :: 
    TypeCheck 
        ([TypeP MplRenamed], [MplType MplRenamed], [MplType MplRenamed], [MplType MplRenamed]) 
        (Maybe ([TypeP MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]))
kindCheckProcessType proctype@(varsyms, seqs, ins, outs) = do
    symtab <- guse (envLcl % typeInfoSymTab % symTabType)
    
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
        return (varsyms , seqs', ins', outs')

-------------------------
-- Utilities
-------------------------
addChToSymTab :: TypeCheck ChIdentR TypeTag
addChToSymTab ch = do
    tag <- freshTypeTag
    let typep = annotateTypeTagToTypeP tag ch
        ann = annotateTypeTag tag typep
    envLcl % typeInfoSymTab % symTabTerm %%= 
        ((tag,) . (Map.singleton (ch ^. uniqueTag) (_SymEntry # (SymSub ann , SymChInfo ch))  <>))
    
        -- ((Map.fromList $ map (view uniqueTag &&&) ins )<>)
