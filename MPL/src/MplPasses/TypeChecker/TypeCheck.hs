{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeCheck where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeEqns
import MplPasses.TypeChecker.TypeCheckObj 
import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeCheckPatt 
import MplPasses.TypeChecker.TypeCheckPanic
import MplPasses.TypeChecker.TypeCheckErrorPkg
import MplPasses.TypeChecker.TypeCheckSymUtils 
import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Control.Arrow
import Control.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Bool
import Data.Function
import Data.Traversable
import Data.Coerce

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List
import Debug.Trace

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable

import Data.Functor.Foldable (Base, cata, para)
import Data.Tuple

{- Module for type checking..  -}
runTypeCheck' ::
    ( AsAllTypeCheckErrors err ) =>
    (TopLevel, UniqueSupply) ->
    MplProg MplRenamed ->
    Either [err] (MplProg MplTypeChecked)
runTypeCheck' ~(top, sup) = 
    \case (res, []) -> Right res ; (_, errs) -> Left errs
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
    forall e m0 m1 symm n. 
    ( AsAllTypeCheckErrors e

    , MonadWriter [e] n 
    , MonadWriter [e] symm

    , MonadFix n 

    , Zoom symm n SymTab TypeCheckEnv
    , SymZooms m0 m1 symm
    ) =>
    MplProg MplRenamed -> n (MplProg MplTypeChecked)
runTypeCheck (MplProg stmts) = MplProg . map fst <$> traverse typeCheckStmt stmts

typeCheckStmt ::
    forall e m0 m1 symm n. 
    ( AsAllTypeCheckErrors e

    , MonadWriter [e] n 
    , MonadWriter [e] symm

    , MonadFix n 

    , Zoom symm n SymTab TypeCheckEnv
    , SymZooms m0 m1 symm
    ) =>
    MplStmt MplRenamed -> 
    n (MplStmt MplTypeChecked, [Sub MplTypeSub])
typeCheckStmt (MplStmt defns wheres) = do
    notscoped <- zoom envGbl $ collectNotScopedSymTabTypeVariables

    wheres' <- fmap (map fst) $ traverse typeCheckStmt wheres

    envLcl % typeInfoSymTab .= mempty
    rec envLcl % typeInfoEnvMap .= tagmap
        ~((defns', eqns), errpkg) <- fmap (first (NE.unzip . NE.fromList)) 
            $ runWriterT 
            $ typeCheckDefns 
            $ NE.toList defns
        let terrs = collectPkgErrors errpkg
            erroccured = hasn't _Empty 
                {-  Previously tried filtering things out, but doesn't really work.
                 filter (has _CannotCallTypeCts) 
                 filter (has _CannotCallCh) 
                 filter (has _CannotCallTerm) 
                 -}
                    terrs

            foralls = foldMap (view _1) eqns
            exists = foldMap (view _2) eqns
            subs = foldMap (view _3) eqns
            eqns' = TypeEqnsForall foralls $ [TypeEqnsExist exists subs]
            pkg = runExcept 
                $ bool (return ()) 
                    (throwError mempty) erroccured 
                    >> withExceptT pure (solveTypeEqns eqns') 
            subs' = either mempty (view packageSubs) pkg
                                                                          
            notscoped' = concatMap (`variableClosure` subs') $ map typeTToTypeIdentT notscoped

        tagmap <- packageToTypeTagMap notscoped'
                $ either mempty id (pkg :: Either [e] (Package MplTypeSub))

    tell terrs
    tell $ either id mempty pkg

    -- need to replace definitions in the symbol table here for
    -- functions. Moreover, illegally called functions need listening..
    -- If the function did type check, then we "recollect" the functions types
    -- and generalize their polymorphic types with for alls. Otherwise,
    -- we remove them from the symbol table and don't use them to type check anymore
    -- (if they are called, I think an "Internal" error is thrown? I can't 
    -- quite remember..)
    zoom envGbl $ bool 
        (traverse eliminateSymTabDefn defns') 
        (traverse recollectSymTabDefn defns') 
        (has _Right pkg)

    -- swap the buffers..
    symtab' <- guse envGbl
    envLcl % typeInfoSymTab .= symtab'

    return (MplStmt defns' wheres', subs')

typeCheckDefns ::
    TypeCheck
        [MplDefn MplRenamed] 
        [ ( MplDefn MplTypeChecked
          , ( [([TypeP MplTypeSub], TypeP MplTypeSub, MplType MplTypeSub)]
            , [TypeP MplTypeSub], [TypeEqns MplTypeSub])
          ) 
        ]
-- same as the rename step.. need to do the magic recursive do in order
-- to get the recursive declarations together properly.
typeCheckDefns (defn : defns) = do
    rec ~(defn', eqns) <- envLcl % typeInfoSymTab .= symtab 
                    >> fmap snd (withFreshTypeTag (typeCheckDefn defn))
        nsyms <- zoom (envLcl % typeInfoSymTab) $ collectSymTabDefn defn'
        envGbl %= (nsyms<>)
        defns' <- typeCheckDefns defns
        symtab <- guse envGbl
    return $ (defn', eqns) : defns'
typeCheckDefns [] = return []


typeCheckDefn ::
    TypeCheck (MplDefn MplRenamed) 
            ( MplDefn MplTypeChecked
            , ( [([TypeP MplTypeSub], TypeP MplTypeSub, MplType MplTypeSub)]
                , [TypeP MplTypeSub], [TypeEqns MplTypeSub]
              )
            )
typeCheckDefn (ObjectDefn obj) = (((,mempty) . ObjectDefn)) <$> case obj of
        SeqObjDefn obj -> SeqObjDefn <$> case obj of
            DataDefn n -> DataDefn <$> typeCheckTypeClauseSpine n
            CodataDefn n -> CodataDefn <$> typeCheckTypeClauseSpine n
        ConcObjDefn obj -> ConcObjDefn <$> case obj of
            ProtocolDefn n -> ProtocolDefn <$> typeCheckTypeClauseSpine n
            CoprotocolDefn n -> CoprotocolDefn <$> typeCheckTypeClauseSpine n

typeCheckDefn (FunctionDefn fun@(MplFunction name Nothing defn)) = do
    tag <- freshTypeTag
    let tp = typePtoTypeVar $ annotateTypeTag tag fun
        (funtype', symtp) = (tp, _SymImplicit # tp)

    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypemap <- guse (envLcl % typeInfoEnvMap)
    
    rec let funsymentry = _SymEntry # (symtp, _SymSeqCall % _ExprCallFun # fun')
        envLcl % typeInfoSymTab % symTabExpr % at (name ^. uniqueTag) ?= funsymentry

        ~(ttypephrases, (defn', acceqns)) <- second NE.unzip . NE.unzip <$> 
            traverse (withFreshTypeTag . typeCheckFunBody ) defn

        let fun' = MplFunction name 
                (fromJust $ ttypemap ^? at ttype % _Just % _SymTypeSeq) defn'
            ttypep =  annotateTypeTag ttype fun
            ttypep' = typePtoTypeVar $ ttypep
            ttypephrases' = annotateTypeTags (NE.toList ttypephrases) $ NE.toList defn
            eqns = 
                [ TypeEqnsEq (ttypep', funtype') ]
                <> map (TypeEqnsEq . (ttypep',) . typePtoTypeVar ) ttypephrases'
                <> sconcat acceqns
    return (FunctionDefn fun', ([], ttypep : ttypephrases', eqns))

-- more or less duplicated code from the exists case except for some small modifications regarding the user defined type.
typeCheckDefn (FunctionDefn fun@(MplFunction name (Just tp) defn)) = do
    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    let ttypep =  annotateTypeTag ttype fun
        ttypep' = typePtoTypeVar $ ttypep

    (funtype', symtp) <- do
        tp <- kindCheckFunType tp
        arrenv <- freshInstantiateArrEnv
        let (inst, insttp) = runInstantiateArrType
                (instantiateArrType (_Just % _TypeAnnFun # fun) tp) arrenv
        return ([(inst, ttypep, insttp)], _SymExplicit # tp)
    
    rec let funsymentry = _SymEntry # (symtp, _SymSeqCall % _ExprCallFun # fun')
        envLcl % typeInfoSymTab % symTabExpr % at (name ^. uniqueTag) ?= funsymentry

        ~(ttypephrases, (defn', acceqns)) <- second NE.unzip . NE.unzip <$> 
            traverse (withFreshTypeTag . typeCheckFunBody ) defn

        let fun' = MplFunction name (fromJust $ ttypemap ^? at ttype % _Just % _SymTypeSeq) defn'
            ttypephrases' = annotateTypeTags (NE.toList ttypephrases) $ NE.toList defn
            eqns = map (TypeEqnsEq . (ttypep',) . typePtoTypeVar ) ttypephrases' <> sconcat acceqns
    return (FunctionDefn fun', (funtype', ttypep : ttypephrases', eqns))

-- some duplciated code...
typeCheckDefn (ProcessDefn procc@(MplProcess name Nothing defn)) = do
    tag <- freshTypeTag
    let tp = typePtoTypeVar $ annotateTypeTag tag procc
        proctype' = tp
        symtp = _SymImplicit # tp

    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    envLcl % typeInfoSymTab % symTabCh .= mempty
    
    rec let procsymentry = _SymEntry # (symtp, _SymRunInfo # procc')
        envLcl % typeInfoSymTab % symTabConc % at (name ^. uniqueTag) ?= procsymentry

        (ttypephrases, (defn', acceqns)) <- second NE.unzip . NE.unzip <$> 
            traverse (withFreshTypeTag . typeCheckProcessBody) defn

        let procc' = MplProcess name (fromJust $ ttypemap ^? at ttype % _Just % _SymTypeConc) defn'
            ttypep = annotateTypeTag ttype procc
            ttypephrases' = annotateTypeTags (NE.toList ttypephrases) $ NE.toList defn
            eqns = 
                [ TypeEqnsEq (typePtoTypeVar ttypep, proctype') ]
                <> map (TypeEqnsEq . (typePtoTypeVar ttypep,) . typePtoTypeVar) ttypephrases'
                <> sconcat acceqns

    return $ (ProcessDefn procc', ([], ttypep : ttypephrases', eqns)) 

-- more or less duplciated code
typeCheckDefn (ProcessDefn procc@(MplProcess name (Just tp) defn)) = do
    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    let ttypep = annotateTypeTag ttype procc
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    (proctype', symtp) <- do
        ~tp <- fmap fromJust $ kindCheckProcessType tp
        arrenv <- freshInstantiateArrEnv
        let (inst, insttp) = runInstantiateArrType     
                (instantiateArrType (_Just % _TypeAnnProc # procc) tp)
                arrenv 
        return ([(inst, ttypep, insttp)], _SymExplicit # tp)

    {-
    (funtype', symtp) <- do
        tp <- kindCheckFunType tp
        arrenv <- freshInstantiateArrEnv
        let (inst, insttp) = runInstantiateArrType
                (instantiateArrType (_Just % _TypeAnnFun # fun) tp) arrenv
        -- return ((inst, insttp), SymFun tp)
        return ([(inst, ttypep, insttp)], _SymExplicit # tp)
        -}

    
    envLcl % typeInfoSymTab % symTabCh .= mempty
    rec let procsymentry = _SymEntry # (symtp, _SymRunInfo # procc')
        envLcl % typeInfoSymTab % symTabConc % at (name ^. uniqueTag) ?= procsymentry

        (ttypephrases, (defn', acceqns)) <- second NE.unzip . NE.unzip <$> 
            traverse (withFreshTypeTag . typeCheckProcessBody) defn

        let procc' = MplProcess name (fromJust $ ttypemap ^? at ttype % _Just % _SymTypeConc) defn'
            ttypephrases' = annotateTypeTags (NE.toList ttypephrases) $ NE.toList defn
            eqns = -- TypeEqnsExist ttypephrases' $
                -- [ TypeEqnsEq (typePtoTypeVar ttypep, proctype') ]
                -- <> map (TypeEqnsEq . (ttype',)) (annotateTypeTags (NE.toList ttypephrases) $ ttypephrases' )
                map (TypeEqnsEq . (typePtoTypeVar ttypep,) . typePtoTypeVar) ttypephrases'
                <> sconcat acceqns

    return $ (ProcessDefn procc', (proctype', ttypep : ttypephrases', eqns)) 

-------------------------
-- Type checking expressions
-------------------------
typeCheckFunBody ::
    TypeCheck
        ([MplPattern MplRenamed], MplExpr MplRenamed)
        (([MplPattern MplTypeChecked], MplExpr MplTypeChecked), 
            [TypeEqns MplTypeSub] )
typeCheckFunBody bdy@(patts, expr) = do
    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    ~(ttypepatts, (patts', pattacceqns)) <- second NE.unzip . NE.unzip <$> 
        traverse (withFreshTypeTag . typeCheckPattern) patts 

    ~(ttypeexpr, (expr', expracceqn)) <- withFreshTypeTag . typeCheckExpr $ expr

    let ttypep = annotateTypeTag ttype bdy
        ttypepexpr = annotateTypeTag ttypeexpr expr
        ttypeppatts = annotateTypeTags ttypepatts patts

        eqn = TypeEqnsExist (ttypepexpr:ttypeppatts) $
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , mkTypeSubSeqArr (_Just % _TypeAnnFunPhrase # bdy)
                        ( map typePtoTypeVar ttypeppatts
                        , typePtoTypeVar ttypepexpr )
                    )
                ]
                <> concat pattacceqns
                <> expracceqn

    return ((patts', expr'), [eqn])


typeCheckExpr ::
    TypeCheck
        (MplExpr MplRenamed)
        (MplExpr MplTypeChecked, [TypeEqns MplTypeSub])
typeCheckExpr = para f
  where
    f :: Base (MplExpr MplRenamed) (MplExpr MplRenamed, _ (MplExpr MplTypeChecked, [TypeEqns MplTypeSub])) ->
        _ (MplExpr MplTypeChecked, [TypeEqns MplTypeSub])
    f (EVarF cxt n) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry lkuptp (SymSeqCall lkupdef)) <- fmap fromJust $ zoom (envLcl % typeInfoSymTab ) $ lookupSymExpr n

        arrenv <- freshInstantiateArrEnv
        let ann = _EVar # (cxt, n) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann
            
            -- lkuptp' = fromJust $ lkuptp ^? _SymSeqCallType % _SymImplicit
            {- Note: This is a little peculiar! we can have a program like:
             - fun undefined :: -> A =
             -      -> undefined
             - So, this is a valid call because it takes no arguments ( undefined is the same as undefined() )
             - But this indeed has an explicit type... indeed, this is identical to the ECallF case..
            -}
            (ttypeargs, lkuptp') = (`runInstantiateArrType`arrenv)
                $ fromJust 
                $ instantiateArrType (_Just % _TypeAnnExpr # ann) 
                    <$> lkuptp ^? _SymSeqCallType

            eqn = TypeEqnsExist ttypeargs $ 
                [ TypeEqnsEq (typePtoTypeVar ttypep , lkuptp') 
                ] 

            {-
            res = case lkupdef of 
                ExprCallPattern patt -> EVar (patt, fromJust $ ttypemap ^? at ttype % _Just % _SymTypeSeq ) n
                ExprCallFun fun -> ECall (fun, fromJust $ ttypemap ^? at ttype % _Just % _SymTypeSeq) n []
            -}
            res = case lkupdef of 
                ExprCallPattern patt -> EVar (fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) n
                ExprCallFun fun -> ECall (fromJust $ lookupInferredSeqTypeExpr ttype ttypemap) n []

        return (res, [eqn])

    f (EBuiltInOpF _ _ _ _) = panicNotImplemented

    f (EPOpsF cxt op (lexpr, ml) (rexpr, mr)) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        (ttypel, (l', leqns)) <- withFreshTypeTag ml
        (ttyper, (r', reqns)) <- withFreshTypeTag mr

        let opexpr = EPOps cxt op lexpr rexpr :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype opexpr
            ttypelp = annotateTypeTag ttypel lexpr
            ttyperp = annotateTypeTag ttyper rexpr

        let addsubmul = 
                let eqn = TypeEqnsExist [ttypelp, ttyperp] $ 
                        [ TypeEqnsEq (typePtoTypeVar ttypep, typePtoTypeVar ttypelp )
                        , TypeEqnsEq (typePtoTypeVar ttypep, typePtoTypeVar ttyperp )
                        , TypeEqnsEq (typePtoTypeVar ttypep, _TypeIntF % _Just % _TypeAnnExpr # opexpr )
                        , TypeEqnsEq (typePtoTypeVar ttypelp, _TypeIntF % _Just % _TypeAnnExpr # lexpr )
                        , TypeEqnsEq (typePtoTypeVar ttyperp, _TypeIntF % _Just % _TypeAnnExpr # rexpr )
                        ]
                        <> leqns
                        <> reqns
                in
                    ( EPOps ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) op l' r'
                    , [eqn]
                    ) 
            -- duplicated code from @addsubmul@
            div = 
                let eqn = TypeEqnsExist [ttypelp, ttyperp] $ 
                        [ TypeEqnsEq (typePtoTypeVar ttypep, typePtoTypeVar ttypelp )
                        , TypeEqnsEq (typePtoTypeVar ttypep, typePtoTypeVar ttyperp )
                        , TypeEqnsEq (typePtoTypeVar ttypep, _TypeDoubleF % _Just % _TypeAnnExpr # opexpr )
                        , TypeEqnsEq (typePtoTypeVar ttypelp, _TypeDoubleF % _Just % _TypeAnnExpr # lexpr )
                        , TypeEqnsEq (typePtoTypeVar ttyperp, _TypeDoubleF % _Just % _TypeAnnExpr # rexpr )
                        ]
                        <> leqns
                        <> reqns
                in
                    ( EPOps ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) op l' r'
                    , [eqn]
                    ) 
            eqneq = 
                let eqn = TypeEqnsExist [ttypelp, ttyperp] $
                        [ TypeEqnsEq (typePtoTypeVar ttypep, _TypeBoolF % _Just % _TypeAnnExpr # opexpr)
                        , TypeEqnsEq (typePtoTypeVar ttypelp, typePtoTypeVar ttyperp)
                        ]
                        <> leqns
                        <> reqns
                in
                    ( EPOps ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) op l' r'
                    , [eqn]
                    ) 

            -- inequalities on numbers
            ineq = 
                let eqn = TypeEqnsExist [ttypelp, ttyperp] $ 
                        [ TypeEqnsEq (typePtoTypeVar ttypep, _TypeBoolF % _Just % _TypeAnnExpr # opexpr)

                        , TypeEqnsEq (typePtoTypeVar ttypelp, typePtoTypeVar ttyperp)
                        , TypeEqnsEq (typePtoTypeVar ttypelp, _TypeIntF % _Just % _TypeAnnExpr # lexpr )
                        , TypeEqnsEq (typePtoTypeVar ttyperp, _TypeIntF % _Just % _TypeAnnExpr # rexpr )
                        ]
                        <> leqns
                        <> reqns
                in
                    ( EPOps ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) op l' r'
                    , [eqn]
                    ) 
            
            -- colon operation
            colon =
                let eqn = TypeEqnsExist [ttypelp, ttyperp] $
                        [ TypeEqnsEq (typePtoTypeVar ttypep, typePtoTypeVar ttyperp)
                        , TypeEqnsEq (typePtoTypeVar ttyperp, _TypeListF # (_Just % _TypeAnnExpr # opexpr, typePtoTypeVar ttypelp))
                        ]
                        <> leqns
                        <> reqns
                in
                    ( EPOps ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) op l' r'
                    , [eqn]
                    ) 


        return $ case op of
            PrimitiveAdd -> addsubmul
            PrimitiveSub -> addsubmul
            PrimitiveMul -> addsubmul
            PrimitiveDiv -> div

            PrimitiveEq -> eqneq
            PrimitiveNeq -> eqneq

            PrimitiveLt -> ineq
            PrimitiveLeq -> ineq
            PrimitiveGt -> ineq
            PrimitiveGeq -> ineq

            PrimitiveColon -> colon
            -- _ ->  error $ "not implemented op: " ++ show op


    f (EIntF cxt n) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann =  _EInt # (cxt, n) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann
            eqns = [ TypeEqnsEq (typePtoTypeVar ttypep, _TypeIntF % _Just % _TypeAnnExpr # ann ) ]

        return ( EInt ( cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) n, eqns )

    -- duplicated code from EIntF case
    f (EDoubleF cxt n) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann =  _EDouble # (cxt, n) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann
            eqns = [ TypeEqnsEq (typePtoTypeVar ttypep, _TypeDoubleF % _Just % _TypeAnnExpr # ann ) ]
        return ( EDouble ( cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) n, eqns )

    -- duplicated code from the EIntF case
    f (ECharF cxt n) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann =  _EChar # (cxt, n) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann
            eqns = 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , _TypeCharF % _Just % _TypeAnnExpr # ann 
                    ) 
                ]
        return ( EChar ( cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) n, eqns )
    
    -- duplicated code from the EIntF case
    f (EBoolF cxt n) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann =  _EBool # (cxt, n) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann
            eqns = 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , _TypeBoolF % _Just % _TypeAnnExpr # ann 
                    ) 
                ]
        return ( EBool ( cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) n, eqns )

    f (ETupleF cxt (t0,t1,ts)) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        (ttypet0, (t0', t0eqns)) <- withFreshTypeTag $ snd t0
        (ttypet1, (t1', t1eqns)) <- withFreshTypeTag $ snd t1
        (ttypets, (ts', tseqns)) <- fmap (second unzip . unzip) $ traverse (withFreshTypeTag . snd) ts

        let tuplexpr = _ETuple # (cxt, (fst t0, fst t1, map fst ts)) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype tuplexpr
            ttypept0 = annotateTypeTag ttypet0 $ fst t0
            ttypept1 = annotateTypeTag ttypet1 $ fst t1
            ttypepts = annotateTypeTags ttypets $ map fst ts

            eqn = TypeEqnsExist (ttypept0:ttypept1:ttypepts) $ 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , _TypeTupleF # 
                        ( _Just % _TypeAnnExpr # tuplexpr
                        , 
                            ( typePtoTypeVar ttypept0
                            , typePtoTypeVar ttypept1
                            , map typePtoTypeVar ttypepts
                            )
                        )
                    )
                ]
                <> t0eqns
                <> t1eqns
                <> concat tseqns

        return 
            ( _ETuple # 
                ( (cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
                , (t0',t1',ts')
                )
            , [eqn]
            )

    {- more built in types... -}
    f (EListF cxt ts) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        (ttypets, (ts', tseqns)) <- fmap (second unzip . unzip) $ traverse (withFreshTypeTag . snd) ts

        tp <- freshTypeTag

        let ann = _EList # (cxt, map fst ts) :: MplExpr MplRenamed
            tpp = annotateTypeTag tp ann
            ttypep = annotateTypeTag ttype ann
            ttypepts = annotateTypeTags ttypets $ map fst ts

            eqns = TypeEqnsExist [_TypeIdentT # (tp, TypeIdentTInfoTypeAnn $ TypeAnnExpr ann)] $
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , _TypeListF # 
                        ( Just $ TypeAnnExpr ann
                        , typePtoTypeVar $ tpp
                        )
                    )
                ] 
                <> map (TypeEqnsEq . (typePtoTypeVar tpp,) . typePtoTypeVar . uncurry annotateTypeTag)
                    (zip ttypets (map fst ts))
                <> concat tseqns

        return 
            ( _EList # 
                ( (cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
                , ts'
                )
            , [eqns]
            )

    f (EStringF cxt str) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _EString # (cxt, str) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann

            eqns = 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , _TypeListF # 
                        ( Just $ TypeAnnExpr ann
                        , _TypeCharF # (Just $ TypeAnnExpr ann)
                        )
                    )
                ] 

        return 
            ( _EString # 
                ( (cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
                , str
                )
            , eqns
            )

    f (EUnitF cxt) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _EUnit # cxt :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ann

            eqns = 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , _TypeUnitF # Just (TypeAnnExpr ann)
                    )
                ]

        return
            ( _EUnit # (cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
            , eqns
            )

    {- end of built in types -}

    f (EIfF cxt mcond mthenc melsec) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        (ttypemcond, (mcond', mcondeqns)) <- withFreshTypeTag $ snd mcond
        (ttypemthenc, (mthenc', mthenceqns)) <- withFreshTypeTag $ snd mthenc
        (ttypemelsec, (melsec', melseceqns)) <- withFreshTypeTag $ snd melsec

        let ifexpr = _EIf # (cxt, fst mcond, fst mthenc, fst melsec) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype ifexpr

            ttypetmcond = annotateTypeTag ttypemcond $ fst mcond
            ttypetmthenc = annotateTypeTag ttypemthenc $ fst mthenc
            ttypetmelsec = annotateTypeTag ttypemelsec $ fst melsec

            -- eqn = TypeEqnsExist (ttypept0:ttypept1:ttypepts) $ 
            eqn = TypeEqnsExist [ttypetmcond, ttypetmthenc, ttypetmelsec] $ 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , typePtoTypeVar ttypetmthenc
                    )
                , TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , typePtoTypeVar ttypetmelsec 
                    )
                , TypeEqnsEq 
                    ( typePtoTypeVar ttypetmcond
                    , _TypeBoolF # Just (TypeAnnExpr $ fst mcond)
                    )
                ]
                <> mcondeqns
                <> mthenceqns
                <> melseceqns 

        return 
            ( _EIf #
                ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap
                , mcond'
                , mthenc'
                , melsec'
                )
            , [eqn]
            )
    -- ESwitch
    f (ESwitchF cxt switches) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ((ttypestbexprs, bexpreqns), (ttypetexprs, expreqns), switches') <- fmap 
            (over _2 unzip . over _1 unzip . unzip3 . NE.toList) 
            $ for switches $ \(mbexpr, mexpr) -> do
                (ttypebexpr, (bexpr', bexpreqns)) <- withFreshTypeTag $ snd mbexpr
                (ttypeexpr, (expr', expreqns)) <- withFreshTypeTag $ snd mexpr

                let ttypetbexpr = annotateTypeTag ttypebexpr $ fst mbexpr
                    ttypetexpr = annotateTypeTag ttypeexpr $ fst mexpr

                return 
                    -- ( (ttypetbexpr, (bexpr', bexpreqns)) , (ttypetexpr , (expr', expreqns)))
                    -- ( (ttypetbexpr, (, bexpreqns)) , (ttypetexpr , (expr', expreqns)))
                    ( (ttypetbexpr, bexpreqns)
                    , (ttypetexpr, expreqns)
                    , ( bexpr', expr')
                    )
        let switchexpr = _ESwitch # (cxt, switches & mapped %~ (fst***fst) ) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype switchexpr

            eqn = TypeEqnsExist (ttypestbexprs ++ ttypetexprs) $ 
                -- codomain should all be the same type / type of overal expression
                map (TypeEqnsEq . (typePtoTypeVar ttypep,) . typePtoTypeVar) ttypetexprs
                -- TODO: fix the annotation information here..
                <> zipWith 
                    (\ann ttypet -> TypeEqnsEq 
                        ( typePtoTypeVar ttypet
                        , _TypeBoolF # Just (TypeAnnExpr $ ann)
                        )
                    )
                    (map (fst . fst) $ NE.toList switches)
                    ttypestbexprs
                <>  concat bexpreqns
                <>  concat expreqns

        return
            ( _ESwitch #
                ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap
                , NE.fromList switches'
                )
            , [eqn]
            )

    f (ELetF cxt lets (_, mexpr)) = do
        st <- guse equality
        sup <- freshUniqueSupply

        -- traverse typeCheckStmt stmts
        let ~((lets', subs), errs) 
                = first unzip
                $ runWriter 
                $ flip evalStateT 
                    -- some awkwardness here that we need to update the 
                    -- global symbol table...
                    ( st & uniqueSupply .~ sup 
                         & envGbl .~ st ^. envLcl % typeInfoSymTab ) 
                $ traverse typeCheckStmt
                $ NE.toList lets

        tell $ review _ExternalError $ errs

        notinscope <- zoom (envLcl % typeInfoSymTab) $ collectNotScopedSymTabTypeVariables 

        zoom (envLcl % typeInfoSymTab) $ do
            nsyms <- traverse (traverse collectSymTabDefn . view stmtDefns) lets'
            equality %= (foldOf (folded % folded) nsyms<>)
            traverse_ (traverse_ recollectSymTabDefn . view stmtDefns) lets'

        (expr', expreqns) <- mexpr

        return 
            ( _ELet # (cxt, NE.fromList lets', expr') 
            , concat 
                [ expreqns 
                , concatMap (map (\(t,ty) -> TypeEqnsEq (TypeVar Nothing t ,ty))) subs
                ]
            ) 

    f (EFoldF cxt (foldonexpr, foldon) (phrase :| phrases)) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let foldexpr = EFold cxt foldonexpr (over (mapped % _4) fst $ phrase :| phrases) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype foldexpr

        (ttypefoldon, (foldon', foldoneqns)) <- withFreshTypeTag foldon
        let ttypepfoldon = annotateTypeTag ttypefoldon foldonexpr 
        {- Remarks..
         - Given a fold expression:
         - fold a of
         -  Constructor0 : a b c -> expr0
         -  Constructor1 : a b c -> expr1
         -  Constructor2 : a b c -> expr2
         -
         - The type of the overall expression is the type of the state 
         - variable for THE FIRST CONSTRUCTOR even given a mutually 
         - recursive case (we what seems to be suggested by Prashant's examples)....
         -}

        arrenv <- freshInstantiateArrEnv
        ((phrases', phraseeqns), instt) <- fmap 
            (second (toListOf (instantiateArrEnvInstantiated % folded)))
            $ flip runStateT arrenv $ do
                -- the first phrase is the type of the overall expresion... 
                let (cxt, ident, patts, (expr, mexpr)) = phrase

                ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                    $ zoom (envLcl % typeInfoSymTab) 
                    $ lookupSymExprDataPhrase (ident, expr)

                (ttypepfroms, ttypestvar, ttypeclause) <- state $ runState $ do
                    let (freevars, froms, to) = fromJust $ lkuptp ^? _SymDataPhrase % originalType 
                    subs <- updateInstantiatedAndGetSubs freevars
                       
                    return ( fromJust $ traverse (instantiateTypeWithSubs (_Just % _TypeAnnExpr # foldexpr) subs) froms
                           , fromJust $ instantiateTypeWithSubs (_Just % _TypeAnnExpr # foldexpr) subs to
                           , seqdef ^. typePhraseExt % Optics.to 
                                ( fromJust 
                                . instantiateTypeWithSubs (_Just % _TypeAnnExpr # foldexpr) subs
                                . typeClauseToMplType 
                                )
                           )

                (ttypepatts, (patts', pattseqns)) <- lift 
                    $ fmap (second unzip <<< unzip) 
                    $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                let reachablephrases = foldMapOf 
                        ( typePhraseExt 
                        % typeClauseExt 
                        % typeClauseSpineClauses 
                        % folded 
                        % typeClausePhrases 
                        % folded 
                        % typePhraseName ) (:[]) seqdef
                    phrase' = (seqdef, ident, patts', expr')
                    ttypeppatts = annotateTypeTags ttypepatts patts
                    ttypepexpr = annotateTypeTag ttypeexpr expr
                    phraseeqn = TypeEqnsExist (ttypepexpr : ttypeppatts) $ 
                        -- the type of the overall expression is the type of 
                        -- the first state variable that is being folded to
                        [ TypeEqnsEq (typePtoTypeVar ttypep, ttypestvar) 
                        -- moreover, the fold on should be the same type as this
                        -- as the clause of the first fold phrase....
                        , TypeEqnsEq (typePtoTypeVar ttypepfoldon,  ttypeclause) 
                        , TypeEqnsEq 
                            ( mkTypeSubSeqArr (_Just % _TypeAnnExpr # foldexpr) (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                            , mkTypeSubSeqArr (_Just % _TypeAnnExpr # foldexpr) (ttypepfroms, ttypestvar)  
                            )
                        ] 
                        <> expreqns
                        <> fold pattseqns

                (phrases', phraseseqns) <- fmap unzip 
                    $ for phrases 
                    -- duplciated code, but we remove the requirement of the 
                    -- state variable being the type of the overall expression
                    -- REMARK: We should really test if each of the phrases 
                    -- are indeed from the same data graph here!! TODO in the
                    -- future (THIS REALLY SHOULD BE DONE)!
                    $ \(cxt, ident, patts, (expr, mexpr)) -> do
                        ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                            $ zoom (envLcl % typeInfoSymTab) 
                            $ lookupSymExprDataPhrase (ident, expr)

                        tell $ review _ExternalError $ bool
                            []
                            [_ExpectedFoldPhraseToBeEitherButGot # (reachablephrases, seqdef ^. typePhraseName)]
                            $ seqdef ^. typePhraseName `notElem` reachablephrases

                        (ttypepfroms, ttypestvar) <- state $ runState $ do
                            let (freevars, froms, to) = fromJust $ lkuptp ^? _SymDataPhrase % originalType 
                            subs <- updateInstantiatedAndGetSubs freevars
                               
                            return ( fromJust $ traverse (instantiateTypeWithSubs (_Just % _TypeAnnExpr # foldexpr) subs) froms
                                   , fromJust $ instantiateTypeWithSubs (_Just % _TypeAnnExpr # foldexpr) subs to)

                        (ttypepatts, (patts', pattseqns)) <- lift 
                            $ fmap (second unzip <<< unzip) 
                            $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                        (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                        let phrase' = (seqdef, ident, patts', expr')
                            ttypeppatts = annotateTypeTags ttypepatts patts
                            ttypepexpr = annotateTypeTag ttypeexpr expr
                            phraseeqn = TypeEqnsExist (ttypepexpr : ttypeppatts) $ 
                                -- patterns should match the type of constructor without 
                                [ TypeEqnsEq 
                                    ( mkTypeSubSeqArr (_Just % _TypeAnnExpr # foldexpr) (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                                    , mkTypeSubSeqArr (_Just % _TypeAnnExpr # foldexpr) (ttypepfroms, ttypestvar)  
                                    )
                                ] 
                                <> expreqns
                                <> fold pattseqns
                        return (phrase', phraseeqn)

                return (phrase' :| phrases', phraseeqn : phraseseqns)

        let eqns = TypeEqnsExist (ttypepfoldon : instt) $ phraseeqns <> foldoneqns
        return 
            ( _EFold # 
                ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap
                , foldon'
                , phrases'
                )
            , [eqns]
            )

    f (EUnfoldF cxt (unfoldorigexpr, unfoldonexpr) (phrase :| phrases)) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let unfoldexpr = EUnfold cxt unfoldorigexpr 
                (over (mapped % _3 % mapped % _4) fst $ phrase :| phrases) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype unfoldexpr

        (ttypeunfoldonexpr, (unfoldonexpr', unfoldonexpreqns)) <- withFreshTypeTag unfoldonexpr
        let ttypepunfoldonexpr = annotateTypeTag ttypeunfoldonexpr unfoldorigexpr 
        {- Remarks..
         - Given an unfold expression:
         - unfold a of
         -      pattern0 of
         -          Destructor0A : a b c -> expr0A
         -          Destructor0B : a b c -> expr0B
         -      pattern1 of
         -          Destructor1A : a b c -> expr1A
         -          Destructor1B : a b c -> expr1B
         -
         - The type of the overall expression is the type of the state 
         - variable for THE FIRST unfold phrase, and the type of the type
         - of the whole expression is the type of the clause in the FIRST
         - unfold phrase
         -
         - TODO -- we need to check for unreachable unfold phrases! 
         -}

        arrenv <- freshInstantiateArrEnv
        ((phrases', phraseeqns), instt) <- fmap 
            (second (toListOf (instantiateArrEnvInstantiated % folded)))
            $ flip runStateT arrenv $ do
                -- the first phrase is the type of the overall expresion... 
                let (cxt, unfoldonpatt, phrasesubphrases@((_, ident, _ ,_):|_)) = phrase

                -- get rid of this at the end
                (ttypeunfoldonpatt, (unfoldonpatt', unfoldonpatteqns)) <- lift $ withFreshTypeTag $ typeCheckPattern unfoldonpatt
                let ttypepunfoldonpatt = annotateTypeTag ttypeunfoldonpatt unfoldonpatt

                ttypesubphrase <- freshTypeTag
                let ttypepsubphrase = annotateTypeTag ttypesubphrase unfoldexpr

                -- need to compute the reachable phrases..
                ~seqdef <- lift 
                    $ fmap (fromJust . fromJust)
                    $ zoom (envLcl % typeInfoSymTab) 
                    $ runZoomedLookup
                    $ zoomSymExpr ident 
                    $ zoomSymExprSeqPhrase 
                    $ guses equality (preview _CodataDefn)

                let reachablephrases = foldMapOf 
                        ( typePhraseExt 
                        % typeClauseExt 
                        % typeClauseSpineClauses 
                        % folded 
                        % typeClausePhrases 
                        % folded 
                        % typePhraseName ) (:[]) seqdef
                
                (phrasesubphrases', phrasesubphraseseqns) <- fmap NE.unzip $ for phrasesubphrases $ \(cxt, ident, patts, (expr, mexpr)) -> do
                    ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                        $ zoom (envLcl % typeInfoSymTab) 
                        $ lookupSymExprCodataPhrase (ident, expr)

                    tell $ review _ExternalError $ bool
                        []
                        [ _ExpectedUnfoldPhraseToBeEitherButGot # 
                            ( reachablephrases
                            , seqdef ^. typePhraseName)
                            ]
                        $ seqdef ^. typePhraseName `notElem` reachablephrases

                    (ttypepfroms, ttypestvar, ttypeto, ttypeclause) <- state $ runState $ do
                        let (freevars, (froms, st), to) = fromJust $ lkuptp ^? _SymCodataPhrase % originalType 
                        subs <- updateInstantiatedAndGetSubs freevars
                           
                        return ( fromJust $ traverse (instantiateTypeWithSubs (_Just % _TypeAnnExpr # unfoldexpr) subs) froms
                               , fromJust $ instantiateTypeWithSubs (_Just % _TypeAnnExpr # unfoldexpr) subs st
                               , fromJust $ instantiateTypeWithSubs (_Just % _TypeAnnExpr # unfoldexpr) subs to
                               , seqdef ^. typePhraseExt % Optics.to 
                                    ( fromJust 
                                    . instantiateTypeWithSubs (_Just % _TypeAnnExpr # unfoldexpr) subs
                                    . typeClauseToMplType 
                                    )
                               )

                    (ttypepatts, (patts', pattseqns)) <- lift 
                        $ fmap (second unzip <<< unzip) 
                        $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                    (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                    let subphrase' = (seqdef, ident, patts', expr')
                        ttypeppatts = annotateTypeTags ttypepatts patts
                        ttypepexpr = annotateTypeTag ttypeexpr expr
                        subphraseeqn = TypeEqnsExist (ttypepexpr : ttypepunfoldonpatt : ttypeppatts) $ 
                            -- the type of the overall expression is the type of 
                            -- the clause of the first unfold subphrase
                            [ TypeEqnsEq (typePtoTypeVar ttypep, ttypeclause) 
                            -- The type of the state var is the same
                            -- as the unfoldon pattern 
                            , TypeEqnsEq (typePtoTypeVar ttypepunfoldonpatt, ttypestvar) 
                            -- The type of the type of the unfoldonpatt is the same as
                            -- the as the unfoldonexpr
                            , TypeEqnsEq (typePtoTypeVar ttypepunfoldonpatt, typePtoTypeVar ttypepunfoldonexpr) 
                            -- of course, we need the patternsand the expression sto match
                            , TypeEqnsEq 
                                ( mkTypeSubSeqArr (_Just % _TypeAnnExpr # unfoldexpr) (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                                , mkTypeSubSeqArr (_Just % _TypeAnnExpr # unfoldexpr) (ttypepfroms, ttypeto)  
                                )
                            -- Think of this subphrase like case.. Each of these 
                            -- expressions must be the same..
                            , TypeEqnsEq
                                ( typePtoTypeVar ttypepsubphrase
                                , ttypeclause
                                )
                            ] 
                            <> unfoldonpatteqns
                            <> expreqns
                            <> fold pattseqns
                    return (subphrase', subphraseeqn)

                let phraseeqn = unfoldonexpreqns <> NE.toList phrasesubphraseseqns
                    phrase' = (cxt, unfoldonpatt', phrasesubphrases')

                (phrases', (phraseseqns, ttypepsubphrases)) <- 
                    fmap (second unzip . unzip) 
                    $ for phrases $ \(cxt, unfoldonpatt, phrasesubphrases) -> do
                        (ttypeunfoldonpatt, (unfoldonpatt', unfoldonpatteqns)) <- lift $ withFreshTypeTag $ typeCheckPattern unfoldonpatt
                        -- duplciated code..
                        let ttypepunfoldonpatt = annotateTypeTag ttypeunfoldonpatt unfoldonpatt

                        ttypesubphrase <- freshTypeTag
                        let ttypepsubphrase = annotateTypeTag ttypesubphrase unfoldexpr
                        
                        (phrasesubphrases', phrasesubphraseseqns) <- fmap NE.unzip $ for phrasesubphrases $ \(cxt, ident, patts, (expr, mexpr)) -> do
                            ~(SymEntry lkuptp seqdef) <- lift $ fmap fromJust 
                                $ zoom (envLcl % typeInfoSymTab) 
                                $ lookupSymExprCodataPhrase (ident, expr)

                            tell $ review _ExternalError $ bool
                                []
                                [ _ExpectedUnfoldPhraseToBeEitherButGot # 
                                    ( reachablephrases
                                    , seqdef ^. typePhraseName)
                                    ]
                                $ seqdef ^. typePhraseName `notElem` reachablephrases

                            (ttypepfroms, ttypestvar, ttypeto, ttypeclause) <- state $ runState $ do
                                let (freevars, (froms, st), to) = fromJust $ lkuptp ^? _SymCodataPhrase % originalType 
                                subs <- updateInstantiatedAndGetSubs freevars
                                   
                                return ( fromJust $ traverse (instantiateTypeWithSubs (_Just % _TypeAnnExpr # expr) subs) froms
                                       , fromJust $ instantiateTypeWithSubs (_Just % _TypeAnnExpr # expr) subs st
                                       , fromJust $ instantiateTypeWithSubs (_Just % _TypeAnnExpr # expr) subs to
                                       , seqdef ^. typePhraseExt % Optics.to 
                                            ( fromJust 
                                            . instantiateTypeWithSubs (_Just % _TypeAnnExpr # expr) subs
                                            . typeClauseToMplType 
                                            )
                                       )

                            (ttypepatts, (patts', pattseqns)) <- lift 
                                $ fmap (second unzip <<< unzip) 
                                $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                            (ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpr

                            let subphrase' = (seqdef, ident, patts', expr')
                                ttypeppatts = annotateTypeTags ttypepatts patts
                                ttypepexpr = annotateTypeTag ttypeexpr expr
                                phraseeqn = TypeEqnsExist (ttypepunfoldonpatt : ttypepexpr : ttypeppatts) $ 
                                    -- The type of the state var is the same
                                    -- as the unfoldon pattern 
                                    [ TypeEqnsEq (typePtoTypeVar ttypepunfoldonpatt, ttypestvar) 
                                    -- of course, we need the patternsand the expression sto match
                                    , TypeEqnsEq 
                                        ( mkTypeSubSeqArr (_Just % _TypeAnnExpr # unfoldexpr) (map typePtoTypeVar ttypeppatts, typePtoTypeVar ttypepexpr)  
                                        , mkTypeSubSeqArr (_Just % _TypeAnnExpr # unfoldexpr) (ttypepfroms, ttypeto)  
                                        )
                                    -- Think of this subphrase like case.. Each of these 
                                    -- expressions must be the same..
                                    , TypeEqnsEq
                                        ( typePtoTypeVar ttypepsubphrase
                                        , ttypeclause
                                        )
                                    ] 
                                    <> expreqns
                                    <> fold pattseqns
                                    <> unfoldonpatteqns

                            return (subphrase', phraseeqn)

                        let phrase' = (cxt, unfoldonpatt', phrasesubphrases')
                            eqns = TypeEqnsExist [ttypepunfoldonpatt, ttypepsubphrase] $ unfoldonpatteqns <> NE.toList phrasesubphraseseqns
                        return (phrase', ([eqns], ttypepsubphrase))

                let nunfoldphrases' = phrase' :| phrases'
                    eqns = TypeEqnsExist (ttypepsubphrase : ttypepsubphrases) $
                            phraseeqn <> fold phraseseqns

                return (nunfoldphrases', [eqns] )

        let eqns = TypeEqnsExist (ttypepunfoldonexpr:instt) $ unfoldonexpreqns <> phraseeqns

        return 
            ( _EUnfold # 
                ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap
                , unfoldonexpr'
                , phrases' 
                )
            , [eqns]
            )

    f (ECaseF cxt (caseon, mcaseon) cases) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        (ttypecaseon, (caseon', caseoneqn)) <- withFreshTypeTag mcaseon

        ((ttypepatts, ttypeexprs), (pattsexprs', acceqns)) <- 
            fmap ((NE.unzip *** NE.unzip) <<< NE.unzip)
            $ for cases $ \(patt, (_, mexpreqn)) -> do
                (ttypepatt, (patt', patteqn)) <- withFreshTypeTag $ typeCheckPattern patt
                (ttypeexpr, (expr', expreqn)) <- withFreshTypeTag mexpreqn
                return ((ttypepatt, ttypeexpr), ((patt', expr'), patteqn <> expreqn))

        let ttypep = annotateTypeTag ttype (_ECase # (cxt, caseon, fmap (second fst) cases) :: MplExpr MplRenamed)
            ttypepcaseon = annotateTypeTag ttypecaseon caseon

            ttypeppatts = annotateTypeTags (NE.toList ttypepatts) (NE.toList $ fmap fst cases)
            ttypepexprs = annotateTypeTags (NE.toList ttypeexprs) (NE.toList $ fmap (fst . snd) cases)

            eqns = TypeEqnsExist (ttypepcaseon : ttypeppatts ++ ttypepexprs) $
                -- the case on should be the same as all the patterns
                map (TypeEqnsEq . (typePtoTypeVar ttypepcaseon,) . typePtoTypeVar) ttypeppatts
                -- the resulting type should be the same as all expressions
                -- on the other side of the case
                <> map (TypeEqnsEq . (typePtoTypeVar ttypep,) . typePtoTypeVar) ttypepexprs
                -- accumulate the old equations of course.
                <> caseoneqn
                <> concat acceqns

        return 
            ( _ECase # 
             ( fromJust $ lookupInferredSeqTypeExpr ttype ttypemap
             , caseon'
             , pattsexprs' )
            , [eqns])
        
    f (EObjCallF cxt ident args) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry lkuptp (SymSeqPhraseCall seqdef)) <- fmap fromJust 
            $ zoom (envLcl % typeInfoSymTab) 
            $ lookupSymExpr ident

        (ttypeargs, (args', argseqns)) <- fmap (second unzip <<< unzip) $
            for args $ \(_, mexpreqns) -> withFreshTypeTag mexpreqns

        let expr = _EObjCall # (cxt, ident, map fst args) :: MplExpr MplRenamed

        -- Actually, we can type a destructor and just call it...
        -- tell $ review _ExternalError $ flip (maybe mempty) (seqdef ^? _CodataDefn) $ \defn ->
            -- [_IllegalExprDataCallGotCodataInstead # (expr, defn) ]

        arrenv <- freshInstantiateArrEnv
        let ttypep = annotateTypeTag ttype expr
            ttypepargs = annotateTypeTags ttypeargs $ map fst args

            ann = _Just % _TypeAnnExpr # expr
            ~(ttypesphrase, lkuptp') = (`runInstantiateArrType`arrenv)
                $ fromJust 
                $ instantiateArrType ann
                    <$> lkuptp ^? _SymDataPhrase % noStateVarsType
                <|> instantiateArrType ann
                    <$> lkuptp ^? _SymCodataPhrase % noStateVarsType

            eqns = TypeEqnsExist (ttypesphrase ++ ttypepargs) $
                [ TypeEqnsEq 
                    ( mkTypeSubSeqArr (_Just % _TypeAnnExpr # expr) (map typePtoTypeVar ttypepargs, typePtoTypeVar ttypep)  
                    , lkuptp') 
                ] <> concat argseqns

        return 
            ( _EObjCall # 
              ( (seqdef, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
              , ident
              , args' ) 
            , [eqns] )

    -- lots of duplicated code..
    f (ECallF cxt ident args ) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry lkuptp (SymSeqCall seqdef)) <- fmap fromJust 
            $ zoom (envLcl % typeInfoSymTab) 
            $ lookupSymExpr ident

        (ttypeargs, (args', argseqns)) <- fmap (second unzip <<< unzip) 
            $ for args $ \(_, mexpreqns) -> withFreshTypeTag mexpreqns

        arrenv <- freshInstantiateArrEnv
        let expr = _ECall # (cxt, ident, map fst args) :: MplExpr MplRenamed

            ttypep = annotateTypeTag ttype expr
            -- ttypepargs = annotateTypeTags ttypeargs $ map fst args
            -- changed annotaiotn information
            ttypepargs = annotateTypeTags ttypeargs $ repeat expr

            ann = _Just % _TypeAnnExpr # expr
            ~(ttypesphrase, lkuptp') = (`runInstantiateArrType`arrenv)
                $ fromJust 
                $ instantiateArrType ann <$> lkuptp ^? _SymSeqCallType

            eqns = TypeEqnsExist (ttypesphrase ++ ttypepargs) $
                [ TypeEqnsEq 
                    ( mkTypeSubSeqArr (_Just % _TypeAnnExpr # expr) (map typePtoTypeVar ttypepargs, typePtoTypeVar ttypep)  
                    , lkuptp') 
                ] 
                <> concat argseqns

            {-
            res = case seqdef of 
                ExprCallPattern patt -> EVar (patt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) ident
                ExprCallFun fun -> ECall (fun, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap) ident args'
            -}
            res = case seqdef of 
                ExprCallPattern patt -> case args' of
                    [] -> EVar (fromJust $ lookupInferredSeqTypeExpr ttype ttypemap ) ident
                    _ -> ECall (fromJust $ lookupInferredSeqTypeExpr ttype ttypemap) ident args'
                ExprCallFun fun -> ECall (fromJust $ lookupInferredSeqTypeExpr ttype ttypemap) ident args'

        return 
            ( res
            , [eqns] )



    f (ERecordF cxt phrases) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        
        let expr = _ERecord # (cxt, phrases & mapped % _3 % _2 %~ fst) :: MplExpr MplRenamed
            ttypep = annotateTypeTag ttype expr

        {-
         -- STILL NEED TO CHECK IF THE CLAUSE IS EXHAUSTIVE
         -- Not really a type checking thing to do? Think of this as
         -- a non exhaustive patterns error which happens in compilation
         -- of pattern matching.....

        tell $ review _ExternalError $ bool 
            [] [_RecordConstructionErrorGotPhrasesButExpected # 
                ( recordphraseidents
                , clausephrases ) ]
            $ length recordphraseidents /= length clausephrases || 
                any id (zipWith (\a b -> a ^. uniqueTag /= b ^. typePhraseName % uniqueTag ) 
                    (NE.toList recordphraseidents) clausephrases)
        -}

        arrenv <- freshInstantiateArrEnv
        ~(((ttypeppatts, ttypepexpr), (phrases', phraseseqns)), ttypepinst) <- fmap 
            (( unzip *** unzip <<< unzip <<< NE.toList) 
                *** (toListOf (instantiateArrEnvInstantiated % folded)))
            $ flip runStateT arrenv
            $ for phrases $ \(_, ident, (patts, (expr, mexpreqn))) -> do
                ~(SymEntry lkuptp seqdef) <- fmap fromJust $ lift $ zoom (envLcl % typeInfoSymTab) $
                        lookupSymExprCodataPhrase (ident,expr)

                ~(ttypepatts, (patts', pattseqns)) <- lift 
                    $ fmap (second unzip <<< unzip) 
                    $ traverse (withFreshTypeTag . typeCheckPattern) patts 

                ~(ttypeexpr, (expr', expreqns)) <- lift $ withFreshTypeTag mexpreqn 

                -- Note: we need the strange ``state $ runState" call here to get the correct laziness
                -- To be honest, this is kinda messed up, and I don't think that in the future we should
                -- ever use MonadFix to handle a symbol table.. it's just too weird to get the laziness
                -- right so we don't accidentally hit a bottom value.. It seems like clearly,
                -- @state $ runState $ e@ should just be the same as @e@ but of course as seen here)
                -- that's not the case with large recursive tying the knot.
                (ttypepphrase, ttypeclause) <- state $ runState $ do
                    ttypepphrase <- instantiateArrType 
                        {- TODO, probably should include some sort of annotation
                        - information here... e.g. (_Just % TypeAnnPatt seqdef) -}
                        (_Just % _TypeAnnExpr # expr)
                        $ fromJust $ lkuptp ^? _SymCodataPhrase 
                                % noStateVarsType 
                                % to (over _2 fst)
                    subs <- getInstantiatedSubs
                    return 
                        ( ttypepphrase
                        , seqdef ^. typePhraseExt % to 
                            ( fromJust 
                            . instantiateTypeWithSubs (_Just % _TypeAnnExpr # expr) subs
                            . typeClauseToMplType 
                            )
                        )

                let ttypeppatts = annotateTypeTags ttypepatts patts 
                    ttypepexpr = annotateTypeTag ttypeexpr expr
                    phraseeqns = 
                        -- the type of the patts and expression
                        -- must match the type phrase given in 
                        -- the clause
                        [ TypeEqnsEq 
                            ( mkTypeSubSeqArr (_Just % _TypeAnnExpr # expr) (ttypeppatts, ttypepexpr) 
                            , ttypepphrase)
                        -- the type of this whole expression is the
                        -- same as the end result type
                        , TypeEqnsEq
                            ( ttypeclause
                            , typePtoTypeVar ttypep )
                        ]

                return 
                    ( (ttypeppatts, ttypepexpr)
                    , ((seqdef, ident, (patts', expr')), phraseeqns ++ concat pattseqns ++ expreqns)
                    )

        let eqns = TypeEqnsExist (concat ttypeppatts <> ttypepexpr <> ttypepinst ) $
                concat phraseseqns

        return $ 
            ( _ERecord # 
              ( ( cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
              , NE.fromList phrases') 
            , [eqns] 
            )


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
typeCheckProcessBody procbdy@((patts, ins, outs), cmds) = do
    ttype <- guse (envLcl % typeInfoEnvTypeTag)
    ttypemap <- guse (envLcl % typeInfoEnvMap)

    (ttypepatts, (patts', pattacceqns)) <- second NE.unzip . NE.unzip <$> 
        traverse (withFreshTypeTag . typeCheckPattern) patts 

    ttypeins <- traverse freshChTypeTag ins
    ttypeouts <- traverse freshChTypeTag outs

    -- ttypepattsstable <- traverse (const freshTypeTag) ttypepatts

    (cmds', acccmds) <- typeCheckCmds cmds

    let ttypep = annotateTypeTag ttype procbdy
        ttypeppatts = annotateTypeTags ttypepatts patts
        ttypepins = annotateTypeTags ttypeins ins
        ttypepouts = annotateTypeTags ttypeouts outs

        eqn = TypeEqnsExist (ttypeppatts ++ ttypepins ++ ttypepouts) $
                -- phrase equation
                [ TypeEqnsEq    
                    ( typePtoTypeVar ttypep
                    , _TypeConcArrF # 
                        ( _Just % _TypeAnnProcPhrase # procbdy
                        , map typePtoTypeVar ttypeppatts
                        , map typePtoTypeVar ttypepins
                        , map typePtoTypeVar ttypepouts ) 
                    ) 
                ]
                -- accumulate the equations
                <> acccmds
                <> concat pattacceqns
        ins' = zipWith 
                (\stref -> 
                    review _ChIdentT 
                    . (,fromJust $ lookupInferredTypeCh stref ttypemap) 
                    )
                ttypeins ins 
        outs' = zipWith 
                (\stref -> 
                    review _ChIdentT 
                    . (,fromJust $ lookupInferredTypeCh stref ttypemap) 
                    )
                ttypeouts
                outs 

    return (((patts', ins', outs'), cmds'), [eqn])

typeCheckCmds ::
    TypeCheck
        (NonEmpty (MplCmd MplRenamed)) 
        (NonEmpty (MplCmd MplTypeChecked), [TypeEqns MplTypeSub])
typeCheckCmds (cmd :| []) = do
    tell $ review _ExternalError $ case cmd of
        (CClose cxt _) -> [_IllegalLastCommand # cxt]
        (CGet cxt _ _) -> [_IllegalLastCommand # cxt]
        (CPut cxt _ _) -> [_IllegalLastCommand # cxt]
        (CHPut cxt _ _) -> [_IllegalLastCommand # cxt]
        (CSplit cxt _ _) -> [_IllegalLastCommand # cxt]
        _ -> mempty

    (cmd', eqns) <- typeCheckCmd cmd

    openchs <- guses (envLcl % typeInfoSymTab % symTabCh) (toListOf (folded % symEntryInfo))
    tell 
        $ review _ExternalError 
        $ bool [_AtLastCmdThereAreUnclosedChannels # (cmd, openchs)] [] 
        $ null openchs

    return (cmd' :| [], eqns)
typeCheckCmds (cmd :| rst) = do
    tell $ review _ExternalError $ case cmd of
        CFork cxt _ _ -> [_IllegalNonLastCommand # cxt]
        CId cxt _ -> [_IllegalNonLastCommand # cxt]
        CIdNeg cxt _ -> [_IllegalNonLastCommand # cxt]
        -- run commands are not techincally a keyword..
        CRun _ cxt _ _ _ -> [review _IllegalNonLastCommand $ cxt ^. identPNameOcc % to KeyWordNameOcc]
        CHCase cxt _ _ -> [_IllegalNonLastCommand # cxt]
        CHalt cxt _ -> [_IllegalNonLastCommand # cxt]
        CRace cxt _ -> [_IllegalNonLastCommand # cxt]
        _ -> []
    (cmd', eqn) <- typeCheckCmd cmd
    (rst', eqns) <- first NE.toList <$> typeCheckCmds (NE.fromList rst)
    return (cmd' :| rst', eqn <> eqns)

typeCheckCmd ::
    TypeCheck
        (MplCmd MplRenamed)
        (MplCmd MplTypeChecked, [TypeEqns MplTypeSub])
typeCheckCmd cmd = let cmdann = _Just % _TypeAnnCmd # cmd in case cmd of 
    CRun cxt ident seqs ins outs -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry tp (SymRunInfo procc)) <- zoom (envLcl % typeInfoSymTab) $ lookupSymConc ident

        arrenv <- freshInstantiateArrEnv
        let (ttypepargs, ttypeproc) = (`runInstantiateArrType` arrenv) 
                $ fromJust 
                $ tp ^? _SymConcCallType % to (instantiateArrType (_Just % _TypeAnnCmd # cmd))

        ttypesins <- zoom (envLcl % typeInfoSymTab) $ traverse lookupSymCh ins 

        ttypesouts <- zoom (envLcl % typeInfoSymTab) $ for outs $ lookupSymCh 
        ttypesoutsstables <- traverse (const freshTypeTag) outs

        tell $ review _ExternalError $ 
            foldMapOf (folded % symEntryInfo) expectedInputPolarity ttypesins
            <> foldMapOf (folded % symEntryInfo) expectedOutputPolarity ttypesouts


        (ttypeseqs, (seqs', seqseqns)) <- fmap (second unzip . unzip) 
            $ traverse (withFreshTypeTag . typeCheckExpr) seqs

        -- remove the entries from the symbol table
        zoom (envLcl % typeInfoSymTab) $ do
            for_ ins $ \ch -> symTabCh % at (ch ^. uniqueTag) .= Nothing
            for_ outs $ \ch -> symTabCh % at (ch ^. uniqueTag) .= Nothing

        {-
        let ttypespins = annotateTypeTags (map (view symEntryType) ttypesins) ins
            ttypespouts = annotateTypeTags (map (view symEntryType) ttypesouts) outs
            ttypespseqs = annotateTypeTags ttypeseqs seqs
            -- changed annotaiotn information
        -}
        let ttypespins = annotateTypeTags (map (view symEntryType) ttypesins) $ repeat cmd
            ttypespouts = annotateTypeTags (map (view symEntryType) ttypesouts) $ repeat cmd
            ttypespseqs = annotateTypeTags ttypeseqs $ repeat cmd

            eqns = TypeEqnsExist (ttypepargs <> ttypespseqs <> ttypespins <> ttypespouts) $
                    -- match the given types with the actual type of the process
                    [ TypeEqnsEq 
                        ( ttypeproc
                        , _TypeConcArrF # 
                            ( _Just % _TypeAnnCmd # cmd
                            , map typePtoTypeVar ttypespseqs
                            , map typePtoTypeVar ttypespins
                            , map typePtoTypeVar ttypespouts
                            ) 
                        ) 
                    ] 
                    -- match the new channel types with the old channel types
                    {-
                    <> genTypeEqEqns 
                        (map typePtoTypeVar (annotateTypeTags ttypesinsstables ins))
                        (map typePtoTypeVar ttypespins) 
                    <> genTypeEqEqns 
                        (map typePtoTypeVar (annotateTypeTags ttypesinsstables outs))
                        (map typePtoTypeVar ttypespouts) 
                    -}
                    -- accumlate old equations
                    <> concat seqseqns

            ins' = zipWith (\chr tag -> _ChIdentT # (chr, fromJust $ lookupInferredSeqTypeExpr tag ttypemap)) 
                    ins (map (view symEntryType) ttypesins)

            outs' = zipWith (\chr tag -> _ChIdentT # (chr, fromJust $ lookupInferredSeqTypeExpr tag ttypemap))
                    outs (map (view symEntryType) ttypesouts) 

        return (_CRun # (procc, ident, seqs', ins', outs'), [eqns] )
    
    CClose cxt ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch

        envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) .= Nothing

        let ch' = _ChIdentT # (ch, fromJust $ lookupInferredSeqTypeExpr ttypech ttypemap )

            ttypepch = annotateTypeTag ttypech cmd
            eqns = 
                [ -- the type is top bot..
                TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , _TypeTopBotF # _Just % _TypeAnnCmd # cmd )
                ]

        return (_CClose # (cxt, ch'), eqns)
    
    -- duplicated code from the CClose case...
    CHalt cxt ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch

        envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) .= Nothing

        let ch' = _ChIdentT # (ch, fromJust $ lookupInferredSeqTypeExpr ttypech ttypemap )

            ttypepch = annotateTypeTag ttypech ch
            eqns = 
                [ TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , _TypeTopBotF # _Just % _TypeAnnCmd # cmd )
                -- the type is top bot..
                ]

        return (_CHalt # (cxt, ch'), eqns)

    CGet cxt patt ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch

        ttypech' <- freshChTypeTag ch

        (ttypepatt, (patt', patteqns)) <- withFreshTypeTag $ typeCheckPattern patt

        let ch' = _ChIdentT # (ch, fromJust $ lookupInferredSeqTypeExpr ttypech ttypemap)
            ttypepch = annotateTypeTag ttypech ch
            ttypepch' = annotateTypeTag ttypech' ch

            ttypeppatt = annotateTypeTag ttypepatt patt

            eqn = TypeEqnsExist [ttypepch', ttypeppatt] $
                [ TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , inputOutput (ch ^. polarity) 
                        (_TypePutF # 
                            ( _Just % _TypeAnnCmd # cmd
                            , typePtoTypeVar ttypeppatt
                            , typePtoTypeVar ttypepch'))
                        (_TypeGetF # 
                            ( _Just % _TypeAnnCmd # cmd
                            , typePtoTypeVar ttypeppatt
                            , typePtoTypeVar ttypepch'))
                    )
                ]
                <> patteqns

        return (_CGet # (cxt, patt', ch'), [eqn])

    -- duplciated (except changing the expression)
    CPut cxt expr ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch

        ttypech' <- freshChTypeTag ch

        (ttypeexpr, (expr', expreqns)) <- withFreshTypeTag $ typeCheckExpr expr


        let ch' = _ChIdentT # (ch, fromJust $ lookupInferredSeqTypeExpr ttypech ttypemap)
            ttypepch = annotateTypeTag ttypech ch
            ttypepch' = annotateTypeTag ttypech' ch

            ttypepexpr = annotateTypeTag ttypeexpr expr

            eqn = TypeEqnsExist [ttypepch', ttypepexpr] $
                [ TypeEqnsEq
                    ( typePtoTypeVar ttypepch
                    , inputOutput (ch ^. polarity) 
                        (_TypeGetF # 
                            ( _Just % _TypeAnnCmd # cmd
                            , typePtoTypeVar ttypepexpr
                            , typePtoTypeVar ttypepch'))
                        (_TypePutF # 
                            ( _Just % _TypeAnnCmd # cmd
                            , typePtoTypeVar ttypepexpr
                            , typePtoTypeVar ttypepch'))
                    )

                ]
                <> expreqns

        return (_CPut # (cxt, expr', ch'), [eqn])

    CHCase cxt ch cases -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch

        let ttypepch = annotateTypeTag ttypech ch

        arrenv <- freshInstantiateArrEnv
        ((cases', caseseqns), ttypeinst) <- fmap 
            (NE.unzip *** toListOf (instantiateArrEnvInstantiated % folded) )
            $ (`runStateT` arrenv) 
            $ for cases $ \(cxt, ident, cmds) -> do
                ~(SymEntry clauselkuptp ~(SymConcPhraseCall def)) <- lift 
                    $ zoom (envLcl % typeInfoSymTab) $ do
                        res <- guse $ symTabConc % at (ident ^. uniqueTag)
                        tell $ review _InternalError $ maybe [_CannotCallTerm # ident] mempty res
                        return $ fromJust res

                tell $ review _ExternalError 
                    $ inputOutput (ch ^. polarity)
                        ( maybeToList 
                            $ def ^? _CoprotocolDefn 
                                % to 
                                    ( review 
                                    _HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol 
                                    . (ch,)
                                    )
                            )
                        ( maybeToList 
                            $ def ^? _ProtocolDefn 
                                % to 
                                    ( review 
                                    _HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol 
                                    . (ch,)
                                    )
                        )

                let (ttypeargs, unwrappedtp) = fromJust $ clauselkuptp ^? _SymConcPhrase 

                subs <- state $ runState $ updateInstantiatedAndGetSubs ttypeargs

                (ttypech', (cmds', cmdseqns)) <- lift $ localEnvSt id $ do
                    ttypech' <- freshChTypeTag ch
                    (cmds', cmdseqns) <- typeCheckCmds cmds
                    return (ttypech', (cmds', cmdseqns))

                let ttypepunwrapped = fromJust $ instantiateTypeWithSubs cmdann subs $ unwrappedtp
                    ttypepclause = fromJust $ case def of
                        ProtocolDefn phrase -> 
                            phrase ^. typePhraseExt
                                % to ( instantiateTypeWithSubs cmdann subs 
                                     . typeClauseToMplType)
                        CoprotocolDefn phrase -> 
                            phrase ^. typePhraseExt
                                % to ( instantiateTypeWithSubs cmdann subs 
                                     . typeClauseToMplType)
                    ttypepch' = annotateTypeTag ttypech' ch
                    eqns = TypeEqnsExist [ttypepch'] $
                        [ TypeEqnsEq 
                            ( typePtoTypeVar ttypepch'
                            , ttypepunwrapped )
                        , TypeEqnsEq 
                            ( typePtoTypeVar ttypepch
                            , ttypepclause )
                        ]
                        <> cmdseqns

                return ((def, ident, cmds') , [eqns])

        let eqn = TypeEqnsExist ttypeinst $ fold caseseqns
            ch' = _ChIdentT # (ch, fromJust $ lookupInferredSeqTypeExpr ttypech ttypemap)

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CHCase # (cxt, ch', cases'), [eqn])

    CHPut cxt ident ch -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        let ttypepch = annotateTypeTag ttypech ch

        ~(SymEntry clauselkuptp (SymConcPhraseCall def)) <- zoom (envLcl % typeInfoSymTab) $ do
            res <- guse $ symTabConc % at (ident ^. uniqueTag)

            tell $ review _InternalError $ 
                maybe [_CannotCallTerm # ident] 
                mempty 
                res
            return $ fromJust res

        tell $ review _ExternalError 
            $ inputOutput (ch ^. polarity)
                ( maybeToList 
                    $ def ^? _ProtocolDefn 
                        % to ( review _HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol 
                             . (cxt, ch,))
                    )
                ( maybeToList 
                  $ def ^? _CoprotocolDefn 
                        % to ( review _HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol 
                             . (cxt, ch,)))

        ttypech' <- freshChTypeTag ch
        let ttypepch' = annotateTypeTag ttypech' ch

        arrenv <- freshInstantiateArrEnv
        let (ttypeargs, unwrappedtp) = fromJust $ clauselkuptp ^? _SymConcPhrase 
            (ttypepargs, (ttypepclause, ttypepunwrapped)) = (`runInstantiateArrType` arrenv) $ do
                subs <- updateInstantiatedAndGetSubs ttypeargs
                return 
                    ( fromJust $ case def of
                        ProtocolDefn phrase -> 
                            phrase ^. typePhraseExt % to ( instantiateTypeWithSubs cmdann subs . typeClauseToMplType )
                        CoprotocolDefn phrase -> 
                            phrase ^. typePhraseExt % to ( instantiateTypeWithSubs cmdann subs . typeClauseToMplType )
                    , fromJust $ instantiateTypeWithSubs cmdann subs $ unwrappedtp)
            eqn = TypeEqnsExist ([ttypepch'] <> ttypepargs) $
                    [ TypeEqnsEq
                        ( typePtoTypeVar ttypepch
                        , ttypepclause )
                    , TypeEqnsEq
                        ( typePtoTypeVar ttypepch'
                        , ttypepunwrapped
                        )
                    ]
            ch' = _ChIdentT # (ch, fromJust $ lookupInferredSeqTypeExpr ttypech ttypemap)
        
        return (_CHPut # ((cxt, def), ident, ch'), [eqn])

    CSplit cxt ch (ch0, ch1) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        let ttypepch = annotateTypeTag ttypech ch
        ttypech0 <- freshChTypeTag ch0
        ttypech1 <- freshChTypeTag ch1

        envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) .= Nothing

        let ch' = _ChIdentT # (ch, fromJust $ lookupInferredSeqTypeExpr ttypech ttypemap )
            ch0' = _ChIdentT # (ch0, fromJust $ lookupInferredSeqTypeExpr ttypech0 ttypemap )
            ch1' = _ChIdentT # (ch1, fromJust $ lookupInferredSeqTypeExpr ttypech1 ttypemap )

            ttypepch0 = annotateTypeTag ttypech0 ch0
            ttypepch1 = annotateTypeTag ttypech1 ch1

            eqn = TypeEqnsExist [ttypepch0, ttypepch1] $ 
                    [ TypeEqnsEq 
                        ( typePtoTypeVar ttypepch
                        , inputOutput (ch ^. polarity)
                            ( _TypeTensorF # 
                                ( _Just % _TypeAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                            (_TypeParF # 
                                ( _Just % _TypeAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                        )
                    ]

        return (_CSplit # (cxt, ch', (ch0', ch1')), [eqn])

    -- duplciated code from the split case...
    CFork cxt ch ((ch0, cxt0, cmds0), (ch1, cxt1, cmds1)) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        let ttypepch = annotateTypeTag ttypech ch

        scoped <- guses (envLcl % typeInfoSymTab % symTabCh) (toListOf (itraversed % symEntryInfo))
        let sharedchs = cxt0 `intersect` cxt1
        tell $ review _ExternalError $ bool 
            [_ForkExpectedDisjointChannelsButHasSharedChannels # (cxt, sharedchs)] 
            [] $ null sharedchs
        let nonexhaustive = scoped \\ (ch : cxt0 <> cxt1)
        tell $ review _ExternalError $ bool 
            [_ForkHasChannelsInScopeButContextsAreNonExhaustiveWith #
                ( cxt
                , scoped
                , (cxt0, cxt1)
                , nonexhaustive)
            ] 
            [] $ null nonexhaustive

        ttypecxt0 <- zoom (envLcl % typeInfoSymTab) $ traverse lookupSymCh cxt0
        ttypecxt1 <- zoom (envLcl % typeInfoSymTab) $ traverse lookupSymCh cxt1
        
        -- phrase 1...
        ttypech0 <- freshChTypeTag ch0
        let cxt0tags = ch0 ^. uniqueTag : map (view uniqueTag) cxt0
        (cmds0', cmds0eqns) <- localEnvSt 
            ( over 
                (envLcl % typeInfoSymTab % symTabCh) 
                (Map.filterWithKey (\k _ -> k `elem` cxt0tags))
            ) $ typeCheckCmds cmds0 

        -- phrase 2...
        ttypech1 <- freshChTypeTag ch1
        let cxt1tags = ch1 ^. uniqueTag : map (view uniqueTag) cxt1
        (cmds1', cmds1eqns) <- localEnvSt 
            ( over 
                (envLcl % typeInfoSymTab % symTabCh) 
                (Map.filterWithKey (\k _ -> k `elem` cxt1tags))
            ) $ typeCheckCmds cmds1 


        let ch' = _ChIdentT # (ch, fromJust $ lookupInferredTypeCh ttypech ttypemap )
            ch0' = _ChIdentT # (ch0, fromJust $ lookupInferredTypeCh ttypech0 ttypemap )
            ch1' = _ChIdentT # (ch1, fromJust $ lookupInferredTypeCh ttypech1 ttypemap )

            ttypepch0 = annotateTypeTag ttypech0 ch0
            ttypepch1 = annotateTypeTag ttypech1 ch1

            cxt0' = zipWith 
                    (\stable ch -> 
                        _ChIdentT # (ch, fromJust $ lookupInferredTypeCh stable ttypemap )) 
                    (map (view symEntryType) ttypecxt0) 
                    cxt0
            cxt1' = zipWith 
                    (\stable ch -> 
                        _ChIdentT # (ch, fromJust $ lookupInferredTypeCh stable ttypemap ))
                        (map (view symEntryType) ttypecxt1) 
                        cxt1

            ttypepcxt0 = annotateTypeTags (map (view symEntryType) ttypecxt0) cxt0
            ttypepcxt1 = annotateTypeTags (map (view symEntryType) ttypecxt1) cxt1


            eqn = TypeEqnsExist [ttypepch0, ttypepch1] $ 
                    [ TypeEqnsEq 
                        ( typePtoTypeVar ttypepch
                        , inputOutput (ch ^. polarity)
                            (_TypeParF # 
                                ( _Just % _TypeAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                            (_TypeTensorF # 
                                ( _Just % _TypeAnnCmd # cmd
                                , typePtoTypeVar ttypepch0
                                , typePtoTypeVar ttypepch1
                                ) 
                            )
                        )
                    ]
                    <> cmds0eqns 
                    <> cmds1eqns 

        envLcl % typeInfoSymTab % symTabCh .=  mempty

        return 
            ( _CFork # 
                ( cxt
                , ch'
                , ( (ch0', cxt0', cmds0')
                  , (ch1', cxt1', cmds1')
                  ) 
                )
            , [eqn]) 

    CId cxt (ch0, ch1) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech0 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch0
        let ttypepch0 = annotateTypeTag ttypech0 ch0

        ~(SymEntry ttypech1 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch1
        let ttypepch1 = annotateTypeTag ttypech1 ch1

        tell $ review _ExternalError $ bool []
            [_IllegalIdGotChannelsOfTheSamePolarityButIdNeedsDifferentPolarity # (cxt, ch0, ch1) ] 
            $ ch0 ^. polarity == ch1 ^. polarity

        let ch0' = _ChIdentT # (ch0, fromJust $ lookupInferredTypeCh ttypech0 ttypemap )
            ch1' = _ChIdentT # (ch1, fromJust $ lookupInferredTypeCh ttypech1 ttypemap)

            eqns = 
                [ TypeEqnsEq (typePtoTypeVar ttypepch0, typePtoTypeVar ttypepch1)
                ]

        envLcl % typeInfoSymTab % symTabCh % at (ch0 ^. uniqueTag) .= Nothing
        envLcl % typeInfoSymTab % symTabCh % at (ch1 ^. uniqueTag) .= Nothing

        return ( _CId # (cxt, (ch0', ch1')), eqns )
    -- duplciated code..
    CIdNeg cxt (ch0, ch1) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)
        ~(SymEntry ttypech0 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch0
        let ttypepch0 = annotateTypeTag ttypech0 ch0

        ~(SymEntry ttypech1 info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch1
        ttypech1stable <- freshTypeTag
        let ttypepch1 = annotateTypeTag ttypech1 ch1

        tell $ review _ExternalError $ bool []
            [ _IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity # (cxt, ch0, ch1) ] 
            $ ch0 ^. polarity /= ch1 ^. polarity

        let ch0' = _ChIdentT # (ch0, fromJust $ lookupInferredTypeCh ttypech0 ttypemap )
            ch1' = _ChIdentT # (ch1, fromJust $ lookupInferredTypeCh ttypech1 ttypemap )

            eqns = 
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypepch0
                    , _TypeNegF # (_Just % _TypeAnnCmd # cmd, typePtoTypeVar ttypepch1))
                ] 

        envLcl % typeInfoSymTab % symTabCh % at (ch0 ^. uniqueTag) .= Nothing
        envLcl % typeInfoSymTab % symTabCh % at (ch1 ^. uniqueTag) .= Nothing

        return ( _CIdNeg # (cxt, (ch0', ch1')), eqns )
    CRace cxt ((ch, cmds) :| races) -> do
        {-
         - the unneccessary error checking?
         - neg (empty or empty) = not empty and not empty
        let inpoutchs = NE.partition (has (_1 % polarity % _Input)) races
        tell $ review _ExternalError $ bool []
            [ _IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity # (cxt, ch0, ch1) ] 
            $ inpoutchs
            ch0 ^. polarity /= ch1 ^. polarity
        - (ChIdentR, NonEmpty cmd)
        -}

        ~(SymEntry ttypech info) <- zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ttypepch = annotateTypeTag ttypech ch

        (ttypepdummies, geteqn) <- instantiateRaceEqn ch ttypepch

        (cmds', cmdseqns) <- localEnvSt id $ typeCheckCmds cmds

        let ch' = _ChIdentT # (ch, fromJust $ lookupInferredTypeCh ttypech ttypemap )
            raceeqn = TypeEqnsExist ttypepdummies $ 
                [ geteqn  ]
                <> cmdseqns

        (races', raceseqns) <- fmap unzip 
            $ flip evalStateT ttypepch
            $ for races $ \(ch, cmds) -> do
                -- duplciated code..
                ~(SymEntry ttypech info) <- lift $ zoom (envLcl % typeInfoSymTab) $ lookupSymCh ch
        
                let ttypepch = annotateTypeTag ttypech ch
        
                (ttypepdummies, geteqn) <- lift $ instantiateRaceEqn ch ttypepch
                (cmds', cmdseqns) <- lift $ localEnvSt id $ typeCheckCmds cmds

                oldch <- equality <<.= ttypepch

                let ch' = _ChIdentT # (ch, fromJust $ lookupInferredTypeCh ttypech ttypemap )
                    raceeqn = TypeEqnsExist ttypepdummies $ 
                        [ geteqn 
                        , TypeEqnsEq (typePtoTypeVar oldch, typePtoTypeVar ttypepch) ]
                        <> cmdseqns

                return ((ch', cmds'), raceeqn)
        let eqns = [ raceeqn ] <> raceseqns

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CRace # (cxt, (ch',cmds') :| races'), eqns)
      where
        -- instantiates the race equations because races must have the type 
        -- be of get or put (depending on polarity)...
        instantiateRaceEqn ch ttypepch = do
            ttypechgetseq <- freshTypeTag
            ttypechgetconc <- freshTypeTag
            let ttypepchgetseq = annotateTypeTag ttypechgetseq cmd
                ttypepchgetconc = annotateTypeTag ttypechgetconc cmd
                eqn = TypeEqnsEq 
                        ( typePtoTypeVar ttypepch 
                        , inputOutput (ch ^. polarity)
                            (_TypePutF # 
                                ( _Just % _TypeAnnCmd # cmd
                                , typePtoTypeVar ttypepchgetseq
                                , typePtoTypeVar ttypepchgetconc
                                ))
                            (_TypeGetF # 
                                ( _Just % _TypeAnnCmd # cmd
                                , typePtoTypeVar ttypepchgetseq
                                , typePtoTypeVar ttypepchgetconc
                                ))
                        )
            return ([ttypepchgetseq, ttypepchgetconc], eqn)

    CPlugs (cxt, plugs) (phr1, phr2, phrs) -> do
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        -- see the cut condition things...
        let allphrases = phr1:phr2:phrs
            allphrasesgraph = map (view _2) allphrases
        tell $ review _ExternalError $ cutConditions plugs allphrasesgraph
        tell $ review _ExternalError $ cutCycles $ NE.fromList allphrasesgraph

        -- Get the map for the stable equations
        {-
        let allchs = nubBy ((==) `on` view uniqueTag) 
                $ foldMapOf (folded % _2 % each % folded) (pure . view uniqueTag) 
                $ phr1:phr2:phrs
            allchsids = map (view uniqueTag) allchs
        ttypechsstable <- traverse (const freshTypeTag) allchs 
        let stablerefs = Map.fromList $ zip allchs ttypechsstable
        -}

        {-  Honestly, this is really confusing... I'm pretty sure 
         'ttypesplugged' is a map from the channels and the tags of types which it could be
         (should be at most 2 from the plug condition checks) and after when all the commands
         types are put together, it will set each of the types in the non empty list equal to 
         each other.. some legacy stuff from misunderstanding how this works initially....
        -}
        ~((phr1':phr2':phrs', acceqns), ttypesplugged) <- flip runStateT Map.empty 
            $ fmap unzip 
            $ for allphrases 
            $ \(cxt, (ins, outs), cmds) -> do
                let insr = map (view identR) ins
                    outsr = map (view identR) outs
                    pluggedins = map (review _ChIdentR . (,Input)) 
                        $ filter (\identr -> identr `elem` insr ) plugs 
                    pluggedouts = map (review _ChIdentR . (,Output)) 
                        $ filter (\identr -> identr `elem` outsr ) plugs 

                    phrsechsids = map (view uniqueTag) $ ins <> outs

                for_ (pluggedins <> pluggedouts) $ \ch -> do
                    ch' <- lift $ freshChTypeTag ch
                    at (ch ^. uniqueTag) %= Just . maybe (ch' :| []) (NE.cons ch')

                lift $ tell $ review _ExternalError $ foldMap expectedInputPolarity ins
                    <> foldMap expectedOutputPolarity outs

                (cmds', cmdseqns) <- lift 
                    $ localEnvSt (over (envLcl % typeInfoSymTab % symTabCh) 
                        (Map.filterWithKey (\k _ -> k `elem` phrsechsids)))
                    $ typeCheckCmds cmds

                -- awkwardness with the legacy way of doing this to recover the original type tag
                -- to look up the information.
                symtabch <- lift $ guse $ envLcl % typeInfoSymTab % symTabCh

                let eqns = cmdseqns
                    ins' = map (\ch -> _ChIdentT # 
                            ( ch
                            , fromJust $ 
                                lookupInferredTypeCh (fromJust $ symtabch ^? at (ch ^. uniqueTag) % _Just % _SymEntry % _1) ttypemap 
                            )) ins
                    outs' = map (\ch -> _ChIdentT # 
                            ( ch
                            , fromJust $ lookupInferredTypeCh (fromJust $ symtabch ^? at (ch ^. uniqueTag) % _Just % _SymEntry % _1) ttypemap
                            )) outs

                return ((cxt, (ins', outs'), cmds'), eqns)

        -- same awkardness as before.
        symtabch <- guse $ envLcl % typeInfoSymTab % symTabCh 

        let (exists, pluggedeqns) = second fold 
                $ unzip 
                $ flip concatMap plugs 
                $ \ch -> 
                    let f (h :| hs) = (:) (annotateTypeTag h cmd, []) 
                            $ map (\(prev, curr) -> 
                                    ( annotateTypeTag curr cmd
                                    , [ TypeEqnsEq 
                                        ( typePtoTypeVar $ annotateTypeTag prev cmd
                                        , typePtoTypeVar $ annotateTypeTag curr cmd) ]
                                    ) 
                                ) 
                            $ zip (h:hs) hs
                    in f . fromJust $ ttypesplugged ^. at (ch ^. uniqueTag)

            eqn = TypeEqnsExist exists $ pluggedeqns <> fold acceqns
            plugs' = map (\ch -> 
                    ( ch
                    , fromJust $ lookupInferredTypeCh (fromJust $ symtabch ^? at (ch ^. uniqueTag) % _Just % _SymEntry % _1) ttypemap
                    )
                ) plugs

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CPlugs # ((cxt, plugs'), (phr1', phr2',phrs')), [eqn])

    CCase cxt expr cases -> do
        (ttypeexpr, (expr', expreqn)) <- withFreshTypeTag $ typeCheckExpr expr
        ttypeexprstable <- freshTypeTag
        (ttypespatts, (cases', caseseqns)) <- fmap (second NE.unzip . NE.unzip)
            $ for cases $ \(patt, cmds) -> localEnvSt id $ do
                (ttypepatt, (patt', patteqn)) <- withFreshTypeTag $ typeCheckPattern patt
                (cmds', cmdseqns) <- typeCheckCmds cmds
                return (ttypepatt, ((patt', cmds'), patteqn <> cmdseqns))

        let ttypepexpr = annotateTypeTag ttypeexpr expr
            ttypesppatts = annotateTypeTags (NE.toList ttypespatts) (NE.toList $ fmap fst cases)
            eqn = TypeEqnsExist (ttypepexpr : ttypesppatts) $ 
                map (review _TypeEqnsEq . (typePtoTypeVar ttypepexpr,) . typePtoTypeVar) ttypesppatts
                <> expreqn
                <> fold caseseqns

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return (_CCase # (cxt, expr', cases'), [eqn])

    CSwitch cxt switches -> do
        ((ttypeexprs, eqns), switches') <- fmap (unzip *** NE.fromList <<< unzip <<< NE.toList) 
                $ forM switches 
                $ \(bexpr, cmds) ->  do   
                    (ttypeexpr, (bexpr', expreqn)) <- withFreshTypeTag $ typeCheckExpr bexpr
                    (cmds', cmdseqns) <- localEnvSt id $ typeCheckCmds cmds
                    return $ ((ttypeexpr, expreqn <> cmdseqns), (bexpr',cmds'))

        envLcl % typeInfoSymTab % symTabCh .= mempty

        let ttypepexprs = annotateTypeTags ttypeexprs (map fst $ NE.toList switches)
            eqn = TypeEqnsExist ttypepexprs $
                zipWith (\ann -> TypeEqnsEq . (,_TypeBoolF # Just (TypeAnnExpr $ ann)) . typePtoTypeVar) 
                    (map fst $ NE.toList switches) ttypepexprs
                <> concat eqns

        return (CSwitch cxt switches', [eqn])

    CIf cxt cond cthen celse -> do
        (ttypeexpr, (expr', expreqns)) <- withFreshTypeTag $ typeCheckExpr cond

        (cthen', ctheneqns) <- localEnvSt id $ typeCheckCmds cthen
        (celse', celseeqns) <- localEnvSt id $ typeCheckCmds celse

        let ttypepexpr = annotateTypeTag ttypeexpr cond
            eqns = 
                [ TypeEqnsEq (typePtoTypeVar ttypepexpr, _TypeBoolF # Just (TypeAnnExpr $ cond)) ]
                <> ctheneqns
                <> celseeqns
                <> expreqns

        envLcl % typeInfoSymTab % symTabCh .= mempty

        return ( CIf cxt expr' cthen' celse', eqns )

-------------------------
-- Kind checking
-------------------------
kindCheckProcessType :: 
    TypeCheck 
        ([TypeP MplRenamed], [MplType MplRenamed], [MplType MplRenamed], [MplType MplRenamed]) 
        (Maybe ([TypeP MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]))
kindCheckProcessType proctype@(varsyms, seqs, ins, outs) = do
    symtab <- guse (envLcl % typeInfoSymTab % symTabType)
    
    ~(res, st) <- 
        ( flip runStateT $ _KindCheckEnv #
            ( SeqKind ()
            , Map.fromList $ map (view uniqueTag &&& const Nothing ) varsyms
            ) )
        . ( flip runReaderT symtab) $ do
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
        return (map NamedType varsyms , seqs', ins', outs')

kindCheckFunType ::
    TypeCheck 
        ([TypeP MplRenamed], [MplType MplRenamed], MplType MplRenamed) 
        ([TypeP MplTypeChecked], [MplType MplTypeChecked], MplType MplTypeChecked)
kindCheckFunType proctype@(varsyms, froms, to) = do
    ~symtab <- guse (envLcl % typeInfoSymTab % symTabType)
    
    ~(froms', to') <- 
        ( flip evalStateT $ _KindCheckEnv # 
            ( SeqKind ()
            , Map.fromList $ map (view uniqueTag &&& const Nothing ) varsyms
            ) ) 
        . (flip runReaderT symtab) $ do
            ~froms' <- traverse 
                (\mpltype -> do
                    kindCheckExpectedPrimitiveKind .= SeqKind ()
                    primitiveKindCheck mpltype)
                    froms

            kindCheckExpectedPrimitiveKind .= SeqKind ()
            to' <- primitiveKindCheck to
            return $ (map fromJust froms', fromJust to')

    return (map NamedType varsyms , froms', to')

-------------------------
-- Utilities
-------------------------
freshChTypeTag :: TypeCheck ChIdentR TypeTag
freshChTypeTag ch = do
    tag <- freshTypeTag
    envLcl % typeInfoSymTab % symTabCh % at (ch ^. uniqueTag) ?=
        _SymEntry # (tag, ch) 
    return tag
