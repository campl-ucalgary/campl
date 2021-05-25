{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeCheckPatt where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.TypeChecker.TypeCheckUtils
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeEqns
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckSymUtils
import MplPasses.TypeChecker.TypeCheckErrorPkg
import MplPasses.TypeChecker.TypeCheckCallErrors
import MplPasses.Env

import MplUtil.UniqueSupply

import Data.Maybe

import Data.Functor.Foldable (Base, cata, para)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow
import Control.Applicative

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable

import Data.Traversable

import Debug.Trace

typeCheckPattern ::
    TypeCheck
        (MplPattern MplRenamed)
        (MplPattern MplTypeChecked, [TypeEqns MplTypeSub])
typeCheckPattern = para f
  where
    f :: Base (MplPattern MplRenamed) (MplPattern MplRenamed, _ (MplPattern MplTypeChecked, [TypeEqns MplTypeSub]))
        -> _ (MplPattern MplTypeChecked, [TypeEqns MplTypeSub])
    f (PConstructorF cxt n patts) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let patt = (PConstructor cxt n (map fst patts) :: MplPattern MplRenamed) 

        ~(SymEntry lkuptp (SymSeqPhraseCall (DataDefn seqdef))) <- zoom (envLcl % typeInfoSymTab) $ do
            res <- guse $ symTabExpr % at (n ^. uniqueTag)
            let callterm = maybe (_Just % _CannotCallTerm # n) (const Nothing) res
            tell $ review _InternalError $ maybeToList $ callterm
            tell $ review _InternalError $ maybeToList $ 
                res ^? _Just 
                        % symEntryInfo 
                        % _SymSeqPhraseCall 
                        % _CodataDefn 
                        % to (review _IllegalPattDataCallGotCodataInstead . (patt,))

            return $ fromJust res

        ~(ttypepatts, (patts', pattacceqns)) <- second unzip . unzip <$> 
            traverse (withFreshTypeTag . snd) patts

        arrenv <- freshInstantiateArrEnv 
        let ttypep = annotateTypeTag ttype patt
            ttypeppatts = annotateTypeTags ttypepatts (map fst patts)
            
            ~(ttypesphrase, lkuptp') = (`runInstantiateArrType`arrenv) 
                $ instantiateArrType (_Just % _TypeAnnPatt # patt)
                $ fromJust 
                $ lkuptp ^? _SymDataPhrase % noStateVarsType


            eqns = TypeEqnsExist (ttypesphrase ++ ttypeppatts) $
                    [ TypeEqnsEq 
                        ( lkuptp'
                        , mkTypeSubSeqArr (ttypeppatts, ttypep)
                        ) 
                    ]
                    -- CHANGED FROM STABLE EQN
                    -- accumulate the equations
                    <> concat pattacceqns
        
        return ( _PConstructor # 
                ( (seqdef 
                    -- CHANGED FROM STABLE EQN
                , fromJust $ lookupInferredSeqTypeExpr ttype ttypemap )
                , n
                , patts'
                ), [eqns]
            )

    -- This will look very similar to the record expressions!
    f (PRecordF cxt phrases) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let patt = _PRecord # (cxt, phrases & mapped % _3 %~ fst) :: MplPattern MplRenamed
            ttypep = annotateTypeTag ttype patt

        {- TODO, given a tuple projections P1, and P2, this will type check
         -
         - (P1 := a) -> a
         - 
         - when clearly, we should have two arguments to the tuple.. Not totally
         - sure where we should put this error tho? Moreover, we should check for duplicated
         - pattern matching i.e., (P1 := a, P1 := b) -> a is allowed... 
         - I guess this is also a compilation of pattern matching sort of error thing
         - going on again -- so we delay this to later...
         -}

        st <- guse equality
        sup <- freshUniqueSupply
        arrenv <- freshInstantiateArrEnv
        (((ttypeppatts, phrases'), phraseseqns), ttypepinst) <- fmap 
            (( first NE.unzip <<< NE.unzip)
                *** (toListOf (instantiateArrEnvInstantiated % folded)))
            $ (`runStateT` arrenv)
            $ for phrases $ \((), ident, (_, mpatt)) -> do
                -- duplicated checking code from the constructor definition..
                ~(SymEntry lkuptp (SymSeqPhraseCall (CodataDefn seqdef))) <- lift $ zoom (envLcl % typeInfoSymTab) $ do
                    res <- guse $ symTabExpr % at (ident ^. uniqueTag)
                    let callterm = maybe (_Just % _CannotCallTerm # ident) (const Nothing) res
                    tell $ review _InternalError $ maybeToList $ callterm
                    tell $ review _InternalError $ maybeToList $ 
                        res ^? _Just 
                                % symEntryInfo 
                                % _SymSeqPhraseCall 
                                % _DataDefn 
                                % to (review _IllegalPattCodataCallGotDataInstead . (patt,))
                    return $ fromJust res

                (ttypepatt, (patt', pattseqns)) <- lift $ withFreshTypeTag mpatt 

                (ttypepphrase, ttypeclause) <- state $ runState $ do
                    ttypepphrases <- instantiateArrType
                        {- TODO, probably should include some sort of annotation
                        - information here... e.g. (_Just % TypeAnnPatt seqdef) -}
                        Nothing
                        $ fromJust $ lkuptp ^? _SymCodataPhrase
                            % noStateVarsType
                            % to (over _2 (view _1))
                    subs <- getInstantiatedSubs
                    return 
                        ( ttypepphrases
                        , seqdef ^. typePhraseExt % to 
                            ( fromJust 
                            . instantiateTypeWithSubs subs 
                            . typeClauseToMplType )
                        )
                let ttypeppatt = annotateTypeTag ttypepatt patt
                    phraseeqns = 
                        -- the type of the patt is the same as the 
                        -- type of the phrase...
                        [ TypeEqnsEq
                            ( typePtoTypeVar ttypeppatt
                            , ttypepphrase ) 
                        -- The type of this pattern is the type of the clause..
                        , TypeEqnsEq
                            ( ttypeclause
                            , typePtoTypeVar ttypep )
                        ]

                return ((ttypeppatt, (seqdef, ident, patt')), phraseeqns <> pattseqns)

        let eqn = TypeEqnsExist (ttypepinst <> NE.toList ttypeppatts) $
                fold phraseseqns

        return 
            ( _PRecord # 
              ( ( cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
              , phrases')
            , [eqn] )

    f (PVarF cxt v) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _TypeAnnPatt # (PVar cxt v)
            ttypep =  _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            eqns = [ TypeEqnsEq (typePtoTypeVar ttypep , typePtoTypeVar ttypep) ]

            res = _PVar # (fromJust $ lookupInferredSeqTypeExpr ttype ttypemap, v)

        envLcl % typeInfoSymTab % symTabExpr % at (v ^. uniqueTag) ?= 
            _SymEntry # 
                ( _SymImplicit # typePtoTypeVar ttypep 
                , _SymSeqCall % _ExprCallPattern # res )

        return (res, eqns)

    f (PIntF cxt v) = do 
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _TypeAnnPatt # (PInt cxt v)
            ttypep =  _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            eqns = [ TypeEqnsEq (typePtoTypeVar ttypep , _TypeIntF % _Just # ann ) ]

            res = _PInt # ((cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap), v)
        return (res, eqns)

    -- duplicated from 'PInt' case
    f (PCharF cxt v) = do 
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _TypeAnnPatt # (PChar cxt v)
            ttypep =  _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            eqns = [ TypeEqnsEq (typePtoTypeVar ttypep , _TypeCharF % _Just # ann ) ]

            res = _PChar # ((cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap), v)
        return (res, eqns)

    f (PBoolF cxt v) = do 
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _TypeAnnPatt # (PBool cxt v)
            ttypep =  _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            eqns = [ TypeEqnsEq (typePtoTypeVar ttypep , _TypeBoolF % _Just # ann ) ]

            res = _PBool # ((cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap), v)
        return (res, eqns)


    f (PNullF cxt) = do 
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        -- let patt = PNull cxt :: MplPattern MplRenamed
        let ann = _TypeAnnPatt # (PNull cxt)
            ttypep =  _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            eqns = [ TypeEqnsEq (typePtoTypeVar ttypep , typePtoTypeVar ttypep) ]

        return 
            ( PNull (cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
            , eqns
            )

    f (PTupleF cxt (t0,t1,ts)) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        (ttypet0, (t0pat, t0eqns)) <- withFreshTypeTag $ snd t0
        (ttypet1, (t1pat, t1eqns)) <- withFreshTypeTag $ snd t1
        (ttypests, (tspat, tseqns)) <- fmap (second unzip . unzip) $ traverse (withFreshTypeTag . snd) ts

        let ann = _TypeAnnPatt # (PTuple cxt (fst t0, fst t1, map fst ts) :: MplPattern MplRenamed)
            ttypep = _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            eqns =
                [ TypeEqnsEq 
                    ( typePtoTypeVar ttypep
                    , _TypeTupleF # 
                        ( _Just % _NameOcc # (tupleName (2 + length ts), cxt)
                        , 
                            ( TypeVar Nothing 
                                $ annotateTypeTag ttypet0 
                                $ fst t0
                            , TypeVar Nothing 
                                $ annotateTypeTag ttypet1 
                                $ fst t1
                            , map (TypeVar Nothing . uncurry annotateTypeTag) 
                                $ zip ttypests (map fst ts)
                            )
                        )
                    )
                ] 
                <> t0eqns
                <> t1eqns
                <> concat tseqns
            res = PTuple 
                    (cxt, fromJust $ lookupInferredSeqTypeExpr ttype ttypemap)
                    ( t0pat
                    , t1pat
                    , tspat
                    )
        return (res , eqns)
