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
        ttypestable <- freshTypeTag
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
            traverse (withFreshTypeTag . snd ) patts

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
                    -- stable equation for this expression
                    ,  genStableEqn ttypestable ttypep ]
                    -- accumulate the equations
                    <> concat pattacceqns
        
        return ( _PConstructor # 
                ( (seqdef 
                -- THIS WILL THROW ERRORS ALWAYS -- TODO, we need to convert
                -- all of these type anotations to ``function like annotations"
                , fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq )
                , n
                , patts'
                ), [eqns]
            )

    -- This will look very similar to the record expressions!
    f (PRecordF cxt phrases) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let patt = _PRecord # (cxt, phrases & mapped % _3 %~ fst) :: MplPattern MplRenamed
            -- ann = _TypeAnnPatt # patt

        st <- guse equality
        sup <- freshUniqueSupply
        arrenv <- freshInstantiateArrEnv
        ((ttypepphrases, (phrases', phraseseqns)), ttypepinst) <- fmap 
            ((second NE.unzip <<< NE.unzip) 
                *** (toListOf (_2 % instantiateArrEnvInstantiated % folded)))
            $ (`runStateT` (st & uniqueSupply .~ sup, arrenv))
            $ for phrases $ \((), ident, (_, mpatt)) ->
                undefined

        -- TODO get back to this later...
        error "record pat not implemented"

    f (PVarF cxt v) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)


        let ann = _TypeAnnPatt # (PVar cxt v)
            ttypep =  _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            mplttype =  _TypeVar # (Just ann, ttypep)
            eqns = [ TypeEqnsEqStable (ttypep & typeIdentTUniqueTag .~ ttypestable, mplttype ) ]

            res = PVar (fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq) v 

        envLcl % typeInfoSymTab % symTabExpr % at (v ^. uniqueTag) ?= 
            _SymEntry # (_SymImplicit # mplttype, _SymSeqCall % _ExprCallPattern # res )

        return (res, eqns)

    f (PNullF cxt) = do 
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let patt = PNull cxt :: MplPattern MplRenamed

        return 
            ( PNull (cxt, fromJust $ ttypemap ^? at ttypestable % _Just % _SymTypeSeq)
            , [ genStableEqn ttypestable (annotateTypeTag ttype patt) ]
            )
{-
    PConstructor !(XPConstructor x) (IdP x) [MplPattern x]
    | PRecord !(XPRecord x) (NonEmpty (XPRecordPhrase x, IdP x, MplPattern x) )
    | PVar !(XPVar x) (IdP x)
    | PNull !(XPNull x) 

    -- built in patterns
    | PUnit !(XPUnit x)
    | PTuple !(XPTuple x) (MplPattern x, MplPattern x, [MplPattern x])
    | PList !(XPList x) [MplPattern x]
    | PString !(XPString x) [MplPattern x]
    | PListCons !(XPListCons x) (MplPattern x) (MplPattern x)
    | PChar !(XPChar x) Char
    | PInt !(XPInt x) Int

    | XPattern !(XXPattern x)
-}
