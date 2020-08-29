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
import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeEqns
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil 
import MplPasses.TypeChecker.TypeCheckSym 
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

        ~(SymEntry lkuptp (SymSeqPhraseCall seqdef)) <- fmap fromJust 
            $ guse (envLcl % typeInfoSymTab % symTabTerm % at (n ^. uniqueTag))
        let patt = (PConstructor cxt n (map fst patts) :: MplPattern MplRenamed) 

        tell $ flip (maybe []) (seqdef ^? _CodataDefn ) $ \defn ->
            [ _IllegalPattDataCallGotCodataInstead # (patt, defn) ]
            -- should the defn be of type MplSeqObjDefn MplTypeChecked?

        (ttypepatts, (patts', pattacceqns)) <- second unzip . unzip <$> 
            traverse (withFreshTypeTag . snd ) patts

        sup <- freshUniqueSupply 
        let ttypep = annotateTypeTagToTypeP ttype patt
            ttypeppatts = annotateTypeTagToTypePs ttypepatts (map fst patts)
            
            (ttypesphrase, lkuptp') = (`evalState`sup) 
                $ instantiateArrType -- (_Just % _TypeAnnPatt # patt)
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
            ( (fromJust $ seqdef ^? _DataDefn
              -- THIS WILL THROW ERRORS ALWAYS -- TODO, we need to convert
              -- all of these type anotations to ``function like annotations"
              , fromJust $ ttypemap ^? at ttypestable % _Just % _SymType )
            , n
            , patts'
            ), [eqns])


    f (PRecordF cxt phrases) = error "pat not implemented"
    f (PVarF cxt v) = do
        ttype <- guse (envLcl % typeInfoEnvTypeTag)
        ttypestable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _TypeAnnPatt # (PVar cxt v)
            ttypep =  _TypeIdentT # (ttype, TypeIdentTInfoTypeAnn ann)
            mplttype =  _TypeVar # (Just ann, ttypep)
            eqns = [ TypeEqnsEqStable (ttypep & typeIdentTUniqueTag .~ ttypestable, mplttype ) ]

            res = PVar (fromJust $ ttypemap ^? at ttypestable % _Just % _SymType) v 

        envLcl % typeInfoSymTab % symTabTerm % at (v ^. uniqueTag) ?= 
            _SymEntry # (_SymSub # mplttype, _SymSeqCall % _ExprCallPattern # res )

        return (res, eqns)

    f (PNullF cxt) = error "pat not implemented"
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
