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
import MplPasses.TypeChecker.TypeEqns
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.Env

import Data.Maybe

import Data.Functor.Foldable (Base, cata, para)
import Data.Map (Map)
import qualified Data.Map as Map

typeCheckPatterns ::
    TypeCheck
        [MplPattern MplRenamed]
        [MplPattern MplTypeChecked]
typeCheckPatterns = undefined

typeCheckPattern ::
    TypeCheck
        (MplPattern MplRenamed)
        (MplPattern MplTypeChecked, Maybe [TypeEqns MplTypeSub])
typeCheckPattern = para f
  where
    f :: Base (MplPattern MplRenamed) (MplPattern MplRenamed, _ (MplPattern MplTypeChecked, Maybe [TypeEqns MplTypeSub]))
        -> _ (MplPattern MplTypeChecked, Maybe [TypeEqns MplTypeSub])
    f (PConstructorF cxt id patts) = error "pat not implemented"
    f (PRecordF cxt phrases) = error "pat not implemented"
    f (PVarF cxt v) = do
        ttypetag <- guse (envLcl % typeInfoEnvTypeTag)
        ttypetagstable <- freshTypeTag
        ttypemap <- guse (envLcl % typeInfoEnvMap)

        let ann = _TypeAnnPatt # (PVar cxt v)
            ttype =  _TypeIdentT # (ttypetag, TypeIdentTInfoTypeAnn ann)
            mplttype =  _TypeVar # (Just ann, ttype)
            eqns = [ TypeEqnsEqStable (ttype & typeIdentTUniqueTag .~ ttypetagstable, mplttype ) ]

        envLcl % typeInfoSymTab % at (v ^. uniqueTag) ?=  
                _SymSeqPattVar % _Just # mplttype

        return ( PVar (fromJust $ Map.lookup ttypetagstable ttypemap) v
            , Just eqns)
        -- error "pat not implemented"
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
