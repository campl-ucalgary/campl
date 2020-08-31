{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeCheckMplTypeSub where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil

import MplUtil.UniqueSupply

import Control.Monad.State
import Control.Arrow

import Data.Functor.Foldable (Base, cata, embed)
import Data.Void
import Data.Maybe

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Control.Applicative

import Debug.Trace

data MplTypeSub 

type instance IdP MplTypeSub = IdP MplTypeChecked
type instance TypeP MplTypeSub = TypeIdentT

type instance XMplType MplTypeSub = MplType MplTypeChecked
type instance XTypeSeqWithArgs MplTypeSub = ((), MplSeqObjDefn MplTypeCheckedClause )
type instance XTypeSeqVarWithArgs MplTypeSub = Void
type instance XTypeConcWithArgs MplTypeSub = ((), MplConcObjDefn MplTypeCheckedClause )
type instance XTypeConcVarWithArgs  MplTypeSub = Void

type instance XTypeVar MplTypeSub = Maybe TypeAnn
type instance XTypeWithNoArgs MplTypeSub = ()
type instance XXType MplTypeSub = Void
type instance XTypeIntF MplTypeSub = NameOcc
type instance XTypeCharF MplTypeSub = NameOcc
type instance XTypeDoubleF MplTypeSub = NameOcc
type instance XTypeStringF MplTypeSub = NameOcc
type instance XTypeUnitF MplTypeSub = NameOcc
type instance XTypeBoolF MplTypeSub = NameOcc
type instance XTypeListF MplTypeSub = NameOcc
type instance XTypeTupleF MplTypeSub = NameOcc

type instance XTypeGet MplTypeSub = NameOcc
type instance XTypePut MplTypeSub = NameOcc
type instance XTypeTensor MplTypeSub = NameOcc
type instance XTypePar MplTypeSub = NameOcc
type instance XTypeTopBot MplTypeSub = NameOcc
type instance XTypeNeg MplTypeSub = NameOcc
type instance XTypeSeqArrF MplTypeSub = 
    Maybe TypeAnn -- Maybe ([MplPattern MplRenamed], MplExpr MplRenamed)
type instance XTypeConcArrF MplTypeSub = 
    Maybe TypeAnn -- Maybe ( ([MplPattern MplRenamed], [ChIdentR], [ChIdentR]), NonEmpty (MplCmd MplRenamed) )

type instance XXMplBuiltInTypesF MplTypeSub = ()

class InstantiateArrType t where
    instantiateArrType :: 
        ( HasUniqueSupply s 
        , MonadState s m ) => 
        Maybe TypeAnn -> t -> m ([TypeP MplTypeSub], MplType MplTypeSub)

{-
instance InstantiateArrType (MplFunction MplRenamed) where
    instantiateArrType fun@(MplFunction name Nothing defn) = do
        tag <- freshTypeTag
        let ttypep = undefined -- _TypeIdentT # (tag, _TypeVarPFun # fun)
        return $ ([ttypep], _TypeVar # (_Just % _TypeAnnFun # fun , ttypep))

    instantiateArrType fun@(MplFunction name (Just tp) defn) = do
        error "ahaha still need to do"
-}

instance TypeP MplTypeChecked ~ tp => InstantiateArrType ([tp], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]) where
    instantiateArrType ann (tpvars, seqs, ins, outs) = do
        tags <- traverse (const freshTypeTag) tpvars
        let typeps = annotateTypeTags tags tpvars 
            subs = zipWith (\identt -> (identt,) . typePtoTypeVar) tpvars typeps
        return $ 
            ( typeps
            , _TypeConcArrF # 
                ( ann 
                , fromJust (traverse (instantiateTypeWithSubs subs) seqs)
                , fromJust (traverse (instantiateTypeWithSubs subs) ins)
                , fromJust (traverse (instantiateTypeWithSubs subs) outs)
                )
            )

instance InstantiateArrType (MplType MplTypeSub) where
    instantiateArrType ann tp = return ([], tp)

instance TypeP MplTypeChecked ~ tp => InstantiateArrType ([tp], [MplType MplTypeChecked], MplType MplTypeChecked) where
    instantiateArrType ann (tpvars, [], to) = do
        tags <- traverse (const freshTypeTag) tpvars
        let typeps = annotateTypeTags tags tpvars 
            subs = zipWith (\identt -> (identt,) . typePtoTypeVar) tpvars typeps
        -- TODO: this actualyl will not preserve the annotation information here...
        return $ ( typeps, fromJust $ instantiateTypeWithSubs subs to)
        
    instantiateArrType ann (tpvars, froms, to) = do
        tags <- traverse (const freshTypeTag) tpvars
        let typeps = annotateTypeTags tags tpvars 
            subs = zipWith (\identt -> (identt,) . typePtoTypeVar) tpvars typeps
        return $ 
            ( typeps, _TypeSeqArrF # 
                ( ann
                , NE.fromList $ fromJust $ traverse (instantiateTypeWithSubs subs) froms
                , fromJust $ instantiateTypeWithSubs subs to)
            )

instance TypeP MplTypeChecked ~ tp => InstantiateArrType ([tp], ([MplType MplTypeChecked], MplType MplTypeChecked), MplType MplTypeChecked) where
    instantiateArrType ann (tpvars, (froms, st), to) = 
        instantiateArrType ann (tpvars, froms ++[st], to)
        

instantiateTypeWithSubs ::
    [(TypeP MplTypeChecked, MplType MplTypeSub)] ->
    MplType MplTypeChecked ->
    Maybe (MplType MplTypeSub)
instantiateTypeWithSubs sublist = cata f
  where
    f :: Base (MplType MplTypeChecked) 
        (Maybe (MplType MplTypeSub)) -> Maybe (MplType MplTypeSub)
    f (TypeVarF cxt typep) = lookup typep sublist
    f (TypeSeqWithArgsF cxt id args) =
        TypeSeqWithArgs (mempty, cxt) id <$> sequenceA args 
    f (TypeConcWithArgsF cxt id args) =
        TypeConcWithArgs (mempty, cxt) id <$> traverseOf each sequenceA args 
    f (TypeBuiltInF rst) = error "to implement in substitute type"
    -- f (TypeBuiltInF rst) = TypeBuiltIn . embedBuiltInTypes <$> sequenceA rst 
    --

substituteTypeVars ::
    [(TypeP MplTypeChecked, MplType MplTypeChecked)] ->
    MplType MplTypeChecked -> 
    MplType MplTypeChecked
substituteTypeVars sublist = cata f
  where
    f :: Base (MplType MplTypeChecked) 
        (MplType MplTypeChecked) -> MplType MplTypeChecked
    f (TypeVarF cxt typep) = fromMaybe (_TypeVar # (cxt, typep)) $ lookup typep sublist
    f (TypeSeqWithArgsF cxt id args) = TypeSeqWithArgs cxt id args 
    f (TypeConcWithArgsF cxt id args) = TypeConcWithArgs cxt id args 
    f (TypeBuiltInF rst) = error "to implement in substitute type"

class TypeClauseSpineStateVarClauseSubs (t :: ObjectDefnTag) where
    typeClauseSpineStateVarClauseSubs :: 
        MplTypeClauseSpine MplTypeChecked t ->
        [(TypeP MplTypeChecked, MplType MplTypeChecked)]

instance TypeClauseSpineStateVarClauseSubs (SeqObjTag DataDefnTag) where
    typeClauseSpineStateVarClauseSubs = 
        foldMapOf (typeClauseSpineClauses % folded) f 
      where
        f :: MplTypeClause _ _ -> [(_, _)]
        f clause = 
            [ ( clause ^. typeClauseStateVar % to NamedType 
              , _TypeSeqWithArgs # 
                ( _DataDefn #  clause
                , clause ^. typeClauseName 
                , clause ^. typeClauseArgs 
                    % to (map (review _TypeVar . (Just $ SeqKind (),) . NamedType )) )
              ) 
            ]

-- duplicated code...
instance TypeClauseSpineStateVarClauseSubs (SeqObjTag CodataDefnTag) where
    typeClauseSpineStateVarClauseSubs = 
        foldMapOf (typeClauseSpineClauses % folded) f 
      where
        f :: MplTypeClause _ _ -> [(_, _)]
        f clause = 
            [ ( clause ^. typeClauseStateVar % to NamedType 
              , _TypeSeqWithArgs # 
                ( _CodataDefn #  clause
                , clause ^. typeClauseName 
                , clause ^. typeClauseArgs 
                    % to (map (review  _TypeVar . (Just $ SeqKind (),) . NamedType )) )
              ) 
            ]

class AnnotateTypeTagToTypeP t where
    annotateTypeTag :: TypeTag -> t -> TypeP MplTypeSub

instance AnnotateTypeTagToTypeP (([MplPattern MplRenamed], [ChIdentR], [ChIdentR]), NonEmpty (MplCmd MplRenamed)) where
    annotateTypeTag tag res =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnProcPhrase res

instance AnnotateTypeTagToTypeP ([MplPattern MplRenamed], MplExpr MplRenamed) where
    annotateTypeTag tag res =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnFunPhrase res


instance AnnotateTypeTagToTypeP (MplPattern MplRenamed) where
    annotateTypeTag tag patt =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnPatt patt

instance AnnotateTypeTagToTypeP ChIdentR where
    annotateTypeTag tag ch =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnCh ch

instance AnnotateTypeTagToTypeP (MplProcess MplRenamed) where
    annotateTypeTag tag res =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnProc res

instance AnnotateTypeTagToTypeP (MplFunction MplRenamed) where
    annotateTypeTag tag res =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnFun res

instance AnnotateTypeTagToTypeP (MplExpr MplRenamed) where
    annotateTypeTag tag res =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnExpr res

-- Meant for type variables only!!
instance AnnotateTypeTagToTypeP IdentR where
    annotateTypeTag tag identr =  _TypeIdentT # (tag, ann)
      where
        ann = _TypeIdentTInfoTypeVar # NamedType identr

instance AnnotateTypeTagToTypeP TypeT where
    annotateTypeTag tag tpt =  _TypeIdentT # (tag, ann)
      where
        ann = _TypeIdentTInfoTypeVar # tpt


-- the two lists should be the same size
annotateTypeTags :: AnnotateTypeTagToTypeP t => [TypeTag] -> [t] -> [TypeP MplTypeSub]
annotateTypeTags tags = zipWith annotateTypeTag tags

typePtoTypeVar :: TypeP MplTypeSub -> MplType MplTypeSub 
typePtoTypeVar typep = _TypeVar # ( typep ^? typeIdentTInfo % _TypeIdentTInfoTypeAnn, typep ) 

instance PPrint (MplBuiltInTypesF MplTypeSub (MplType MplTypeSub)) where
    pprint n = pprint $ mplTypeToBnfc (TypeBuiltIn n)

instance PPrint TypeTag where
    pprint (TypeTag n) = pprint n

instance PPrint TypeIdentT where
    pprint (TypeIdentT tag (TypeIdentTInfoTypeVar v)) = pprint v 
    pprint (TypeIdentT tag _) = pprint tag

