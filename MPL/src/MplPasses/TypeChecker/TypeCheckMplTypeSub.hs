{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeCheckMplTypeSub where

import Optics
import Optics.State.Operators

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
import Data.Proxy

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Control.Applicative

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Foldable
import Data.Tuple

import Debug.Trace

{- Module for defining type which is to be substituted..
 -
 - 
 -}

data MplTypeSub 

type instance IdP MplTypeSub = IdP MplTypeChecked
type instance TypeP MplTypeSub = TypeIdentT

type instance XMplType MplTypeSub = MplType MplTypeChecked
type instance XTypeSeqWithArgs MplTypeSub = (Maybe TypeAnn, MplSeqObjDefn MplTypeCheckedClause )
type instance XTypeSeqVarWithArgs MplTypeSub = Void
type instance XTypeConcWithArgs MplTypeSub = (Maybe TypeAnn, MplConcObjDefn MplTypeCheckedClause )
type instance XTypeConcVarWithArgs  MplTypeSub = Void

type instance XTypeVar MplTypeSub = Maybe TypeAnn
type instance XTypeWithNoArgs MplTypeSub = (Maybe TypeAnn, MplObjectDefn MplTypeCheckedClause)
type instance XXType MplTypeSub = Void
type instance XTypeIntF MplTypeSub = Maybe TypeAnn
type instance XTypeDoubleF MplTypeSub = Maybe TypeAnn
type instance XTypeCharF MplTypeSub = Maybe TypeAnn
-- type instance XTypeStringF MplTypeSub = Maybe NameOcc
type instance XTypeUnitF MplTypeSub = Maybe TypeAnn
type instance XTypeBoolF MplTypeSub = Maybe TypeAnn
type instance XTypeListF MplTypeSub = Maybe TypeAnn
type instance XTypeTupleF MplTypeSub = Maybe TypeAnn

type instance XTypeGet MplTypeSub = Maybe TypeAnn
type instance XTypePut MplTypeSub = Maybe TypeAnn
type instance XTypeTensor MplTypeSub = Maybe TypeAnn
type instance XTypePar MplTypeSub = Maybe TypeAnn
type instance XTypeTopBot MplTypeSub = Maybe TypeAnn
type instance XTypeNeg MplTypeSub = Maybe TypeAnn
type instance XTypeSeqArrF MplTypeSub = 
    Maybe TypeAnn -- Maybe ([MplPattern MplRenamed], MplExpr MplRenamed)
type instance XTypeConcArrF MplTypeSub = 
    Maybe TypeAnn -- Maybe ( ([MplPattern MplRenamed], [ChIdentR], [ChIdentR]), NonEmpty (MplCmd MplRenamed) )

type instance XXMplBuiltInTypesF MplTypeSub = ()



data InstantiateArrEnv = InstantiateArrEnv  {
    _instantiateArrEnvInstantiated :: Map (TypeP MplTypeChecked) (TypeP MplTypeSub)
    , _instantiatArrEnvUniqueSupply :: UniqueSupply
}  deriving Show

$(makeLenses ''InstantiateArrEnv)

instance HasUniqueSupply InstantiateArrEnv where
    uniqueSupply = instantiatArrEnvUniqueSupply 

freshInstantiateArrEnv ::
    ( HasUniqueSupply s 
    , MonadState s m ) => m InstantiateArrEnv
freshInstantiateArrEnv = do
    sup <- freshUniqueSupply
    return $ InstantiateArrEnv mempty sup

runInstantiateArrType :: 
    State InstantiateArrEnv a -> InstantiateArrEnv -> ([TypeP MplTypeSub], a)
runInstantiateArrType act 
    = first (toListOf (instantiateArrEnvInstantiated % folded)) 
    . swap 
    . runState act 

updateInstantiated ::
    ( MonadState InstantiateArrEnv m ) =>
    [TypeP MplTypeChecked] -> m ()
updateInstantiated ns = for_ ns $ \n -> do
    tag <- freshTypeTag
    instantiateArrEnvInstantiated % at n %= maybe (Just $ annotateTypeTag tag n) Just

getInstantiatedSubs :: 
    ( MonadState InstantiateArrEnv m ) =>
    m [(TypeP MplTypeChecked, MplType MplTypeSub)] 
getInstantiatedSubs = do
    tosubs <- guse $ instantiateArrEnvInstantiated 
    return $ map (second typePtoTypeVar) $ itoListOf ifolded tosubs

updateInstantiatedAndGetSubs :: 
    ( MonadState InstantiateArrEnv m ) =>
    [TypeP MplTypeChecked] -> 
    m [(TypeP MplTypeChecked, MplType MplTypeSub)]
updateInstantiatedAndGetSubs ns = do
    updateInstantiated ns
    getInstantiatedSubs
    

class InstantiateArrType t where
    instantiateArrType :: 
        ( MonadState InstantiateArrEnv m ) => 
        Maybe TypeAnn -> t -> m (MplType MplTypeSub)

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
        updateInstantiated tpvars
        subs <- getInstantiatedSubs
        return $ 
            _TypeConcArrF # 
                ( ann 
                , fromJust (traverse (instantiateTypeWithSubs ann subs) seqs)
                , fromJust (traverse (instantiateTypeWithSubs ann subs) ins)
                , fromJust (traverse (instantiateTypeWithSubs ann subs) outs)
                )

instance InstantiateArrType (MplType MplTypeSub) where
    instantiateArrType ann tp = return tp

instance TypeP MplTypeChecked ~ tp => InstantiateArrType ([tp], [MplType MplTypeChecked], MplType MplTypeChecked) where
    instantiateArrType ann (tpvars, [], to) = do
        updateInstantiated tpvars
        subs <- getInstantiatedSubs
        -- TODO: this actually will not preserve the annotation information here...
        return $ fromJust $ instantiateTypeWithSubs ann subs to
        
    instantiateArrType ann (tpvars, froms, to) = do
        updateInstantiated tpvars
        subs <- getInstantiatedSubs
        return $ 
            _TypeSeqArrF # 
                ( ann
                , NE.fromList $ fromJust $ traverse (instantiateTypeWithSubs ann subs) froms
                , fromJust $ instantiateTypeWithSubs ann subs to
                )

instance TypeP MplTypeChecked ~ tp => InstantiateArrType ([tp], ([MplType MplTypeChecked], MplType MplTypeChecked), MplType MplTypeChecked) where
    instantiateArrType ann (tpvars, (froms, st), to) = 
        instantiateArrType ann (tpvars, froms ++[st], to)

instantiateTypeWithSubs ::
    Maybe TypeAnn -> 
    [(TypeP MplTypeChecked, MplType MplTypeSub)] ->
    MplType MplTypeChecked ->
    Maybe (MplType MplTypeSub)
instantiateTypeWithSubs ann sublist = cata f
  where
    f :: Base (MplType MplTypeChecked) 
        (Maybe (MplType MplTypeSub)) -> Maybe (MplType MplTypeSub)
    f (TypeVarF cxt typep) = return $ fromMaybe 
        -- TODO: I can't really remember how to annotate types rn
        -- but perhaps it would be better to give an annotation here.
        (TypeVar ann
            (TypeIdentT (TypeTag $ typep ^. uniqueTag) 
            $ TypeIdentTInfoTypeVar typep)
        )
        (lookup typep sublist)
    f (TypeWithNoArgsF cxt id) = return $ TypeWithNoArgs (ann, cxt) id
    f (TypeSeqWithArgsF cxt id args) =
        TypeSeqWithArgs (ann, cxt) id <$> sequenceA args 
    f (TypeConcWithArgsF cxt id args) =
        TypeConcWithArgs (ann, cxt) id <$> traverseOf each sequenceA args 
    f (TypeBuiltInF rst) = case rst of
        -- TODO: we can preserve some error information here.
        TypeIntF cxt -> return $ _TypeIntF # ann
        TypeDoubleF cxt -> return $ _TypeDoubleF # ann
        TypeCharF cxt -> return $ _TypeCharF # ann
        TypeBoolF cxt -> return $ _TypeBoolF # ann
        TypeUnitF cxt -> return $ _TypeUnitF # ann
        TypeListF cxt rst -> do
            rst' <- rst
            return $ _TypeListF # (ann, rst')


        TypeTupleF cxt (t0,t1,ts) -> do
            ~(t0':t1':ts') <- sequenceA $ t0:t1:ts
            return $ _TypeTupleF # (ann, (t0',t1',ts'))

        TypeGetF cxt seq conc -> do
            seq' <- seq
            conc' <- conc
            return $ _TypeGetF # (annotate cxt, seq', conc')

        TypePutF cxt seq conc -> do
            seq' <- seq
            conc' <- conc
            return $ _TypePutF # (annotate cxt, seq', conc')

        TypeTensorF cxt a b -> do
            a' <- a
            b' <- b
            return $ _TypeTensorF # (annotate cxt, a', b')
        TypeParF cxt a b -> do
            a' <- a
            b' <- b
            return $ _TypeParF # (annotate cxt, a', b')

        TypeNegF cxt a -> do
            a' <- a
            return $ _TypeNegF # (annotate cxt, a')

        TypeTopBotF cxt -> 
            return $ _TypeTopBotF # annotate cxt
      where
        -- normally, we don't care about the annotaiotn nomrally provided.. it should be replaced
        annotate cxt = ann -- review _TypeChAnnNameOcc <$> cxt

    -- f (TypeBuiltInF rst) = TypeBuiltIn . embedBuiltInTypes <$> sequenceA rst 

substituteTypeVars ::
    [(TypeP MplTypeChecked, MplType MplTypeChecked)] ->
    MplType MplTypeChecked -> 
    MplType MplTypeChecked
substituteTypeVars sublist = cata f
  where
    f :: Base (MplType MplTypeChecked) 
        (MplType MplTypeChecked) -> MplType MplTypeChecked
    f (TypeVarF cxt typep) = fromMaybe (_TypeVar # (cxt, typep)) $ 
        lookup typep sublist
    f (TypeSeqWithArgsF cxt id args) = TypeSeqWithArgs cxt id args 
    f (TypeConcWithArgsF cxt id args) = TypeConcWithArgs cxt id args 
    f n = embed n

typeClauseSpineStateVarClauseSubs :: 
    TypeClauseToMplType t => MplTypeClauseSpine MplTypeChecked t ->
    [(TypeP MplTypeChecked, MplType MplTypeChecked)]
typeClauseSpineStateVarClauseSubs = 
    foldMapOf (typeClauseSpineClauses % folded) f 
  where
    f clause = [ (clause ^. typeClauseStateVar % to NamedType, typeClauseToMplType clause) ]

class TypeClauseToMplType (t :: ObjectDefnTag) where
    typeClauseToMplType :: 
        MplTypeClause MplTypeChecked t -> MplType MplTypeChecked 

instance TypeClauseToMplType (SeqObjTag DataDefnTag) where
    typeClauseToMplType clause = _TypeSeqWithArgs # 
                ( _DataDefn # clause
                , clause ^. typeClauseName 
                , clause ^. typeClauseArgs 
                    % to (map (review _TypeVar . (Just $ SeqKind (),) . NamedType )) )

-- duplciated code
instance TypeClauseToMplType (SeqObjTag CodataDefnTag) where
    typeClauseToMplType clause = _TypeSeqWithArgs # 
                ( _CodataDefn # clause
                , clause ^. typeClauseName 
                , clause ^. typeClauseArgs 
                    % to (map (review _TypeVar . (Just $ SeqKind (),) . NamedType )) )

instance TypeClauseToMplType (ConcObjTag ProtocolDefnTag) where
    typeClauseToMplType clause = _TypeConcWithArgs # 
                ( _ProtocolDefn # clause
                , clause ^. typeClauseName 
                , clause ^. typeClauseArgs 
                    % to (over each (map (review _TypeVar . (Just $ SeqKind (),) . NamedType )))
                )

-- duplicated code..
instance TypeClauseToMplType (ConcObjTag CoprotocolDefnTag) where
    typeClauseToMplType clause = _TypeConcWithArgs # 
                ( _CoprotocolDefn # clause
                , clause ^. typeClauseName 
                , clause ^. typeClauseArgs 
                    % to (over each (map (review _TypeVar . (Just $ SeqKind (),) . NamedType )))
                ) 

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

instance AnnotateTypeTagToTypeP (MplCmd MplRenamed) where
    annotateTypeTag tag ch =  _TypeIdentT # (tag, TypeIdentTInfoTypeAnn ann)
      where
        ann = TypeAnnCmd ch

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

instance PPrint (MplBuiltInTypesF MplTypeSub (MplType MplTypeSub)) y where
    pprint proxy n = pprint proxy $ mplTypeToBnfc proxy (TypeBuiltIn n)

instance PPrint TypeTag y where
    pprint proxy (TypeTag n) = pprint proxy n

-- overlapping so it is printable without the unqiue tag.
instance {-# OVERLAPPING #-} PPrint TypeIdentT MplParsed where
    pprint proxy (TypeIdentT tag (TypeIdentTInfoTypeVar v)) = case v of
        NamedType identt -> identt ^. name % coerced 
        GenNamedType _ -> "T" ++ pprint proxy tag
    -- pprint proxy (TypeIdentT tag (TypeIdentTInfoTypeVar v)) = pprint proxy v  ++ "__" ++ pprint proxy tag
    pprint proxy (TypeIdentT tag _) = "T" ++ pprint proxy tag


instance PPrint TypeIdentT y where
    pprint proxy (TypeIdentT tag (TypeIdentTInfoTypeVar v)) = case v of
        NamedType identt -> identt ^. name % coerced ++ "__" ++ pprint proxy tag
        GenNamedType _ -> "T" ++ pprint proxy tag
    -- pprint proxy (TypeIdentT tag (TypeIdentTInfoTypeVar v)) = pprint proxy v  ++ "__" ++ pprint proxy tag
    pprint proxy (TypeIdentT tag _) = "T" ++ pprint proxy tag

