{-# LANGUAGE ScopedTypeVariables #-}
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

data MplTypeSub 

type instance IdP MplTypeSub = IdP MplTypeChecked
type instance TypeP MplTypeSub = TypeIdentT

type instance XMplType MplTypeSub = MplType MplTypeChecked
type instance XTypeSeqWithArgs MplTypeSub = ()
type instance XTypeSeqVarWithArgs MplTypeSub = ()
type instance XTypeConcWithArgs MplTypeSub = ()
type instance XTypeConcVarWithArgs  MplTypeSub = ()

type instance XTypeVar MplTypeSub = Maybe TypeAnn
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
type instance XTypeSeqArrF MplTypeSub = ()
type instance XTypeConcArrF MplTypeSub = ()

type instance XXMplBuiltInTypesF MplTypeSub = ()


class InstantiateType t where
    instantiateType :: 
        ( HasUniqueSupply s 
        , MonadState s m ) => 
        t -> m ([TypeP MplTypeSub], MplType MplTypeSub)

instance InstantiateType (MplFunction MplRenamed) where
    instantiateType fun@(MplFunction name Nothing defn) = do
        tag <- freshTypeTag
        let ttypep = undefined -- _TypeIdentT # (tag, _TypeVarPFun # fun)
        return $ ([ttypep], _TypeVar # (_Just % _TypeAnnFun # fun , ttypep))
    instantiateType fun@(MplFunction name (Just tp) defn) = do
        error "ahaha still need to do"

instance InstantiateType ([IdentT], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]) where
    instantiateType (tpvars, seqs, ins, outs) = do
        subs <- traverse (sequenceA <<< (id &&& toTypeIdentT)) tpvars
        return $ 
            ( map snd subs
            , _TypeConcArrF # 
                ( () 
                , (fromJust (traverse (substituteType subs) seqs))
                , (fromJust (traverse (substituteType subs) ins))
                , (fromJust (traverse (substituteType subs) outs))
                )
            )

substituteType ::
    [(TypeP MplTypeChecked, TypeP MplTypeSub)] ->
    MplType MplTypeChecked ->
    Maybe (MplType MplTypeSub)
substituteType sublist = cata f
  where
    f :: Base (MplType MplTypeChecked) 
        (Maybe (MplType MplTypeSub)) -> Maybe (MplType MplTypeSub)
    f (TypeVarF cxt typep) = TypeVar Nothing <$> lookup typep sublist
    {-
    f (TypeSeqWithArgsF cxt id args) =
        TypeSeqWithArgs cxt id <$> sequenceA args 
    f (TypeConcWithArgsF cxt id args) =
        TypeConcWithArgs cxt id <$> traverseOf each sequenceA args 
    f (TypeBuiltInF rst) = TypeBuiltIn . embedBuiltInTypes 
        <$> sequenceA rst 
    -}
