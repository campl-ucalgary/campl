{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeEqns where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.Env
import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil 

import MplUtil.UniqueSupply

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad

import Control.Arrow

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Base, cata, embed)
import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bool

import Debug.Trace

data TypeEqns x =
    TypeEqnsEq (MplType x, MplType x)
    | TypeEqnsExist [TypeP x] [TypeEqns x]
    | TypeEqnsForall [TypeP x] [TypeEqns x]

    -- used to recover the intermediate types..
    | TypeEqnsEqStable ( TypeP x, MplType x )

deriving instance ( ForallMplType Show x , Show (IdP x) ) => 
    Show (TypeEqns x)

data TypeUnificationError x =
    TypeMatchFailure (MplType x) (MplType x)
    | TypeOccursCheck (TypeP x) (MplType x)
    | TypeForallMatchFailure (TypeP x, MplType x)
deriving instance ( Eq (MplType x), ForallMplType Eq x , Eq (IdP x) ) => 
    Eq (TypeEqns x)

data Package x = Package {
    _packageUnivVar :: Set (TypeP x)
    , _packageExisVar :: Set (TypeP x)
    , _packageSubs :: [(SubTag, ( TypeP x, MplType x ))]
}
deriving instance ( Eq (MplType x), ForallMplType Show x , Show (IdP x) ) => 
    Show (Package x)
deriving instance ( Eq (MplType x), ForallMplType Eq x , Eq (IdP x) ) => 
    Eq (Package x)

data SubTag = PlainSubTag | StableSubTag
  deriving (Show, Eq)

type Sub x = (TypeP x, MplType x)
type TaggedSub x = (SubTag, Sub x)

instance Ord (TypeP x) => Semigroup (Package x) where
    Package a0 b0 c0 <> Package a1 b1 c1 = 
        Package (a0 <> a1) (b0 <> b1) (c0 <> c1)

instance Ord (TypeP x) => Monoid (Package x) where
    mempty = Package mempty mempty mempty

$(makeClassyPrisms ''TypeUnificationError)
$(concat <$> traverse makePrisms
    [ ''TypeEqns
    , ''SubTag ]
 )
$(makeBaseFunctor ''TypeEqns)
$(makeLenses ''Package)

substitute ::
    Eq (TypeP x) =>
    (TypeP x, MplType x) -> 
    MplType x -> 
    MplType x
substitute (v, sub) = cata f
  where
    f (TypeVarF cxt v') 
        | v == v' = sub
        | otherwise = TypeVar cxt v'
    f (TypeSeqVarWithArgsF cxt v' []) 
        | v == v' = sub
        | otherwise = TypeSeqVarWithArgs cxt v' mempty
    f (TypeConcVarWithArgsF cxt v' ([],[])) 
        | v == v' = sub
        | otherwise = TypeConcVarWithArgs cxt v' mempty
    f n = embed n

match :: 
    ( AsTypeUnificationError e x
    , MonadError e m
    , Eq (IdP x) ) => 
    MplType x -> MplType x -> 
    m [(TypeP x, MplType x)]
match = f
  where
    f (TypeVar cxt0 a) (TypeVar cxt1 b) = 
     return [(a, TypeVar cxt1 b)]

    {-
    f type0@(TypeSeqWithArgs cxt0 a args) type1@(TypeSeqWithArgs cxt1 b brgs) 
        | a == b && length args == length brgs =
            concat <$> traverse (uncurry f) (zip args brgs)
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)

    -- TypeSeqVarWithArgs !(XTypeSeqVarWithArgs x) (TypeP x) [MplType x]
        
    f type0@(TypeConcWithArgs _ a (args0, args1)) type1@(TypeConcWithArgs _ b (brgs0, brgs1)) 
        | a == b && length args0 == length brgs0 && length args1 == length brgs1 =
            concat <$> traverse (uncurry f) (zip (args0 <> args1) (brgs0 <> brgs1))
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)
        -}
    -- TypeConcVarWithArgs !(XTypeConcVarWithArgs x) (TypeP x) ([MplType x], [MplType x])
    {-
     -
    | TypeBuiltIn !(MplBuiltInTypesF x (MplType x))
    | XType !(XXType x)

    -- builtin..
    -- primitive sequential types
    TypeIntF !(XTypeIntF x)
    | TypeCharF !(XTypeCharF x)
    | TypeDoubleF !(XTypeDoubleF x)
    -- primitive concurrent types
    | TypeGetF !(XTypeGet x) r r
    | TypePutF !(XTypePut x) r r
    | TypeTensorF !(XTypeTensor x) r r
    | TypeParF !(XTypePar x) r r
    | TypeNegF !(XTypeNeg x) r
    | TypeTopBotF !(XTypeTopBot x)

    -- built in non primitive types
    | TypeStringF !(XTypeStringF x)
    | TypeUnitF !(XTypeUnitF x)
    | TypeBoolF !(XTypeBoolF x)
    | TypeListF !(XTypeListF x) r
    | TypeTupleF !(XTypeTupleF x) (r, r, [r])

    -- arrow types
    | TypeSeqArrF !(XTypeSeqArrF x) (NonEmpty r) r
    | TypeConcArrF !(XTypeConcArrF x) [r] [r] [r] 
    -}

failsOccursCheck ::
    Eq (TypeP x) => 
    TypeP x ->
    MplType x ->
    Bool
failsOccursCheck _ (TypeVar _ _) = False
failsOccursCheck _ (TypeSeqVarWithArgs _ _ []) = False
failsOccursCheck _ (TypeConcVarWithArgs _ _ ([],[])) = False
failsOccursCheck n rst = n `elem` mplTypeCollectTypeP rst


mkValidSub ::
    ( AsTypeUnificationError e x 
    , Eq (TypeP x)
    , Eq (IdP x)
    , MonadError e m ) => 
    TypeP x -> 
    MplType x ->
    m (TypeP x, MplType x)
mkValidSub v exp = bool (pure (v, exp)) (throwError $ _TypeOccursCheck # (v,exp)) $ failsOccursCheck v exp

coalesce ::
    ( AsTypeUnificationError e x 
    , Eq (TypeP x)
    , Eq (IdP x)
    , MonadError e m ) => 
    Sub x -> 
    [TaggedSub x] ->
    m [TaggedSub x]
coalesce (s, ssub) = fmap concat . traverse f
  where
    f (PlainSubTag, (t, tsub)) 
        | s == t = map (PlainSubTag,) <$> match ssub tsub
        | otherwise = pure . (PlainSubTag,) <$> 
            mkValidSub t (substitute (s, ssub) tsub)
    f (StableSubTag, (t, tsub)) = pure . (StableSubTag,) <$> 
            mkValidSub t (substitute (s, ssub) tsub)

linearize ::
    ( AsTypeUnificationError e x 
    , Eq (TypeP x) 
    , Eq (MplType x)
    , Eq (IdP x)
    , MonadError e m ) => 
    [TaggedSub x] -> 
    m [TaggedSub x]
linearize = fmap fst . f . ([],)
  where
    f :: ([_], [_]) -> _ ([_], [_])
    f (subs, []) = pure (subs, [])
    f (subs, (StableSubTag, t):ts) = 
        f ((StableSubTag, t) : subs, ts)
    f (subs, (PlainSubTag, t):ts) =  
        ((PlainSubTag, t) : g (subs, h subs),)
        <$> coalesce t 
            ( alignSubs (t ^. _1)
            . filter (not . isTrivialSub) $ ts)
        >>= f
      where
        -- | We need to back substitute to keep the
        -- system ``consistent".

        -- | substitute until there are no more changes.
        g (subs', subs) 
            | subs' == subs = subs'
            | otherwise = g (subs', h subs')

        -- | substitute helper (note: it is not necessary
        -- to check for occurs check here because if an 
        -- occurs check existed, it would have already happened
        -- and not be back substituted)
        h = map (second (second (substitute t)))

isTrivialSub ::
    ( Eq (TypeP x) ) => 
    TaggedSub x ->
    Bool
isTrivialSub (PlainSubTag, (s, expr)) = case expr of
    TypeVar _ t -> s == t
    TypeSeqVarWithArgs _ t [] -> s == t
    TypeConcVarWithArgs _ t ([],[]) -> s == t
    _ -> False
isTrivialSub _ = False

alignSubs ::    
    ( Eq (TypeP x) ) =>
    TypeP x ->
    [TaggedSub x] ->
    [TaggedSub x]
alignSubs k = map f
  where
    f t@(PlainSubTag, (a, TypeVar cxt b))
        | k == b = (PlainSubTag, (b, TypeVar cxt a))
        | otherwise = t
    f t@(PlainSubTag, (a, TypeConcVarWithArgs cxt b ([],[])))
        | k == b = (PlainSubTag, (b, TypeConcVarWithArgs cxt a mempty))
        | otherwise = t
    f t@(PlainSubTag, (a, TypeSeqVarWithArgs cxt b []))
        | k == b = (PlainSubTag, (b, TypeSeqVarWithArgs cxt a mempty))
        | otherwise = t
    f n = n

lookupSubList ::
    ( Eq (TypeP x) ) =>
    TypeP x -> 
    [TaggedSub x] ->
    Maybe (MplType x)
lookupSubList v = lookupOf (folded % to f) (PlainSubTag, v)
  where
    f (tag, (v', expr)) =  ((tag, v'), expr)


deleteSubList ::
    ( Eq (TypeP x) ) =>
    TypeP x -> 
    [TaggedSub x] ->
    [TaggedSub x] 
deleteSubList v = f
  where
    f (t@(PlainSubTag, (v', expr)) : rst) 
        | v == v' = rst
        | otherwise = t : f rst
    f (t@(StableSubTag, (v', expr)) : rst) 
        = t : f rst
    f [] = []


solveTypeEqns ::
    forall m e x.
    ( AsTypeUnificationError e x 
    , Ord (TypeP x) 
    , Eq (TypeP x) 
    , Eq (IdP x) 
    , Eq (MplType x)
    , MonadError e m ) => 
    TypeEqns x -> 
    m (Package x)
solveTypeEqns = cata f
  where
    f :: Base (TypeEqns x) (m (Package x)) -> m (Package x)
    f (TypeEqnsEqF (a,b)) = do  
        subs <- map (PlainSubTag,) <$> match a b
        return $ mempty & packageSubs .~ subs
    -- Note: no need for occurs checking! 
    -- These equations really just free variables that 
    -- are only substituted that match the corresponding
    -- type equation given.
    f (TypeEqnsEqStableF (a,b)) = 
        return $ mempty & packageSubs .~ [(StableSubTag, (a,b))]

    f (TypeEqnsExistF vs acc) = do
        acc' <- mconcat <$> sequenceA acc
        packageExistentialElim $ acc' 
            & packageExisVar %~ (Set.fromList vs `Set.union`)
            & packageSubs %~ id 
                -- something about simplifying 
                -- arrow types ehre?
    f (TypeEqnsForallF vs acc) = do
        acc' <- mconcat <$> sequenceA acc
        packageUniversalElim $ acc' 
            & packageUnivVar %~ (Set.fromList vs `Set.union`)
            & packageSubs %~ id 
                -- something about simplifying 
                -- arrow types ehre?
        
packageExistentialElim  ::
    forall e x m.
    ( AsTypeUnificationError e x 
    , Ord (TypeP x) 
    , Eq (TypeP x) 
    , Eq (IdP x)
    , Eq (MplType x)
    , MonadError e m ) => 
    Package x ->
    m (Package x)
packageExistentialElim pkg = 
    foldrM f pkg' (pkg ^. packageExisVar) >>= traverseOf
        packageSubs linearize
  where
    pkg' = pkg & packageExisVar .~ mempty
               & packageSubs %~ filter (not . isTrivialSub)

    f :: TypeP x -> Package x -> m (Package x)
    f v acc = let subs = acc ^. packageSubs % to (alignSubs v) in case lookupSubList v subs of
        Just sub -> do
            let subs' = deleteSubList v subs
            subs'' <- coalesce (v,sub) $ subs'
            let changed = subs'' == subs'
            return $ acc
                & packageSubs .~ bool ((PlainSubTag, (v, sub)):) id changed subs''
                & packageExisVar %~ bool (Set.singleton v `Set.union`) id changed 
        Nothing -> return $
            acc & packageExisVar %~ (Set.singleton v `Set.union`)

packageUniversalElim  ::
    ( AsTypeUnificationError e x 
    , Ord (TypeP x) 
    , Eq (TypeP x) 
    , Eq (IdP x)
    , Eq (MplType x)
    , MonadError e m ) => 
    Package x ->
    m (Package x)
packageUniversalElim pkg = 
    fmap (over packageSubs (filter (not . isTrivialSub)) )
    $ foldrM f (pkg & packageUnivVar .~ mempty) (pkg ^. packageUnivVar)
  where
    f v acc =  
        let subs = acc ^. packageSubs % to (alignSubs v) in
        case lookupSubList v subs of
            -- | TODO i think we need to check for the other trivial type variables too?
            -- e.g. -> A , A(), A( | )
            Just lkup@(TypeVar cxt v')
                -- | trivial substititions are okay!
                | v == v' -> return acc
                -- | if the variable is an existential, simplify the
                -- existential var first
                | v' `elem` pkg ^. packageExisVar -> 
                    packageExistentialElim $ acc & packageSubs %~ ((PlainSubTag, (v,lkup)):)
                -- | otherwise, its a forall match failure
                | otherwise -> 
                    throwError $ _TypeForallMatchFailure # (v, lkup)
            -- | otherwise, its a forall match failure
            Just err -> throwError $ _TypeForallMatchFailure # (v, err)
            -- | if it's not there, then we done ( we could propogate it up :) )
            Nothing -> return acc


