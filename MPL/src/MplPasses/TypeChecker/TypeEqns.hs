{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bool
import Data.List

import Data.Text.Prettyprint.Doc

import Debug.Trace

data TypeEqns x =
    TypeEqnsEq (MplType x, MplType x)
    | TypeEqnsExist [TypeP x] [TypeEqns x]
    | TypeEqnsForall [TypeP x] [TypeEqns x]

    -- used to recover the intermediate types..
    | TypeEqnsEqStable ( TypeP x, MplType x )

-- deriving instance ( ForallMplType Show x , Show (IdP x) ) => 
    -- Show (TypeEqns x)

instance ( PPrint (MplType x), PPrint (IdP x), PPrint (TypeP x) ) => PPrint (TypeEqns x) where
    pprint = show . f
      where
        f (TypeEqnsEq (a,b)) = hsep 
            [ pretty (pprint a) 
            , pretty "==" 
            , pretty (pprint b) ]
        f (TypeEqnsEqStable (a,b)) = hsep
            [ pretty (pprint a) 
            , pretty "=S=" 
            , pretty (pprint b) ]
        f (TypeEqnsExist exists rst) = vsep 
            [ nest 2 $ vsep 
                $ [ hsep [pretty "exists", pretty (map pprint exists), pretty "s.t."] ]
                    <> map f rst ]

        f (TypeEqnsForall forall rst) = vsep 
            [ nest 2 $ vsep 
                $ [ hsep [pretty "forall", pretty (map pprint forall), pretty "s.t."] ]
                    <> map f rst ]

instance ( PPrint (MplType x), PPrint (IdP x), PPrint (TypeP x) ) => Show (TypeEqns x) where
    show = pprint

deriving instance ( Eq (MplType x), ForallMplType Eq x , Eq (IdP x) ) => 
    Eq (TypeEqns x)

data TypeUnificationError x =
    TypeMatchFailure (MplType x) (MplType x)
    | TypeOccursCheck (TypeP x) (MplType x)
    | TypeForallMatchFailure (TypeP x, MplType x)

deriving instance ( Show (MplType x), ForallMplType Show x , Show (IdP x) ) => 
    Show (TypeUnificationError x)

data Package x = Package {
    _packageUnivVar :: Set (TypeP x)
    , _packageExisVar :: Set (TypeP x)
    , _packageSubs :: [(SubTag, ( TypeP x, MplType x ))]
}
-- deriving instance ( Show (MplType x), ForallMplType Show x , Show (IdP x) ) => Show (Package x)
instance ( PPrint (MplType x), PPrint (IdP x), PPrint (TypeP x) ) => Show (Package x) where
    show = pprint

instance ( PPrint (MplType x), PPrint (IdP x), PPrint (TypeP x) ) => PPrint (Package x) where
    pprint = show . f
      where
        f (Package univ exis subs) = vsep
            [ pretty "Package: "
            , pretty "univ: " <> pretty (map pprint $ Set.toList univ)
            , pretty "exis: " <> pretty (map pprint $ Set.toList exis)
            , pretty "subs: " <> pretty (map g subs)
            ]

        g (PlainSubTag, (tp, sub)) = show $ 
            pretty "(" 
            <> pretty (pprint tp) 
            <> pretty ","
            <> pretty (pprint sub)
            <> pretty ")"
        g (StableSubTag, (tp, sub)) = show $ 
            pretty "S(" 
            <> pretty (pprint tp) 
            <> pretty ","
            <> pretty (pprint sub)
            <> pretty ")"

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
    ( Eq (TypeP x) 
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
    ) =>
    (TypeP x, MplType x) -> 
    MplType x -> 
    MplType x
substitute sub  = fst . substituteDelta sub
    {- cata f
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
    -}

-- | Substitutes and returs a bool if
-- anything changed (useful in linearize..)
substituteDelta ::
    ( Eq (TypeP x)
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
    ) =>
    (TypeP x, MplType x) -> 
    MplType x -> 
    (MplType x, Bool)
-- substituteDelta (v, sub) tp = trace ( pprint v ++  " -vsub- " ++ pprint sub ++ " subtp "++ pprint tp ++ "  res: " ++ pprint (fst res))  res
-- substituteDelta (v, sub) = second getAny . cata f 
substituteDelta (v, sub) tp = res
  where
    res = 
        -- If it is a trivial substitution, then no change has occured...
        second (bool getAny (const False) (isTrivialSub (PlainSubTag, (v,sub))))
        . cata f $ tp

    f (TypeVarF cxt v') 
        | v == v' = (sub, Any True)
        | otherwise = (TypeVar cxt v', Any False)

    f (TypeSeqVarWithArgsF cxt v' []) 
        | v == v' = (sub, Any True)
        | otherwise = (TypeSeqVarWithArgs cxt v' mempty, Any False)
    f (TypeConcVarWithArgsF cxt v' ([],[])) 
        | v == v' = (sub, Any True)
        | otherwise = (TypeConcVarWithArgs cxt v' mempty, Any False)

    f n = (embed $ fmap fst n, foldMap snd n)


match :: 
    ( AsTypeUnificationError e x
    , MonadError e m
    , Eq (IdP x) 
    , Eq (TypeP x) 
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
    ) => 
    MplType x -> MplType x -> 
    m [(TypeP x, MplType x)]
match = f
  where
    f (TypeVar cxt0 a) n = fmap pure $ mkValidSub a n
    f n (TypeVar cxt1 b) = fmap pure $ mkValidSub b n

    f type0@(TypeSeqWithArgs cxt0 a args) type1@(TypeSeqWithArgs cxt1 b brgs) 
        | a == b && length args == length brgs =
            concat <$> traverse (uncurry f) (zip args brgs)
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)

    -- TypeSeqVarWithArgs !(XTypeSeqVarWithArgs x) (TypeP x) [MplType x]
        
    f type0@(TypeConcWithArgs _ a (args0, args1)) type1@(TypeConcWithArgs _ b (brgs0, brgs1)) 
        | a == b && length args0 == length brgs0 && length args1 == length brgs1 =
            concat <$> traverse (uncurry f) (zip (args0 <> args1) (brgs0 <> brgs1))
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)
    -- TypeConcVarWithArgs !(XTypeConcVarWithArgs x) (TypeP x) ([MplType x], [MplType x])

    f type0@(TypeConcWithArgs _ a args) type1@(TypeWithNoArgs _ b) 
        | a == b && has _Empty args = return mempty
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)
    f type0@(TypeSeqWithArgs _ a args) type1@(TypeWithNoArgs _ b) 
        | a == b && has _Empty args = return mempty
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)
    f type0@(TypeWithNoArgs _ a) type1@(TypeConcWithArgs _ b brgs)
        | a == b && has _Empty brgs = return mempty
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)
    f type0@(TypeWithNoArgs _ a) type1@(TypeSeqWithArgs _ b brgs) 
        | a == b && has _Empty brgs = return mempty
        | otherwise = throwError $  _TypeMatchFailure # (type0, type1)

    f type0@(TypeBuiltIn a) type1@(TypeBuiltIn b) = case (a,b) of
        (TypeIntF a, TypeIntF b) -> return []
        (TypeTopBotF a, TypeTopBotF b) -> return []

        (TypeNegF cxt0 a, TypeNegF cxt1 b) -> f a b

        (TypeGetF cxt0 seq0 conc0, TypeGetF cxt1 seq1 conc1) -> 
            concat <$> sequenceA [f seq0 seq1, f conc0 conc1]
        (TypePutF cxt0 seq0 conc0, TypePutF cxt1 seq1 conc1) -> 
            concat <$> sequenceA [f seq0 seq1, f conc0 conc1]

        (TypeParF cxt0 a0 b0, TypeParF cxt1 a1 b1) -> 
            (<>) <$> f a0 a1 <*> f b0 b1
        (TypeTensorF cxt0 a0 b0, TypeTensorF cxt1 a1 b1) -> 
            (<>) <$> f a0 a1 <*> f b0 b1

        (TypeSeqArrF cxt0 froms0 to0, TypeSeqArrF cxt1 froms1 to1) 
            | length froms0 == length froms1 -> 
                (<>) <$> (fold <$> traverse (uncurry f) (NE.zip froms0 froms1 ))
                     <*> f to0 to1
            | otherwise -> throwError $ _TypeMatchFailure # (type0,type1)
        (TypeConcArrF cxt0 seqs0 froms0 tos0, TypeConcArrF cxt1 seqs1 froms1 tos1 ) 
            | getAll $  foldMap All 
                [ length seqs0 == length seqs1
                , length froms0 == length froms1 
                , length tos0 == length tos1 
                ] ->
                    mconcat <$> traverse (fmap fold . traverse (uncurry f))
                    [ zip seqs0 seqs1
                    , zip froms0 froms1
                    , zip tos0 tos1
                    ]
            | otherwise -> throwError $ _TypeMatchFailure # (type0,type1)
        _ -> throwError $ _TypeMatchFailure # (type0,type1)
    f type0 type1 = throwError $ _TypeMatchFailure # (type0,type1)

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
    ( Eq (TypeP x) 
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
    ) => 
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
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
    , MonadError e m ) => 
    TypeP x -> 
    MplType x ->
    m (TypeP x, MplType x)
mkValidSub v exp = bool (pure (v, exp)) (throwError $ _TypeOccursCheck # (v,exp)) $ failsOccursCheck v exp

coalesce ::
    ( AsTypeUnificationError e x 
    , Eq (TypeP x)
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
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
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
    , MonadError e m ) => 
    [TaggedSub x] -> 
    m [TaggedSub x]
linearize = fmap fst . f . ([],)
  where
    f :: ([_], [_]) -> _ ([_], [_])
    f (subs, []) = pure (subs, [])
    f (subs, (StableSubTag, t):ts) = 
        f ((StableSubTag, t) : subs, ts)
    f (subs, (PlainSubTag, t) : ts) =  
        -- ((PlainSubTag, t) : g (subs, h subs),)
        ((PlainSubTag, t) : g (h subs),)
        <$> coalesce t 
            ( alignSubs (t ^. _1)
            . filter (not . isTrivialSub) $ ts)
        >>= f
      where
        -- | We need to back substitute to keep the
        -- system ``consistent".

        -- | substitute until there are no more changes.
        g (subs, delta)
            | delta = g $ h subs
            | otherwise = subs

        -- | substitute helper (note: it is not necessary
        -- to check for occurs check here because if an 
        -- occurs check existed, it would have already happened
        -- and not be back substituted)
        h = second (any id) 
            . unzip 
            . map ( over (_2 % _2) fst &&& view (_2 % _2 % _2)
                    <<< second (second (substituteDelta t)))

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
    , Eq (IdP x) 
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
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
    f (TypeEqnsEqStableF (a,b)) = do
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
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
    , MonadError e m ) => 
    Package x ->
    m (Package x)
packageExistentialElim pkg = do
    foldrM f pkg' (pkg ^. packageExisVar) >>= traverseOf
        packageSubs linearize
  where
    pkg' = pkg & packageExisVar .~ mempty
               & packageSubs %~ filter (not . isTrivialSub)

    f :: TypeP x -> Package x -> m (Package x)
    f v acc = let subs = acc ^. packageSubs % to (alignSubs v) in case lookupSubList v subs of
        Just sub -> do
            let subs' = deleteSubList v subs
            vsub <- mkValidSub v sub
            subs'' <- coalesce vsub $ subs'
            -- let changed = subs'' == subs'
            let changed = v `elem` foldMap collectvars subs'
            return $ acc
                & packageSubs .~ bool ((PlainSubTag, vsub):) id changed subs''
                & packageExisVar %~ bool (Set.singleton v `Set.union`) id changed 
        Nothing -> return $
            acc & packageExisVar %~ (Set.singleton v `Set.union`)

    collectvars (PlainSubTag, (tp, mpltp)) = tp : mplTypeCollectTypeP mpltp
    -- stable vars do not change the system at all...
    collectvars (StableSubTag, (tp, mpltp)) = []


packageUniversalElim  ::
    ( AsTypeUnificationError e x 
    , Ord (TypeP x) 
    , Eq (TypeP x) 
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x)
    , PPrint (IdP x)
    , PPrint (TypeP x)
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
                | v' `elem` pkg ^. packageExisVar -> do
                    nsub <- mkValidSub v lkup
                    packageExistentialElim $ acc & packageSubs %~ ((PlainSubTag, nsub):)
                -- | otherwise, its a forall match failure
                | otherwise -> 
                    throwError $ _TypeForallMatchFailure # (v, lkup)
            -- | otherwise, its a forall match failure
            Just err -> throwError $ _TypeForallMatchFailure # (v, err)
            -- | if it's not there, then we done ( we could propogate it up :) )
            Nothing -> return acc


