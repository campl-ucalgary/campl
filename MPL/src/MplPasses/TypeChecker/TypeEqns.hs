{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
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


import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Bool
import Data.List
import Data.Tuple

import Data.Proxy

import Data.Text.Prettyprint.Doc

import Debug.Trace

import MplPasses.PassesErrorsPprint

{- Module for type equations and solving them (i.e., a variant of the unification algorithm)
 -
 - This is a mostly standard unification algorithm.
 -
 - Some differences. 
 -      - We don't get rid of trivial type variables (since we would otherwise just 
 -          replace them with trivial variables when looking up the type in the end.. 
 -              so we just dont' get rid of them. TODO: remove this)
 -
 - For future work, it might be honestly best to replace this with the faster mutuable
 - algorithm that does substitution in O(1) time and simply just must do matching traversals.
 - This would also simplify the forall checking as well.
 -}

data TypeEqns x =
    TypeEqnsEq (MplType x, MplType x)
    | TypeEqnsExist [TypeP x] [TypeEqns x]
    | TypeEqnsForall [([TypeP x], TypeP x, MplType x)] [TypeEqns x]

-- deriving instance ( ForallMplType Show x , Show (IdP x) ) => 
    -- Show (TypeEqns x)

instance ( PPrint (MplType x) y, PPrint (IdP x) y, PPrint (TypeP x) y) => PPrint (TypeEqns x) y where
    pprint proxy = show . f
      where
        f (TypeEqnsEq (a,b)) = hsep 
            [ pretty (pprint proxy a) 
            , pretty "==" 
            , pretty (pprint proxy b) ]
        f (TypeEqnsExist exists rst) = vsep 
            [ nest 2 $ vsep 
                $ [ hsep [pretty "exists", pretty (map (pprint proxy) exists), pretty "s.t."] ]
                    <> map f rst ]

        f (TypeEqnsForall forall rst) = vsep 
            [ nest 2 $ vsep 
                $ [ hsep [pretty "forall", pretty (map (pprint proxy . view _2) forall), pretty "s.t."] ]
                    <> map f rst ]

instance ( PPrint (MplType x) MplRenamed, PPrint (IdP x) MplRenamed, PPrint (TypeP x) MplRenamed) => Show (TypeEqns x) where
    show = pprint (Proxy :: Proxy MplRenamed)

deriving instance ( Eq (MplType x), ForallMplType Eq x , Eq (IdP x) ) => 
    Eq (TypeEqns x)

data TypeUnificationError x 
    = TypeMatchFailure (MplType x) (MplType x)
    | TypeOccursCheck (TypeP x) (MplType x)
    | TypeForallMatchFailure (MplType x) (MplType x)
        -- ^ given type, inferred type

deriving instance ( Show (MplType x), ForallMplType Show x , Show (IdP x) ) => 
    Show (TypeUnificationError x)

data Package x = Package {
    -- _packageUnivVar :: Set (TypeP x)
    _packageExisVar :: Set (TypeP x)
    , _packageSubs :: [( TypeP x, MplType x)]
}
-- deriving instance ( Show (MplType x), ForallMplType Show x , Show (IdP x) ) => Show (Package x)
instance ( PPrint (MplType x) MplRenamed, PPrint (IdP x) MplRenamed, PPrint (TypeP x) MplRenamed) => Show (Package x) where
    show = pprint (Proxy :: Proxy MplRenamed)

instance ( PPrint (MplType x) y, PPrint (IdP x) y, PPrint (TypeP x) y) => PPrint (Package x) y where
    pprint proxy = show . f
      where
        f (Package {- univ -} exis subs) = vsep
            [ pretty "Package: "
            -- , pretty "univ: " <> pretty (map pprint $ Set.toList univ)
            , pretty "exis: " <> pretty (map (pprint proxy) $ Set.toList exis)
            , pretty "subs: " <> pretty (map g subs)
            ]

        g (tp, sub) = show $ 
            pretty "(" 
            <> pretty (pprint proxy tp) 
            <> pretty ","
            <> pretty (pprint proxy sub)
            <> pretty ")"


type Sub x = (TypeP x, MplType x)

instance Ord (TypeP x) => Semigroup (Package x) where
    Package {- a0 -} b0 c0 <> Package {- a1 -} b1 c1 = 
        Package {- (a0 <> a1) -} (b0 <> b1) (c0 <> c1)

instance Ord (TypeP x) => Monoid (Package x) where
    mempty = Package {- mempty -} mempty mempty

$(makeClassyPrisms ''TypeUnificationError)
$(makePrisms ''TypeEqns)
$(makeBaseFunctor ''TypeEqns)
$(makeLenses ''Package)

substitute ::
    ( Eq (TypeP x) 
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed
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
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed
    ) =>
    (TypeP x, MplType x) -> 
    MplType x -> 
    (MplType x, Bool)
substituteDelta (v, sub) tp = res
  where
    res = 
        -- If it is a trivial substitution, then no change has occured...
        second (bool getAny (const False) (isTrivialSub ((v,sub))))
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



matchCont :: 
    ( AsTypeUnificationError e x
    , MonadError e m
    , Eq (IdP x) 
    , Eq (TypeP x) 
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)
    ) =>  
    MplType x -> 
    MplType x -> 
    (MplType x -> MplType x -> m [(TypeP x, MplType x)]) -> 
    m [(TypeP x, MplType x)]
matchCont ty0 ty1 k = f ty0 ty1
  where
    -- need to simplify double negations first.
    f (TypeBuiltIn (TypeNegF _ (TypeBuiltIn (TypeNegF _ a)))) b 
        = f a b
    f a (TypeBuiltIn (TypeNegF _ (TypeBuiltIn (TypeNegF _ b)))) 
        = f a b

    f (TypeVar cxt0 a) n = fmap pure $ mkValidSub a n
    f n (TypeVar cxt1 b) = fmap pure $ mkValidSub b n

    f type0@(TypeSeqWithArgs _cxt0 a args) type1@(TypeSeqWithArgs _cxt1 b brgs) 
        | a == b && length args == length brgs =
            concat <$> traverse (uncurry f) (zip args brgs)
        | otherwise = k type0 type1

    f type0@(TypeConcWithArgs _ a (args0, args1)) type1@(TypeConcWithArgs _ b (brgs0, brgs1)) 
        | a == b && length args0 == length brgs0 && length args1 == length brgs1 =
            concat <$> traverse (uncurry f) (zip (args0 <> args1) (brgs0 <> brgs1))
        | otherwise = k type0 type1
    -- TypeConcVarWithArgs !(XTypeConcVarWithArgs x) (TypeP x) ([MplType x], [MplType x])

    -- TODO: Actually, I think there's a bug here... 
    -- We really should check if we are matching concurrent types with 
    -- concurrent type with no args (and vice versa) Although, I think this is handled
    -- in the renaming stage. BUT! This is probably caught in kind checking 
    -- I'm guessing.
    f type0@(TypeWithNoArgs _ a) type1@(TypeWithNoArgs _ b) 
        | a == b = return mempty
        | otherwise = k type0 type1
    f type0@(TypeConcWithArgs _ a args) type1@(TypeWithNoArgs _ b) 
        | a == b && has _Empty args = return mempty
        | otherwise = k type0 type1
    f type0@(TypeSeqWithArgs _ a args) type1@(TypeWithNoArgs _ b) 
        | a == b && has _Empty args = return mempty
        | otherwise = k type0 type1
    f type0@(TypeWithNoArgs _ a) type1@(TypeConcWithArgs _ b brgs)
        | a == b && has _Empty brgs = return mempty
        | otherwise = k type0 type1
    f type0@(TypeWithNoArgs _ a) type1@(TypeSeqWithArgs _ b brgs) 
        | a == b && has _Empty brgs = return mempty
        | otherwise = k type0 type1

    f type0@(TypeBuiltIn a) type1@(TypeBuiltIn b) = case (a,b) of
        (TypeIntF _a, TypeIntF _b) -> return []
        (TypeDoubleF _a, TypeDoubleF _b) -> return []
        (TypeCharF _a, TypeCharF _b) -> return []

        (TypeTopBotF a, TypeTopBotF b) -> return []


        (TypeNegF cxt0 a, TypeNegF cxt1 b) -> f a b

        (TypeTupleF _cxt0 (l0, l1, ls), TypeTupleF _cxt1 (r0, r1, rs)) 
            | length ls == length rs -> 
                fmap concat $ mappend 
                    <$> sequenceA [ f l0 r0 , f l1 r1 ] 
                    <*> traverse (uncurry f) (zip ls rs)

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
            | otherwise -> k type0 type1
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
            | otherwise -> k type0 type1
        _ -> k type0 type1
    f type0 type1 = k type0 type1

{- | This is literally matching as you would expect from a unification algorithm -}
match ::
    ( AsTypeUnificationError e x
    , MonadError e m
    , Eq (IdP x) 
    , Eq (TypeP x) 
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)
    ) =>  
    MplType x -> 
    MplType x -> 
    m [(TypeP x, MplType x)] 
match ty0 ty1 = matchCont ty0 ty1 k
  where
    k type0 type1 = (throwError $ _TypeMatchFailure # (type0,type1))

{- | This matches but cuts some slack for @Int@ and @Double@-}
matchNumNudge ::
    ( AsTypeUnificationError e x
    , MonadError e m
    , Eq (IdP x) 
    , Eq (TypeP x) 
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)
    ) =>  
    MplType x -> 
    MplType x -> 
    m [(TypeP x, MplType x)] 
matchNumNudge ty0 ty1 = matchCont ty0 ty1 k
  where
    k type0@(TypeBuiltIn a) type1@(TypeBuiltIn b) = case (a,b) of
        (TypeIntF a, TypeIntF b) -> return []
        (TypeDoubleF a, TypeDoubleF b) -> return []
        _ -> throwError $ _TypeMatchFailure # (type0,type1)
    k type0 type1 = throwError $ _TypeMatchFailure # (type0,type1)

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
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed
    ) => 
    TypeP x ->
    MplType x ->
    Bool
failsOccursCheck _ (TypeVar _ _) = False
failsOccursCheck _ (TypeSeqVarWithArgs _ _ []) = False
failsOccursCheck _ (TypeConcVarWithArgs _ _ ([],[])) = False
failsOccursCheck n rst = n `elem` mplTypeCollectTypeP rst


{- | Makes a valid substitution (i.e., it tests for occurs check) -}
mkValidSub ::
    ( AsTypeUnificationError e x 
    , Eq (TypeP x)
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed
    , MonadError e m ) => 
    TypeP x -> 
    MplType x ->
    m (TypeP x, MplType x)
mkValidSub v exp = bool (pure (v, exp)) (throwError $ _TypeOccursCheck # (v,exp)) $ failsOccursCheck v exp

{- | this takes a substitution and a list of substitutions and will
essentially eliminate all occurences of that substitution (if possible)
-}
coalesce ::
    ( AsTypeUnificationError e x 
    , Eq (TypeP x)
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)

    , MonadError e m ) => 
    Sub x -> 
    [Sub x] ->
    m [Sub x]
coalesce (s, ssub) = fmap concat . traverse go
  where
    go ((t, tsub)) 
        | s == t = match ssub tsub
        | otherwise = pure <$> mkValidSub t (substitute (s, ssub) tsub)

{-| this is the same as coalesce but in the case we have subsitutions like: @A = Int, A = Double@,
this will (informally) /nudge/ @A@ to be of type @Double@.
In other words, this follows the convention that @Int <= Double@ i.e., @Double@ is the bigger type than @Int@, so we probably mean to implicitly cast the @Int@ to the @Double@.

Input: A substitution
Output: The potentially /nudged/ substitution, and the rest of the substitutions.
 -}
coalesceNumNudge ::
    forall e x m.
    ( AsTypeUnificationError e x 
    , Eq (TypeP x)
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)

    -- we require that these annotations should be the same
    , XTypeDoubleF x ~ XTypeIntF x

    , MonadError e m ) => 
    Sub x -> 
    [Sub x] ->
    m ([Sub x], Sub x)
coalesceNumNudge (s, ssub) = flip runStateT (s,ssub) . go
  where
    go :: [(TypeP x, MplType x)] -> StateT (TypeP x, MplType x) m [(TypeP x, MplType x)]
    go [] = return []
    go ((t, tsub):rst) = do
        (s, ssub) <- get
        if s == t 
            then case (ssub, tsub) of 
                (TypeBuiltIn (TypeIntF a), TypeBuiltIn (TypeDoubleF _)) -> do
                    equality .= (s, TypeBuiltIn (TypeDoubleF a))
                    (:) <$> mkValidSub t tsub <*> go rst
                (TypeBuiltIn (TypeDoubleF _ ), TypeBuiltIn (TypeIntF b)) -> do
                    (:) <$> mkValidSub t (TypeBuiltIn (TypeDoubleF b)) <*> go rst
                _ -> mappend <$> match ssub tsub <*> go rst
            else (:) <$> mkValidSub t (substitute (s, ssub) tsub) <*> go rst

{-| linearize.  This is unification for a set of constraints essentially. -}
linearize ::
    ( AsTypeUnificationError e x 
    , Eq (TypeP x) 
    , Ord (TypeP x) 
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed
    , ForallMplType Show x 
    , Show (IdP x)

    -- we require that these annotations should be the same
    , XTypeDoubleF x ~ XTypeIntF x

    , MonadError e m ) => 
    [Sub x] -> 
    m [Sub x]
linearize = go
  where
    go [] = pure []
    go (t:ts) = do
        -- ts' <- coalesce t (alignSubs (t ^. _1) ts)
        (ts', nt) <- coalesceNumNudge t (alignSubs (t ^. _1) ts)
        ts'' <- go ts' 
        return $ foldr (\t' -> second (substitute t')) nt ts'' : ts''

{- | computes the variable closure of a variable e.g. the variable closure of @1@ and @[(1,2), (2,3), (4,3)]@ is @1,2,3,4@-}
variableClosure ::
    ( Eq (TypeP x) ) =>
    TypeP x -> 
    [Sub x] -> 
    [TypeP x]
variableClosure tp sub = go tp [tp] 
  where
    go tp visited = case lookup tp $ alignSubs tp sub of
        Just (TypeVar cxt tp') | tp' `notElem` visited -> go tp' (tp' : visited)
        _ -> visited

{- | returns true if the substitution is trivial i.e., a = a -}
isTrivialSub ::
    ( Eq (TypeP x) ) => 
    Sub x ->
    Bool
isTrivialSub (s, expr) = case expr of
    TypeVar _ t -> s == t
    TypeSeqVarWithArgs _ t [] -> s == t
    TypeConcVarWithArgs _ t ([],[]) -> s == t
    _ -> False

{- | align the substitutions according to a type. -}
alignSubs ::    
    ( Eq (TypeP x) ) =>
    TypeP x ->
    [Sub x] ->
    [Sub x]
alignSubs k = map f
  where
    f t@(a, TypeVar cxt b)
        | k == b = (b, TypeVar cxt a)
        | otherwise = t
    f t@(a, TypeConcVarWithArgs cxt b ([],[]))
        | k == b = (b, TypeConcVarWithArgs cxt a mempty)
        | otherwise = t
    f t@(a, TypeSeqVarWithArgs cxt b [])
        | k == b = (b, TypeSeqVarWithArgs cxt a mempty)
        | otherwise = t
    f n = n

{- | look ups a type in the substitution list -}
lookupSubList ::
    ( Eq (TypeP x) ) =>
    TypeP x -> 
    [Sub x] ->
    Maybe (MplType x)
lookupSubList v = lookupOf (folded % to f)  v
  where
    f (v', expr) =  (v', expr)

{- | deletes a type within the substitution list -}
deleteSubList ::
    ( Eq (TypeP x) ) =>
    TypeP x -> 
    [Sub x] ->
    [Sub x] 
deleteSubList v = f
  where
    f (t@(v', expr) : rst) 
        | v == v' = rst
        | otherwise = t : f rst
    f [] = []

{- | solves the type equations (essentially the unification algorithm) -}
solveTypeEqns ::
    forall m e x.
    ( AsTypeUnificationError e x 
    , Ord (TypeP x) 
    , Eq (IdP x) 
    , Show (TypeP x) 

    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    -- we require that these annotations should be the same
    , XTypeDoubleF x ~ XTypeIntF x

    , ForallMplType Show x 
    , Show (IdP x)
    , MonadError e m ) => 
    TypeEqns x -> 
    m (Package x)
-- solveTypeEqns eqns = cata f eqns >>= traverseOf packageSubs linearize 
solveTypeEqns eqns = do
    traceShowM eqns
    cata f eqns >>= traverseOf packageSubs linearize
  where
    f :: Base (TypeEqns x) (m (Package x)) -> m (Package x)
    f (TypeEqnsEqF (a,b)) = do  
        subs <- matchNumNudge a b
        return $ mempty & packageSubs .~ subs
    f (TypeEqnsExistF vs acc) = do
        acc' <- mconcat <$> sequenceA acc
        packageExistentialElim $ acc' 
            & packageExisVar %~ (Set.fromList vs `Set.union`)
            & packageSubs %~ id 
                -- something about simplifying 
                -- arrow types ehre?
    f (TypeEqnsForallF vs acc) = do
        acc' <- mconcat <$> sequenceA acc
        packageUniversalElim vs acc'

{- | surface subs. This isn't used -- but the idea was that if there is a substitution in a list, we want that substitution to appear in the final result. e.g., @A=B@, @C=B@ could have @C@ showing up in other substitutions, but in reality, we want @A@ to be in the other substitutions. (this depends on the order of how things are unified / linearized) -}
surfaceSubs :: 
    forall x.
    ( Ord (TypeP x) 
    , Eq (IdP x) 
    , Show (TypeP x) 

    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)) => 
    [TypeP x] -> 
    Package x ->
    Package x 
surfaceSubs surface pkg = foldr go pkg surface
  where
    go s pkg = case lookup s $ alignSubs s $ pkg ^. packageSubs of
        Just (TypeVar cxt s') -> pkg & packageSubs %~ map (second (substitute (s', TypeVar cxt s)))
        _ -> pkg 
        
packageExistentialElim  ::
    forall e x m.
    ( AsTypeUnificationError e x 
    , Ord (TypeP x) 
    , Eq (TypeP x) 
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)

    -- we require that these annotations should be the same
    , XTypeDoubleF x ~ XTypeIntF x

    , MonadError e m ) => 
    Package x ->
    m (Package x)
packageExistentialElim pkg = do
    foldrM f pkg' (pkg ^. packageExisVar) >>= traverseOf
        packageSubs linearize
  where
    pkg' = pkg & packageExisVar .~ mempty
               -- & packageSubs %~ filter (not . isTrivialSub)
               & packageSubs %~ id

    f :: TypeP x -> Package x -> m (Package x)
    f v acc = let subs = acc ^. packageSubs % to (alignSubs v) in case lookupSubList v subs of
        Just sub -> do
            let subs' = deleteSubList v subs
            vsub <- mkValidSub v sub
            subs'' <- fmap (uncurry $ flip (:)) 
                (coalesceNumNudge vsub subs') >>= linearize
            let changed = v `elem` foldMap collectvars subs'
            return $ acc
                & packageSubs .~ bool (vsub:) id changed subs''
                & packageExisVar %~ bool (Set.singleton v `Set.union`) id changed 
        Nothing -> return $
            acc & packageExisVar %~ (Set.singleton v `Set.union`)

    collectvars (tp, mpltp) = tp : mplTypeCollectTypeP mpltp

packageUniversalElim  ::
    forall e m x.
    ( AsTypeUnificationError e x 
    , Ord (TypeP x) 
    , Eq (TypeP x) 
    , Eq (IdP x)
    , Show (TypeP x) 
    , PPrint (MplType x) MplRenamed
    , PPrint (IdP x) MplRenamed
    , PPrint (TypeP x) MplRenamed

    , ForallMplType Show x 
    , Show (IdP x)

    -- we require that these annotations should be the same
    , XTypeDoubleF x ~ XTypeIntF x

    , MonadError e m ) => 
    [([TypeP x], TypeP x, MplType x)] ->
    Package x ->
    m (Package x)
packageUniversalElim vs pkg = traverseOf packageSubs linearize pkg >>= flip (foldrM f) vs
  where
    f :: ([TypeP x], TypeP x, MplType x) -> Package x -> m (Package x)
    f (foralls, v, vtp) acc = acc' ^. packageSubs % to (pure . fromJust . lookup v) >>= \tp -> do
            let throwerr = throwError $ _TypeForallMatchFailure # (vtp, tp) 

                g :: [(TypeP x, MplType x)] -> m ()
                g [] = return ()
                g ((l, r@(TypeVar rcxt r')):rst)
                    -- trivial substitutions are okay or any matching to a non for all
                    | isTrivialSub (l, r) = g rst
                    | l `notElem` foralls && r' `notElem` foralls = g rst
                    | l `elem` foralls && r' `notElem` foralls = coalesce (r', TypeVar rcxt l) rst >>= g
                    | r' `elem` foralls && l `notElem` foralls = g ((r', (TypeVar rcxt l)):rst)
                    | otherwise = throwerr 
                g ((l, r):rst)
                    | l `elem` foralls = throwerr 
                    | otherwise = g rst

            (match vtp tp `catchError` const 
                (throwError $ _TypeForallMatchFailure # (vtp, tp))) 
                >>= linearize 
                >>= g 
            return $ acc' & packageSubs %~ ((v,vtp):) . deleteSubList v
      where
        acc' = acc & packageSubs %~ alignSubs v

{- idea is roughly as follows.
 - We need to find a substitution \sigma so that
 -  \sigma(inferred type) = user provided type
 - i.e.,
 -  the user provided type is /general/ enough to informally fit within the inferred type.
 -
 - We do this by finding the most general unifier, and then testing if the 
 -  most general unifier is such a \sigma i.e.,  each for all quantified type
 -      - must map to at most one non for all type variable 
 -          (otherwise we'd contradict the fact that the substitution sigma is a 
 -          substitution)
 - In plain english, if @a@ is a for all quantified type, we have:
 -      - if @a@ maps to some variable @x@
 -          - if @x@ is @a@, this is allowed
 -          - if @x@ is not @a@, then we must check to see if @x@ maps to 
 -                 something else which would lead to a failure given by these cases
 -                 (there is some nuances with @a = x@, @b = x@ that must be accounted for)
 -      - if @a@ maps to some type @X@
 -          - fail
 -}

{- | TODO: The whole annotation business needs to be cleaned up, and it should be actually used for error messages -}
pprintTypeUnificationError ::
    -- TypeUnificationError x -> 
    TypeUnificationError MplTypeSub -> 
    MplDoc
pprintTypeUnificationError = go
  where
    -- go :: TypeUnificationError x -> MplDoc
    go :: TypeUnificationError MplTypeSub -> MplDoc
    go ty = case ty of 
        TypeMatchFailure tp0 tp1 -> fold
            [ pretty "Match failure with types"
            , codeblock
                $ pprintParsed tp0
            , pretty "and"
            , codeblock
                $ pprintParsed tp1
            ]
        TypeOccursCheck tpp tp0 -> fold
            [ pretty "Occurs check failure with"
            , codeblock
                $ pprintParsed $ typeIdentTToTypeT tpp
            , pretty "and"
            , codeblock
                $ pprintParsed tp0
            ]
        TypeForallMatchFailure given inferred -> fold
            [ pretty "Could not match user provided type with inferred type. The given type was"
            , codeblock
                $ pprintParsed given
            , pretty "and this could not be match with the inferred type"
            , codeblock
                $ pprintParsed inferred
            ]
