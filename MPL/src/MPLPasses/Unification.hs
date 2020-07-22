{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module MPLPasses.Unification where

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLAST.MPLPrinter
import MPLAST.MPLASTCore
import MPLPasses.UnificationErrors

import Control.Monad

import Optics
import Data.Bool
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Foldable
import Control.Monad.State

import Data.List
import Data.Set ( Set (..) )
import qualified Data.Set as Set 
import Data.Map ( Map (..) )
import qualified Data.Map as Map 

import Debug.Trace

data TypeEqns ident typevar =
    TypeEqnsEq (TypeGTypeVar ident typevar, TypeGTypeVar ident typevar)
    | TypeEqnsExist [typevar] [TypeEqns ident typevar]
    | TypeEqnsForall [typevar] [TypeEqns ident typevar]
  deriving (Show, Functor, Foldable, Traversable)

$(concat <$> traverse makeBaseFunctor 
    [ ''TypeEqns ]
 )
$(concat <$> traverse makePrisms 
    [ ''TypeEqns 
    ]
 )

substitutes ::
     [(TypeTag, TypeGTypeTag)] -> 
     TypeGTypeTag -> 
     TypeGTypeTag
substitutes subs ty = foldr substitute ty subs

substitute :: 
     (TypeTag, TypeGTypeTag) -> 
     TypeGTypeTag -> 
     TypeGTypeTag
substitute (v, sub) = cata f
  where
    f (TypeVarF ident args) 
        | v == ident = sub
        | otherwise = TypeVar ident args
    f n = embed n

-- converts the graph type to the tagged type
substitutesTypeGToTypeGTypeTag :: 
    [(TaggedBnfcIdent, TypeGTypeTag)] -> 
    TypeG TaggedBnfcIdent -> 
    Maybe (TypeGTypeTag)
substitutesTypeGToTypeGTypeTag subs typeg = cata f typeg 
  where
    f (TypeWithArgsF a call bs) = TypeWithArgs a call <$> sequenceA bs
    f (TypeVarF a []) = lookup a subs
        -- TODO - fix this! We need proper error handling of data with 
        -- the wrong kind -- for now, we completely ignore higher kinded
        -- data...
    f (TypeVarF a args) = trace "Warning: higher kinded data not supported yet!" (lookup a subs)
    f (TypeSeqF seq) = TypeSeq <$> sequenceA seq
    f (TypeConcF conc) = TypeConc <$> sequenceA conc

data Package ident typevar = Package {
     _packageUnivVar :: Set typevar
    , _packageExisVar :: Set typevar
    , _packageFreeVars :: Set typevar
    , _packageSubs :: [(typevar, TypeGTypeVar ident typevar)]
}  deriving (Show, Eq)

$(makeLenses ''Package)

type TagTypeMap = Map TypeTag TypeGTypeTag

packageToTagMap :: 
    Package TaggedBnfcIdent TypeTag -> 
    TagTypeMap
packageToTagMap pkg = Map.fromList (pkg ^. packageSubs)

instance (Show a, Show b, PPrint a, PPrint b) => PPrint (Package a b) where
    pprint (Package a b c d) =  
        "Package:\n" ++ 
        "\nUniversal\n" ++ show a ++
        "\nExistential\n" ++ show b ++
        "\nFree\n" ++ show c ++
        "\nSubs\n" ++ intercalate "\n" (map (pprint) d) ++ "\n\n"

emptyPackage :: Package ident typevar
emptyPackage = Package Set.empty Set.empty Set.empty []

instance Ord typevar => Semigroup (Package ident typevar) where
    Package univ0 exis0 free0 subs0 <> Package univ1 exis1 free1 subs1
        = Package (univ0 <> univ1) (exis0 <> exis1) (free0 <> free1) (subs0 <> subs1)

instance Ord typevar => Monoid (Package ident typevar) where
    mempty = emptyPackage


match ::
    AsUnificationError e =>
    TypeGTypeTag -> 
    TypeGTypeTag -> 
    Either e [(TypeTag, TypeGTypeTag)]
match = f
  where
    f t0@(TypeWithArgs ident0 _ args0) t1@(TypeWithArgs ident1 _ args1)
        | ident0 == ident1 && length args0 == length args1 = 
            concat <$> traverse (uncurry f) (zip args0 args1)
        | otherwise = Left $ _MatchFailure # (t0, t1)
    -- one is a type varaible (need symmetry as well)
    f (TypeVar a []) b = pure <$> mkValidSub a b
    f a (TypeVar b []) = pure <$> mkValidSub b a

    -- cases for higher kinded data
    f t0@(TypeVar a args) t1@(TypeVar b brgs) 
        | length args /= length brgs = Left $ _MatchFailure # (t0, t1)
        | otherwise = 
            (:) <$> mkValidSub a (TypeVar b []) 
            <*> (concat <$> traverse (uncurry f) (zip args brgs))
    f t0@(TypeVar a args) t1@(TypeWithArgs b cxt brgs) 
        | length args /= length brgs = Left $ _MatchFailure # (t0, t1)
        | otherwise = 
            (:) <$> mkValidSub a (TypeWithArgs b cxt []) 
            <*> (concat <$> traverse (uncurry f) (zip args brgs))
    -- perhaps in the future, we need to add more cases for
    -- passing built in types in a higher order fashion...
    -- Although, it will be strange because of the changing sequential types
    -- and the fact that it literally can't parse passing a tensor / par in 
    -- a higher kinded way. This would require some serious changing of
    -- how this system handles operators..

    f (TypeSeq a) (TypeSeq b) = case (a,b) of
        (TypeIntF _, TypeIntF _) -> pure []
        (TypeCharF _, TypeCharF _) -> pure []
        (TypeDoubleF _, TypeDoubleF _) -> pure []
        (TypeStringF _, TypeStringF _) -> pure []
        (TypeUnitF _, TypeUnitF _) -> pure []
        (TypeBoolF, TypeBoolF) -> pure []
        (TypeListF a, TypeListF b) -> f a b
        (TypeSeqArrF froms0 to0, TypeSeqArrF froms1 to1) 
            | length froms0 == length froms1 -> mappend 
                    <$> (concat <$> traverse (uncurry f) (zip froms0 froms1))
                    <*> f to0 to1
            | otherwise -> Left $ _MatchFailure # (TypeSeq a, TypeSeq b)
        _ -> Left $ _MatchFailure # (TypeSeq a, TypeSeq b)

    f (TypeConc a) (TypeConc b) = case (a,b) of
        (TypeGetF _ l0 r0, TypeGetF _ l1 r1 ) -> 
            mappend <$> f l0 r0 <*> f l1 r1
        (TypePutF _ l0 r0, TypePutF _ l1 r1 ) -> 
            mappend <$> f l0 r0 <*> f l1 r1
        (TypeTensorF _ l0 r0, TypeTensorF _ l1 r1 ) -> 
            mappend <$> f l0 r0 <*> f l1 r1
        (TypeParF _ l0 r0, TypeParF _ l1 r1 ) -> 
            mappend <$> f l0 r0 <*> f l1 r1
        (TypeTopBotF _, TypeTopBotF _) -> pure []
        (TypeNegF _ s, TypeNegF _ t) -> f s t
        _ -> Left $ _MatchFailure # (TypeConc a, TypeConc b)

    f a b = Left $ _MatchFailure # (a, b)

failsOccursCheck ::
    TypeTag -> 
    TypeGTypeTag ->
    Bool
failsOccursCheck _ (TypeVar _ []) = False
failsOccursCheck n tp = n `elem` toList tp

mkValidSub :: 
    AsUnificationError e =>
    TypeTag ->
    TypeGTypeTag ->
    Either e (TypeTag, TypeGTypeTag) 
mkValidSub v exp = 
    if failsOccursCheck v exp 
        then Left $ _OccursCheck # (v, exp)
        else pure $ (v, exp)

coalesce :: 
    AsUnificationError e => 
    (TypeTag, TypeGTypeTag) ->
    [(TypeTag, TypeGTypeTag)] -> 
    Either e [(TypeTag, TypeGTypeTag)]
coalesce _ [] = pure []
coalesce (s, ssub) ((t, tsub):rst) 
    | s == t = mappend <$> match ssub tsub <*> coalesce (s, ssub) rst
    | otherwise = (:) 
        <$> mkValidSub t (substitute (s,ssub) tsub)
        <*> coalesce (s, ssub) rst

linearize :: 
    AsUnificationError e => 
    [(TypeTag, TypeGTypeTag)] -> 
    Either e [(TypeTag, TypeGTypeTag)]
linearize [] = pure []
linearize (a:as) = (:) a <$> (coalesce a as >>= linearize)
    -- not totally correct! does not substitute back to keep
    -- the system consistent..

solveTypeEq ::
    AsUnificationError e => 
    TypeEqns TaggedBnfcIdent TypeTag ->
    Either e (Package TaggedBnfcIdent TypeTag)
solveTypeEq = cata f
  where
    f :: AsUnificationError e => 
        TypeEqnsF
            TaggedBnfcIdent 
            TypeTag (Either e (Package TaggedBnfcIdent TypeTag)) ->
        Either e (Package TaggedBnfcIdent TypeTag)

    f (TypeEqnsEqF (a, b)) = do 
        let freevars = Set.fromList $ toList a ++ toList b
        subs <- match a b
        return $ emptyPackage 
            & packageSubs .~ subs
            & packageFreeVars .~ freevars

    f (TypeEqnsExistF vs acc) = do
        acc' <- sequenceA acc
        let pkg = mconcat acc' & packageExisVar %~ (Set.fromList vs `Set.union`)
        packageExistentialElim pkg

    f (TypeEqnsForallF vs acc) = do
        acc' <- sequenceA acc
        let pkg = mconcat acc' & packageUnivVar %~ (Set.fromList vs `Set.union`)
        packageUniversalElim pkg

isTrivialSubstitution :: (TypeTag, TypeGTypeTag) -> Bool
isTrivialSubstitution (s, TypeVar t []) = s == t
isTrivialSubstitution _ = False

-- prashant does both elimatino of both existential and forall at the same time?
packageExistentialElim ::
    AsUnificationError e => 
    Package TaggedBnfcIdent TypeTag -> 
    Either e (Package TaggedBnfcIdent TypeTag)
packageExistentialElim pkg = do
    pkg' <- foldrM f 
            (pkg & packageExisVar .~ Set.empty & packageSubs %~ filter (not . isTrivialSubstitution)) 
            (pkg ^. packageExisVar)
    return $ pkg' 
        & packageFreeVars %~ (`Set.difference` (pkg ^. packageExisVar))
  where
    f :: AsUnificationError e => 
        TypeTag -> 
        Package TaggedBnfcIdent TypeTag -> 
        Either e (Package TaggedBnfcIdent TypeTag)
    f v acc = let subs = alignSubs v $ acc ^. packageSubs in case find ((== v) . fst) subs of
        Just sub -> do 
            subs' <- coalesce sub $ deleteBy (\v n -> fst v == fst n) (v, TypeVar v []) subs
                -- some stupid stuff just to get deleteBy to type check...
            subs'' <- linearize subs'
            return $ acc & packageSubs .~ subs''
        Nothing -> return $ acc & packageExisVar %~ (Set.singleton v `Set.union`)


packageUniversalElim :: 
    AsUnificationError e => 
    Package TaggedBnfcIdent TypeTag -> 
    Either e (Package TaggedBnfcIdent TypeTag)
packageUniversalElim pkg = do
    pkg' <- foldrM f 
            (pkg & packageUnivVar .~ Set.empty)
            (pkg ^. packageUnivVar)
    return $ pkg' 
        & packageFreeVars %~ (`Set.difference` (pkg ^. packageExisVar))
        & packageSubs %~ filter (not . isTrivialSubstitution)
  where
    f v acc = let subs = alignSubs v $ acc ^. packageSubs in case filter ((==v) . fst) $ subs of
        [] -> return $ acc & packageUnivVar %~ (Set.singleton v `Set.union`)
        as -> let nontrivsubs = (filter (not . isTrivialSubstitution) as) in if null nontrivsubs
                then return acc
                else Left $ _ForallMatchFailure # nontrivsubs

alignSubs :: TypeTag -> [(TypeTag, TypeGTypeTag)] -> [(TypeTag, TypeGTypeTag)]
alignSubs k = map g
  where
    g (a, TypeVar b []) 
        | k == b = (b, TypeVar a [])
        | otherwise = (a, TypeVar b [])
    g n = n 
