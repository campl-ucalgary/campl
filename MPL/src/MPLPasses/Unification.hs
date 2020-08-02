{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module MPLPasses.Unification where

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLAST.MPLPrinter
import MPLAST.MPLASTCore
import MPLPasses.UnificationErrors

import Control.Monad
import Control.Arrow

import GHC.Generics hiding (to)

import Optics
import Data.Bool
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Foldable
import Control.Monad.State
import Text.PrettyPrint hiding ((<>))

import Data.List
import Data.Maybe
import Data.Set ( Set (..) )
import qualified Data.Set as Set 
import Data.Map ( Map (..) )
import qualified Data.Map as Map 
import Data.Foldable

import Debug.Trace

data TypeEqns ident typevar =
    TypeEqnsEq (TypeGTypeVar ident typevar, TypeGTypeVar ident typevar)
    | TypeEqnsExist [typevar] [TypeEqns ident typevar]
    | TypeEqnsForall [typevar] [TypeEqns ident typevar]

    -- used to recover the intermediate types..
    | TypeEqnsStableRef (typevar, TypeGTypeVar ident typevar)
  deriving (Show, Functor, Foldable, Traversable, Generic)

type Substitution typevar ident = SubstitutionTypes (typevar, ident)

data SubstitutionTypes a = 
    PlainSub { _substitution :: a }
    | StableSub { _substitution :: a }
  deriving (Show, Eq, Functor)

$(makePrisms ''SubstitutionTypes)
$(makeLenses ''SubstitutionTypes)

instance (PPrint a) => PPrint (SubstitutionTypes a) where
    pprint (PlainSub a) =  pprint a
    pprint (StableSub a) =  "Stable[" ++ pprint a ++ "]"

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (TypeEqns ident typevar) where
    pprint = render . f
      where
        f (TypeEqnsEq a) = text $ pprint a
        f (TypeEqnsStableRef a) = hcat [text "Stable", text $ pprint a]
        f (TypeEqnsExist typevars eqns) = 
            hcat
            [ text "Exist " 
            , text "" 
            , text ("[" ++ intercalate "," (map pprint typevars) ++ "]")
            , text " . " 
            , nest 2 (vcat (map f eqns))
            ]
        f (TypeEqnsForall typevars eqns) = 
            hcat
            [ text "Forall  " 
            , text "" 
            , text ("[" ++ intercalate "," (map pprint typevars) ++ "]")
            , text " . " 
            , nest 2 (vcat (map f eqns))
            ]

$(concat <$> traverse makeBaseFunctor 
    [ ''TypeEqns ]
 )
$(concat <$> traverse makePrisms 
    [ ''TypeEqns 
    ]
 )


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
substitutesTypeGToTypeGTypeTag subs = cata f 
  where
    f (TypeWithArgsF a call bs) = TypeWithArgs a call <$> sequenceA bs

    f (TypeVarF a []) = lookup a subs
    -- f (TypeVarF a args) = trace "Warning: higher kinded data not supported yet!" (lookup a subs)
    f (TypeVarF a args) = case lookup a subs of
        Just (TypeVar a []) -> TypeVar a <$> sequenceA args
        _ -> Nothing

    f (TypeSeqF seq) = TypeSeq <$> sequenceA seq
    f (TypeConcF conc) = TypeConc <$> sequenceA conc

data Package ident typevar = Package {
     _packageUnivVar :: Set typevar
    , _packageExisVar :: Set typevar
    , _packageSubs :: [Substitution typevar (TypeGTypeVar ident typevar)]
}  deriving (Show, Eq)

$(makeLenses ''Package)

type TagTypeMap = Map TypeTag TypeGTypeTag

packageToTagMap :: 
    Package TaggedBnfcIdent TypeTag -> 
    TagTypeMap
packageToTagMap pkg = Map.fromList packagesubs
  where
    packagesubs = mapMaybe (^?_StableSub) (pkg ^. packageSubs)
    {-
    packagesubs' = concatMap f (pkg ^. packageSubs)
      where
        f (a, TypeVar b []) = [(a, TypeVar b []), (b, TypeVar a [])]
        f n = [n]

    packagesubsandfreevars = 
        packagesubs' ++ map (id&&&flip TypeVar []) (Set.toList (pkg ^. packageFreeVars) \\ map fst packagesubs')
        -}

instance (Show a, Show b, PPrint a, Eq b, PPrint b) => PPrint (Package a b) where
    pprint pkg@(Package a b d) =  
        "Package:\n" ++ 
        "\nUniversal\n" ++ show a ++
        "\nExistential\n" ++ show b ++
        -- "\nFree\n" ++ show c ++
        "\nSubs\n" ++ intercalate "\n" (map (pprint) d) ++ "\n\n"


instance Ord typevar => Semigroup (Package ident typevar) where
    Package univ0 exis0  subs0 <> Package univ1 exis1  subs1
        = Package (univ0 <> univ1) (exis0 <> exis1)  (subs0 <> subs1)

instance Ord typevar => Monoid (Package ident typevar) where
    mempty = Package Set.empty  Set.empty []

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
    [Substitution TypeTag TypeGTypeTag] ->
    Either e [Substitution TypeTag TypeGTypeTag]
coalesce _ [] = pure []
coalesce (s, ssub) (StableSub (t, tsub):rst) = 
    (:)
    <$> (StableSub <$> mkValidSub t (substitute (s,ssub) tsub))
    <*> coalesce (s, ssub) rst
coalesce (s, ssub) (PlainSub (t, tsub):rst) 
    | s == t = mappend 
        <$> (map PlainSub <$> match ssub tsub) 
        <*> coalesce (s, ssub) rst
    | otherwise = (:) 
        <$> (PlainSub <$> mkValidSub t (substitute (s,ssub) tsub))
        <*> coalesce (s, ssub) rst

linearize :: 
    AsUnificationError e => 
    [Substitution TypeTag TypeGTypeTag] -> 
    Either e [Substitution TypeTag TypeGTypeTag]
-- linearize [] = pure []
-- linearize (a:as) = (:) a <$> (coalesce a as >>= linearize)
    -- this is not totally correct! for all cases! does not substitute back 
    -- (substitute out) to keep the system consistent.
    -- hence, the order of the type equations matters... 
    
    -- So, the truly correct implementaiton is as follows.
    -- ideally, we should refactor this to ``tie the knot"
    -- so we can get better performance and not do this needless
    -- back substitution and the back substituntion turns into
    -- a single pointer lookup...
{-
linearize subs = fst <$> f ([], subs)
  where
    -- | linearize driver function..
    f (subs', []) = pure (subs', [])
    f (subs', t:ts) = 
        -- (t : map (second (substitute t)) subs',) 
        (t : g (subs', h subs'),) 
        <$> coalesce t (alignSubs (fst t) . filter (not . isTrivialSubstitution)$ ts) 
        >>= f
      where
        -- | executing the back substitution until no substitutions remain...
        g (subs'', subs') 
            | subs'' == subs' = subs''
            | otherwise = g (subs', h subs')

        -- | a blind back substitution (does not check for occurs check)
        h = map (second (substitute t))
-}
linearize subs = fst <$> f ([], subs)
  where
    -- | linearize driver function..
    f (subs', []) = pure (subs', [])
    f (subs', StableSub t : ts) = f (StableSub t : subs', ts)
    f (subs', PlainSub t : ts) = 
        (PlainSub t : g (subs', h subs'),) 
        <$> coalesce t (alignSubs (fst t) . filter (not . isTrivialSubstitution) $ ts) 
        >>= f
      where
        -- | executing the back substitution until no substitutions remain...
        g (subs'', subs') 
            | subs'' == subs' = subs''
            | otherwise = g (subs', h subs')

        -- | a blind back substitution (does not check for occurs check)
        h = map (fmap (second (substitute t)))

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
        subs <- map PlainSub <$> match a b
        let pkg' = mempty & packageSubs .~ subs -- & packageFreeVars .~ freevars
        return pkg' 

    f (TypeEqnsStableRefF (a, b)) = do 
        return (mempty & packageSubs .~ [StableSub (a,b)] )

    f (TypeEqnsExistF vs acc) = do
        acc' <- sequenceA acc
        let pkg = mconcat acc' 
                & packageExisVar %~ (Set.fromList vs `Set.union`)
                & packageSubs %~ map (fmap (second simplifyArrow))
                -- & packageSubs %~ ((undefined)++)
        packageExistentialElim pkg
        -- packageExistentialElim (trace (pprint pkg) pkg)

    f (TypeEqnsForallF vs acc) = do
        acc' <- sequenceA acc
        let pkg = mconcat acc' 
                & packageUnivVar %~ (Set.fromList vs `Set.union`)
                & packageSubs %~ map (fmap (second simplifyArrow))
        packageUniversalElim pkg 
        -- packageUniversalElim (trace (pprint pkg) pkg)

isTrivialSubstitution :: Substitution TypeTag TypeGTypeTag -> Bool
isTrivialSubstitution (PlainSub (s, TypeVar t [])) = s == t
isTrivialSubstitution _ = False

packageExistentialElim ::
    AsUnificationError e => 
    Package TaggedBnfcIdent TypeTag -> 
    Either e (Package TaggedBnfcIdent TypeTag)
packageExistentialElim pkg = do
    let pkg' = pkg 
            & packageExisVar .~ Set.empty 
            & packageSubs %~ filter (not . isTrivialSubstitution)
    foldrM f pkg' (pkg ^. packageExisVar) >>= traverseOf packageSubs linearize
    -- pkg'' <- foldrM f pkg' (pkg ^. packageExisVar) 

    -- the package reduction (i.e. linearize) CANNOT happen while eliminating each vairalbe
    -- must occur after we eliminate each existential var...
    -- pkg''' <- traverseOf packageSubs linearize $ trace (pprint pkg'') pkg''
    -- return $ trace (pprint pkg''') pkg'''
    --
        -- & packageFreeVars %~ (`Set.difference` (pkg ^. packageExisVar))
  where
    f :: AsUnificationError e => 
        TypeTag -> 
        Package TaggedBnfcIdent TypeTag -> 
        Either e (Package TaggedBnfcIdent TypeTag)
    f v acc = let subs = alignSubs v $ acc ^. packageSubs in case lookupSubList v subs of
        Just sub -> do 
            let subs' = deleteSubList v subs
                plainsubvars = concatMap 
                    ( fromMaybe []
                    . preview 
                        ( _PlainSub 
                        % to (\(a,b) -> a : toList b)
                        )
                    )
                    subs'
                subexisted = v `elem` plainsubvars
            subs'' <- coalesce (v,sub) $ subs'
            return $ acc 
                & packageSubs .~ (bool (PlainSub(v, sub):) id subexisted)  subs''
                & packageExisVar %~ bool (Set.singleton v `Set.union`) id subexisted

        Nothing -> return $ 
            acc & packageExisVar %~ (Set.singleton v `Set.union`)

tracesubs str = (\n -> trace ((str++) . intercalate ",  " . map pprint $ n ) n)

packageUniversalElim :: 
    AsUnificationError e => 
    Package TaggedBnfcIdent TypeTag -> 
    Either e (Package TaggedBnfcIdent TypeTag)
packageUniversalElim pkg = do
    pkg' <- foldrM f 
            (pkg & packageUnivVar .~ Set.empty)
            (pkg ^. packageUnivVar)
    return $ pkg' 
        -- & packageFreeVars %~ (`Set.difference` (pkg ^. packageExisVar))
        & packageSubs %~ filter (not . isTrivialSubstitution)
  where
    f v acc = do
        let subs = alignSubs v $ acc ^. packageSubs
        -- case lookup (trace (pprint v++"\n") v) (trace (intercalate "," . map pprint $ subs) subs) of
        case lookupSubList v subs of
            Just lkup@(TypeVar v' args) 
                | v' == v ->  return acc
                | v' `elem` pkg ^. packageExisVar -> 
                    packageExistentialElim (acc & packageSubs %~ (PlainSub (v, lkup):))
            Just err -> Left $ _ForallMatchFailure # (v, err)
            Nothing -> return acc

            
        {-
        case filter ((==v) . fst) $ subs of
            [] -> return $ acc & packageUnivVar %~ (Set.singleton v `Set.union`)
            as -> let nontrivsubs = (filter (not . isTrivialSubstitution) as) in 
                if null nontrivsubs
                    then return acc
                    else do
                        
                        Left $ _ForallMatchFailure # nontrivsubs
                    -- else Left $ _ForallMatchFailure # nontrivsubs
                    -}
    {-
    f v acc = let subs = alignSubs v $ acc ^. packageSubs in case filter ((==v) . fst) $ subs of
        [] -> return $ acc & packageUnivVar %~ (Set.singleton v `Set.union`)
        as -> let nontrivsubs = (filter (not . isTrivialSubstitution) as) in if null nontrivsubs
                then return acc
                else Left $ _ForallMatchFailure # nontrivsubs
                -}

{-
alignSubs :: TypeTag -> [(TypeTag, TypeGTypeTag)] -> [(TypeTag, TypeGTypeTag)]
alignSubs k = map g
  where
    g (a, TypeVar b []) 
        | k == b = (b, TypeVar a [])
        | otherwise = (a, TypeVar b [])
    g n = n 
-}
alignSubs :: TypeTag -> 
    [Substitution TypeTag TypeGTypeTag] -> 
    [Substitution TypeTag TypeGTypeTag]
alignSubs k = map g
  where
    g (PlainSub (a, TypeVar b []) )
        | k == b = PlainSub (b, TypeVar a [])
        | otherwise = PlainSub (a, TypeVar b [])
    g n = n 

lookupSubList :: 
    TypeTag ->
    [Substitution TypeTag TypeGTypeTag] ->
    Maybe TypeGTypeTag
lookupSubList v (PlainSub (v', sub) : rst)
    | v == v' = Just sub
    | otherwise = lookupSubList v rst
lookupSubList v (StableSub _ : rst) =
    lookupSubList v rst
lookupSubList v [] = Nothing


deleteSubList :: 
    TypeTag ->
    [Substitution TypeTag TypeGTypeTag] ->
    [Substitution TypeTag TypeGTypeTag] 
deleteSubList v (PlainSub (v', sub) : rst) 
    | v == v' = rst
    | otherwise = PlainSub (v', sub) : deleteSubList v rst
deleteSubList v (StableSub n : rst) =
    StableSub n : deleteSubList v rst
deleteSubList v [] = []

floatTypeEqnsQuantifiers :: 
    TypeEqns TaggedBnfcIdent TypeTag ->
    TypeEqns TaggedBnfcIdent TypeTag 
floatTypeEqnsQuantifiers typeeqn = 
    TypeEqnsForall (typeeqn' ^. _1 % to reverse) 
    [TypeEqnsExist (typeeqn' ^. _2 % to reverse) (typeeqn' ^. _3)]
    -- reverse for better error messages
  where
    typeeqn' = cata f typeeqn

    f (TypeEqnsEqF (a, b)) = ([], [], [TypeEqnsEq (a,b)])
    f (TypeEqnsStableRefF (a, b)) = ([], [], [TypeEqnsStableRef (a,b)])
    f (TypeEqnsExistF vs acc) = 
        mconcat acc & _2 %~ (vs++)
    f (TypeEqnsForallF vs acc) = 
        mconcat acc & _1 %~ (vs++)

