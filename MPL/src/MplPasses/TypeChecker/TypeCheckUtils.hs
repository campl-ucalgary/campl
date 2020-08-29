{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeCheckUtils where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplUtil.UniqueSupply

import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckErrors
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.KindCheck
import MplPasses.TypeChecker.TypeCheckPanic
import MplPasses.Env

import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil 

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Functor.Foldable (Base, cata, para)

import Data.Maybe

import Debug.Trace

type TypeCheckEnv = Env SymTab TypeInfoEnv

data TypeInfoEnv = TypeInfoEnv {
    -- | the symbol table of course.
    _typeInfoSymTab :: SymTab
    -- | this should only change as you traverse down
    -- the expression
    , _typeInfoEnvTypeTag :: TypeTag
    -- | this should not change...
    , _typeInfoEnvMap :: TypeTagMap 
}

$(concat <$> traverse makeLenses 
    [ ''TypeInfoEnv ]
 )

freshTypeInfoEnv :: 
    ( HasUniqueSupply s
    , MonadState s m ) =>
    m TypeInfoEnv
freshTypeInfoEnv = do
    tag <- freshTypeTag
    return $ TypeInfoEnv mempty tag mempty

withFreshTypeTag ::
    ( AsTypeCheckErrors e 
    , MonadWriter [e] m 
    , MonadState TypeCheckEnv m
    , MonadFix m ) =>
    m a -> m (TypeTag, a)
withFreshTypeTag act = do
    tag <- guse (envLcl % typeInfoEnvTypeTag)
    tag' <- freshTypeTag
    envLcl % typeInfoEnvTypeTag .= tag'
    res <- act
    envLcl % typeInfoEnvTypeTag .= tag
    return (tag', res)

type TypeCheck renamed typechecked =
    forall e m. 
    ( AsTypeCheckErrors e 
    , AsKindCheckErrors e
    , AsTypeUnificationError e MplTypeSub
    , MonadWriter [e] m 
    , MonadState TypeCheckEnv m
    , MonadFix m ) =>
    renamed -> m typechecked

-- Utility functions..
genStableEqn :: 
    TypeTag -> 
    TypeP MplTypeSub ->  
    TypeEqns MplTypeSub
genStableEqn tag typep = _TypeEqnsEqStable # (typep & typeIdentTUniqueTag .~ tag, typePtoTypeVar typep)

packageToTypeTagMap :: 
    TypeCheck (Package MplTypeSub) TypeTagMap 
packageToTypeTagMap pkg = fmap Map.fromList 
    $ traverse (\(tag, tp) -> fmap ((tag,) . fromJust) $ higherOrderCheck tp) tagstotypesub
  where
    tagstotypesub = pkg ^. packageSubs % to (map (first (view typeIdentTUniqueTag) . snd))

higherOrderCheck :: 
    TypeCheck (MplType MplTypeSub) (Maybe SymTypeEntry)
higherOrderCheck tp 
    | Just (cxt, seqs, ins, outs) <- tp ^? _TypeConcArrF = do
        seqs' <- traverse go seqs
        ins' <- traverse go ins
        outs' <- traverse go outs
        return $ do
            seqs'' <- sequenceA seqs'
            ins'' <- sequenceA ins'
            outs'' <- sequenceA outs'
            return $ _SymTypeProc # ([], seqs'', ins'', outs'')
    | Just (cxt, froms, to) <- tp ^? _TypeSeqArrF = do
        froms' <- traverse go froms
        to' <- go to
        return $ do
            froms'' <- sequenceA froms'
            to'' <- to'
            return $ _SymTypeFun # ([], NE.toList froms'', to'')
    | otherwise = fmap (review _SymType) <$> go tp
  where
    go = para f

    f :: Base (MplType MplTypeSub) (MplType MplTypeSub, _ (Maybe (MplType MplTypeChecked))) ->
        (_ (Maybe (MplType MplTypeChecked)))
    f  (TypeVarF cxt n) = return $ Just $ TypeVar Nothing (typeIdentTToTypeT n)
    -- f  (TypeWithNoArgsF cxt n) = return $ TypeVar Nothing (typeIdentTToTypeT n)
    f  (TypeSeqWithArgsF cxt n args) = do
        args' <- traverse snd args
        return $ TypeSeqWithArgs (snd cxt) n <$> sequenceA args'
    f  (TypeConcWithArgsF cxt n args) = do
        args' <- traverseOf each (traverse snd) args
        return $ TypeConcWithArgs (snd cxt) n <$> traverseOf each sequenceA args'
    f  (TypeBuiltInF n) = fmap (fmap TypeBuiltIn) $ case n of
            TypeIntF cxt -> undefined -- return $ Just $ TypeIntF cxt

            TypeSeqArrF cxt froms to -> do
                tell [ _IllegalHigherOrderFunction # (fmap fst froms, fst to) ]
                return Nothing 

typeIdentTToTypeT :: TypeIdentT -> TypeP MplTypeChecked
typeIdentTToTypeT (TypeIdentT tag (TypeIdentTInfoTypeVar tp)) = tp
typeIdentTToTypeT (TypeIdentT (TypeTag tag) _) = GenNamedType tag

{-
lookupSym :: 
    ( MonadState TypeCheckEnv m ) => Lens' TypeCheckEnv (Maybe a) -> m a
lookupSym lens = join $ guses lens $ maybe (envLcl % typeInfoSymTab % symTabBadLookup .= True >> panicSymTab) pure
-}



class MkTypeSubSeqArr t where
    mkTypeSubSeqArr :: t -> MplType MplTypeSub

instance MkTypeSubSeqArr ([MplType MplTypeSub], MplType MplTypeSub) where
    mkTypeSubSeqArr ([], to) = to
    mkTypeSubSeqArr (froms, to) = _TypeSeqArrF # 
        (Nothing, NE.fromList froms, to)

instance MkTypeSubSeqArr ([TypeIdentT], TypeIdentT) where
    mkTypeSubSeqArr (froms, to) = mkTypeSubSeqArr 
        (map typePtoTypeVar froms, typePtoTypeVar to)
