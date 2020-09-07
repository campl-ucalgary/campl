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
{-# LANGUAGE QuantifiedConstraints #-}
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
import MplPasses.TypeChecker.TypeCheckSemanticErrors
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.KindCheck
import MplPasses.TypeChecker.TypeCheckPanic
import MplPasses.TypeChecker.TypeCheckErrorPkg
import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.Env

import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil 

import Data.List

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
import Data.Kind

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
    MonadState TypeCheckEnv m =>
    m a -> m (TypeTag, a)
withFreshTypeTag act = do
    tag <- guse (envLcl % typeInfoEnvTypeTag)
    tag' <- freshTypeTag
    envLcl % typeInfoEnvTypeTag .= tag'
    res <- act
    envLcl % typeInfoEnvTypeTag .= tag
    return (tag', res)

type AsAllTypeCheckErrors e = 
    ( AsTypeUnificationError e MplTypeSub
    , AsTypeCheckSemanticErrors e
    , AsKindCheckErrors e
    , AsTypeCheckCallErrors e
    , AsTypeCheckErrors e
    )

type TypeCheck renamed typechecked =
    forall e0 e1 m0 m1 symm n. 
    ( AsAllTypeCheckErrors e0
    , AsAllTypeCheckErrors e1

    , MonadWriter (TypeCheckErrorPkg e0 e1) n 
    , MonadWriter (TypeCheckErrorPkg e0 e1) m0
    , MonadWriter (TypeCheckErrorPkg e0 e1) m1
    , MonadWriter (TypeCheckErrorPkg e0 e1) symm

    , MonadFix n 

    , Zoom symm n SymTab TypeCheckEnv
    , SymZooms m0 m1 symm
    ) =>
    renamed -> n typechecked


-- Utility functions..
genStableEqn :: 
    TypeTag -> 
    TypeP MplTypeSub ->  
    TypeEqns MplTypeSub
genStableEqn tag typep = _TypeEqnsEqStable # (typep & typeIdentTUniqueTag .~ tag, typePtoTypeVar typep)

genTypeEqEqns :: 
    [MplType MplTypeSub] ->  
    [MplType MplTypeSub] ->  
    [TypeEqns MplTypeSub]
genTypeEqEqns = zipWith (\a -> review _TypeEqnsEq . (a,) )

packageToTypeTagMap :: 
    ( AsTypeCheckSemanticErrors e
    , MonadWriter [e] m ) => 
    Package MplTypeSub -> m TypeTagMap 
packageToTypeTagMap pkg = fmap Map.fromList 
    $ traverse (\(tag, tp) -> fmap ((tag,) . fromJust) $ higherOrderCheck tp) tagstotypesub
  where
    tagstotypesub = pkg ^. packageSubs % to (map (first (view typeIdentTUniqueTag) . snd))

higherOrderCheck :: 
    ( AsTypeCheckSemanticErrors e
    , MonadWriter [e] m ) => 
    MplType MplTypeSub -> 
    m (Maybe SymTypeEntry)
higherOrderCheck tp 
    | Just (cxt, seqs, ins, outs) <- tp ^? _TypeConcArrF = do
        seqs' <- traverse go seqs
        ins' <- traverse go ins
        outs' <- traverse go outs
        return $ do
            seqs'' <- sequenceA seqs'
            ins'' <- sequenceA ins'
            outs'' <- sequenceA outs'
            return $ _SymTypeConc # 
                ( nub $ foldMap mplTypeCollectTypeP $ seqs'' <> ins'' <> outs''
                , seqs''
                , ins''
                , outs'')
    | Just (cxt, froms, to) <- tp ^? _TypeSeqArrF = do
        froms' <- traverse go froms
        to' <- go to
        return $ do
            froms'' <- sequenceA froms'
            to'' <- to'
            return $ _SymTypeSeq # 
                ( nub $ foldMap mplTypeCollectTypeP (NE.cons to'' froms'')
                , NE.toList froms''
                , to'')
    | otherwise = do
        tp' <- go tp
        return $ do
            tp'' <- tp'
            return $ _SymTypeSeq # 
                ( nub $ mplTypeCollectTypeP tp''
                , mempty
                , tp'')
  where
    go = para f

    f :: Base (MplType MplTypeSub) (MplType MplTypeSub, _ (Maybe (MplType MplTypeChecked))) ->
        (_ (Maybe (MplType MplTypeChecked)))
    f  (TypeVarF cxt n) = return $ Just $ TypeVar Nothing (typeIdentTToTypeT n)
    f  (TypeWithNoArgsF cxt n) = return $ Just $ TypeWithNoArgs cxt n
    f  (TypeSeqWithArgsF cxt n args) = do
        args' <- traverse snd args
        return $ TypeSeqWithArgs (snd cxt) n <$> sequenceA args'
    f  (TypeConcWithArgsF cxt n args) = do
        args' <- traverseOf each (traverse snd) args
        return $ TypeConcWithArgs (snd cxt) n <$> traverseOf each sequenceA args'
    f  (TypeBuiltInF n) = fmap (fmap TypeBuiltIn) $ case n of
            TypeIntF cxt -> undefined -- return $ Just $ TypeIntF cxt

            TypeTopBotF cxt -> return $ _Just % _TypeTopBotF # (cxt ^? _TypeChAnnNameOcc)


            TypeGetF cxt (_, seq) (_, conc) -> do
                seq' <- seq
                conc' <- conc
                return $ do
                    seq'' <- seq'
                    conc'' <- conc'
                    return $ _TypeGetF # (cxt ^? _TypeChAnnNameOcc, seq'', conc'')
            -- duplicatedcode..
            TypePutF cxt (_, seq) (_, conc) -> do
                seq' <- seq
                conc' <- conc
                return $ do
                    seq'' <- seq'
                    conc'' <- conc'
                    return $ _TypePutF # (cxt ^? _TypeChAnnNameOcc, seq'', conc'')

            TypeTensorF cxt (_, a) (_, b) -> do
                a' <- a
                b' <- b
                return $ do
                    a'' <- a'
                    b'' <- b'
                    return $ _TypeTensorF # (cxt ^? _TypeChAnnNameOcc, a'', b'')
            TypeParF cxt (_, a) (_, b) -> do
                a' <- a
                b' <- b
                return $ do
                    a'' <- a'
                    b'' <- b'
                    return $ _TypeParF # (cxt ^? _TypeChAnnNameOcc, a'', b'')

            TypeSeqArrF cxt froms to -> do
                tell [ _IllegalHigherOrderFunction # (fmap fst froms, fst to) ]
                return Nothing 

            TypeNegF cxt (_, tp) -> do
                tp' <- tp
                return $ do
                    tp'' <- tp'
                    return $ _TypeNegF # (cxt ^? _TypeChAnnNameOcc, tp'') 

typeIdentTToTypeT :: TypeIdentT -> TypeP MplTypeChecked
typeIdentTToTypeT (TypeIdentT tag (TypeIdentTInfoTypeVar tp)) = tp
typeIdentTToTypeT (TypeIdentT (TypeTag tag) _) = GenNamedType tag

class MkTypeSubSeqArr t where
    mkTypeSubSeqArr :: t -> MplType MplTypeSub

instance MkTypeSubSeqArr ([MplType MplTypeSub], MplType MplTypeSub) where
    mkTypeSubSeqArr ([], to) = to
    mkTypeSubSeqArr (froms, to) = _TypeSeqArrF # 
        (Nothing, NE.fromList froms, to)

instance MkTypeSubSeqArr ([TypeIdentT], TypeIdentT) where
    mkTypeSubSeqArr (froms, to) = mkTypeSubSeqArr 
        (map typePtoTypeVar froms, typePtoTypeVar to)
