{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module MplPasses.TypeChecker.TypeCheckUtils where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Foldable (Base, cata, para)
import Data.Kind
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplPasses.Env
import MplPasses.TypeChecker.KindCheck
import MplPasses.TypeChecker.TypeCheckCallErrors
import MplPasses.TypeChecker.TypeCheckErrorPkg
import MplPasses.TypeChecker.TypeCheckErrors
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeCheckPanic
import MplPasses.TypeChecker.TypeCheckSemanticErrors
import MplPasses.TypeChecker.TypeCheckSym
import MplPasses.TypeChecker.TypeEqns
import MplUtil.UniqueSupply
import Optics
import Optics.State.Operators

{- Module for defining useful utilities in type checking..
 -}

type TypeCheckEnv = Env SymTab TypeInfoEnv

data TypeInfoEnv = TypeInfoEnv
  { -- | the symbol table of course.
    _typeInfoSymTab :: SymTab,
    -- | this should only change as you traverse down
    -- the expression
    _typeInfoEnvTypeTag :: TypeTag,
    -- | this should not change...
    _typeInfoEnvMap :: TypeTagMap
  }

$(makeLenses ''TypeInfoEnv)

freshTypeInfoEnv ::
  ( HasUniqueSupply s,
    MonadState s m
  ) =>
  m TypeInfoEnv
freshTypeInfoEnv = do
  tag <- freshTypeTag
  return $ TypeInfoEnv mempty tag mempty

withFreshTypeTag ::
  (MonadState TypeCheckEnv m) =>
  m a ->
  m (TypeTag, a)
withFreshTypeTag act = do
  tag <- guse (envLcl % typeInfoEnvTypeTag)
  tag' <- freshTypeTag
  envLcl % typeInfoEnvTypeTag .= tag'
  res <- act
  envLcl % typeInfoEnvTypeTag .= tag
  return (tag', res)

type AsAllTypeCheckErrors e =
  ( AsTypeUnificationError e MplTypeSub,
    AsTypeCheckSemanticErrors e,
    AsKindCheckErrors e,
    AsTypeCheckCallErrors e,
    AsTypeCheckErrors e
  )

type TypeCheck renamed typechecked =
  forall e0 e1 m0 m1 symm n.
  ( AsAllTypeCheckErrors e0,
    AsAllTypeCheckErrors e1,
    MonadWriter (TypeCheckErrorPkg e0 e1) n,
    MonadWriter (TypeCheckErrorPkg e0 e1) m0,
    MonadWriter (TypeCheckErrorPkg e0 e1) m1,
    MonadWriter (TypeCheckErrorPkg e0 e1) symm,
    MonadFix n,
    Zoom symm n SymTab TypeCheckEnv,
    SymZooms m0 m1 symm
  ) =>
  renamed ->
  n typechecked

-- Utility functions..
genTypeEqEqns ::
  [MplType MplTypeSub] ->
  [MplType MplTypeSub] ->
  [TypeEqns MplTypeSub]
genTypeEqEqns = zipWith (\a -> review _TypeEqnsEq . (a,))

-- | converts a package to the tag map (tags to types). Given a list of not scoped variables,
-- -  it will also ensure that it does NOT forall quantify those (this is needed for let bindings
-- -  which use variables not bound within their scope.)
packageToTypeTagMap ::
  ( AsTypeCheckSemanticErrors e,
    MonadWriter [e] m
  ) =>
  [TypeP MplTypeSub] ->
  Package MplTypeSub ->
  m TypeTagMap
packageToTypeTagMap notscoped pkg =
  fmap Map.fromList $
    traverse (\(tag, tp) -> fmap ((tag,) . fromJust) $ higherOrderCheck notscoped tp) $
      f
  where
    f :: [(TypeTag, MplType MplTypeSub)]
    -- tagstotypesub = pkg ^. packageSubs % to (map (first (view typeIdentTUniqueTag)))
    f = pkg ^. packageSubs % to (concatMap g)

    g :: (TypeIdentT, MplType MplTypeSub) -> [(TypeTag, MplType MplTypeSub)]
    g (typet, tp@(TypeVar cxt typet')) =
      [ (typet ^. typeIdentTUniqueTag, tp),
        (typet' ^. typeIdentTUniqueTag, TypeVar cxt typet)
      ]
    g (typet, tp) = [(typet ^. typeIdentTUniqueTag, tp)]

-- | Checks if the inferred type has higher order functions AND scopes all of the free variables
higherOrderCheck ::
  ( AsTypeCheckSemanticErrors e,
    MonadWriter [e] m
  ) =>
  [TypeP MplTypeSub] ->
  MplType MplTypeSub ->
  m (Maybe SymTypeEntry)
higherOrderCheck notscoped tp
  | Just (cxt, seqs, ins, outs) <- tp ^? _TypeConcArrF = do
      seqs' <- traverse go seqs
      ins' <- traverse go ins
      outs' <- traverse go outs
      return $ do
        seqs'' <- sequenceA seqs'
        ins'' <- sequenceA ins'
        outs'' <- sequenceA outs'
        return $
          _SymTypeConc
            # ( nub $ foldMap mplTypeCollectTypeP $ seqs'' <> ins'' <> outs'',
                seqs'',
                ins'',
                outs''
              )
  | Just (cxt, froms, to) <- tp ^? _TypeSeqArrF = do
      froms' <- traverse go froms
      to' <- go to
      return $ do
        froms'' <- sequenceA froms'
        to'' <- to'
        return $
          _SymTypeSeq
            # ( filter (`notElem` notscoped') $
                  nub $
                    foldMap mplTypeCollectTypeP (NE.cons to'' froms''),
                NE.toList froms'',
                to''
              )
  | otherwise = do
      tp' <- go tp
      return $ do
        tp'' <- tp'
        return $
          _SymTypeSeq
            # ( nub $ mplTypeCollectTypeP tp'',
                mempty,
                tp''
              )
  where
    notscoped' = map typeIdentTToTypeT notscoped

    go = para f

    f ::
      Base (MplType MplTypeSub) (MplType MplTypeSub, _ (Maybe (MplType MplTypeChecked))) ->
      (_ (Maybe (MplType MplTypeChecked)))
    f (TypeVarF cxt n) = return $ Just $ TypeVar Nothing (typeIdentTToTypeT n)
    f (TypeWithNoArgsF cxt n) = return $ Just $ TypeWithNoArgs (snd cxt) n
    f (TypeSeqWithArgsF cxt n args) = do
      args' <- traverse snd args
      return $ TypeSeqWithArgs (snd cxt) n <$> sequenceA args'
    f (TypeConcWithArgsF cxt n args) = do
      args' <- traverseOf each (traverse snd) args
      return $ TypeConcWithArgs (snd cxt) n <$> traverseOf each sequenceA args'
    f (TypeBuiltInF n) = fmap (fmap TypeBuiltIn) $ case n of
      -- probably can retain some annotation information?
      TypeIntF _cxt -> return $ Just $ TypeIntF Nothing
      TypeDoubleF _cxt -> return $ Just $ TypeDoubleF Nothing
      TypeCharF _cxt -> return $ Just $ TypeCharF Nothing
      TypeBoolF _cxt -> return $ Just $ TypeBoolF Nothing
      {- built in primitives -}
      TypeUnitF _cxt -> return $ Just $ TypeUnitF Nothing
      TypeListF _cxt rst -> do
        rst' <- snd rst
        return $ fmap (TypeListF Nothing) rst'
      TypeStoreF _cxt rst -> do
        rst' <- snd rst
        return $ fmap (TypeStoreF Nothing) rst'
      TypeTupleF _cxt (t0, t1, ts) -> do
        t0' <- snd t0
        t1' <- snd t1
        ts' <- traverse snd ts
        -- probaly can retain some annotation information?
        return $
          fmap (TypeTupleF Nothing) $
            (,,)
              <$> t0'
              <*> t1'
              <*> sequenceA ts'
      TypeTopBotF cxt ->
        return $
          _Just
            % _TypeTopBotF
            # Nothing
      -- (cxt ^? _TypeChAnnNameOcc)

      TypeGetF cxt (_, seq) (_, conc) -> do
        seq' <- seq
        conc' <- conc
        return $ do
          seq'' <- seq'
          conc'' <- conc'
          -- return $ _TypeGetF # (cxt ^? _TypeChAnnNameOcc, seq'', conc'')
          return $ _TypeGetF # (Nothing, seq'', conc'')
      -- duplicatedcode..
      TypePutF cxt (_, seq) (_, conc) -> do
        seq' <- seq
        conc' <- conc
        return $ do
          seq'' <- seq'
          conc'' <- conc'
          -- return $ _TypePutF # (cxt ^? _TypeChAnnNameOcc, seq'', conc'')
          return $ _TypePutF # (Nothing, seq'', conc'')
      TypeTensorF cxt (_, a) (_, b) -> do
        a' <- a
        b' <- b
        return $ do
          a'' <- a'
          b'' <- b'
          -- return $ _TypeTensorF # (cxt ^? _TypeChAnnNameOcc, a'', b'')
          return $ _TypeTensorF # (Nothing, a'', b'')
      TypeParF cxt (_, a) (_, b) -> do
        a' <- a
        b' <- b
        return $ do
          a'' <- a'
          b'' <- b'
          -- return $ _TypeParF # (cxt ^? _TypeChAnnNameOcc, a'', b'')
          return $ _TypeParF # (Nothing, a'', b'')
      TypeSeqArrF cxt froms to -> do
        tell [_IllegalHigherOrderFunction # (fmap fst froms, fst to)]
        return Nothing
      TypeConcArrF cxt seqs ins outs -> do
        seqs' <- traverse snd seqs
        ins' <- traverse snd ins
        outs' <- traverse snd outs
        return $ do
          seqs'' <- sequenceA seqs'
          ins'' <- sequenceA ins'
          outs'' <- sequenceA outs'
          return $
            _TypeConcArrF
              # ( (),
                  seqs'',
                  outs'',
                  ins''
                )
      TypeNegF cxt (_, tp) -> do
        tp' <- tp
        return $ do
          tp'' <- tp'
          -- return $ _TypeNegF # (cxt ^? _TypeChAnnNameOcc, tp'')
          return $ _TypeNegF # (Nothing, tp'')

class MkTypeSubSeqArr t where
  mkTypeSubSeqArr :: Maybe TypeAnn -> t -> MplType MplTypeSub

instance MkTypeSubSeqArr ([MplType MplTypeSub], MplType MplTypeSub) where
  mkTypeSubSeqArr ann ([], to) = to
  mkTypeSubSeqArr ann (froms, to) =
    _TypeSeqArrF
      # (ann, NE.fromList froms, to)

instance MkTypeSubSeqArr ([TypeIdentT], TypeIdentT) where
  mkTypeSubSeqArr ann (froms, to) = mkTypeSubSeqArr ann (map typePtoTypeVar froms, typePtoTypeVar to)
