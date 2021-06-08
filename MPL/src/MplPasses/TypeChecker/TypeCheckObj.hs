{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeChecker.TypeCheckObj where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.Env
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeCheckErrorPkg 

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import Data.Bool
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow

import Debug.Trace

typeCheckTypeClauseSpine ::
    forall t. 
    ( TypeClauseSpineSameVarError t 
    , TypePhraseStateVarError t
    , KindCheckObjArgsKindEnv t 
    , KindCheckPhrase t
    , TypeClauseArgs MplRenamed t ~ TypeClauseArgs MplTypeChecked t ) =>
    TypeCheck (MplTypeClauseSpine MplRenamed t) (MplTypeClauseSpine MplTypeChecked t)
typeCheckTypeClauseSpine spine = do
    -- first, check if all the args are the same 
    -- (mutually recursive types must have the same type variables)
    tell $ review _ExternalError $ typeClauseSpineSameVarError spine

    -- now, do additional checks with state variable placements....
    tell $ review _ExternalError $ foldMapOf (typeClauseSpineClauses % folded)  
        (\clause -> foldMapOf 
            (typeClausePhrases % folded) 
            (typePhraseStateVarError (clause ^. typeClauseStateVar)) 
            clause)
        spine

    -- kindchecking
    let kenv = kindCheckObjArgsKindEnv spine

    symtab <- guse $ envLcl % typeInfoSymTab % symTabType

    nspine <- (`evalStateT` kenv) . (`runReaderT` symtab) $ mdo
        spine' <- spine ^. typeClauseSpineClauses % to (traverse (f nspine))
        let ~nspine = MplTypeClauseSpine spine' (spine ^. typeClauseSpineExt)
        return nspine 

    return nspine
  where
    f :: _ -> MplTypeClause MplRenamed t -> _ (MplTypeClause MplTypeChecked t)
    f spine clause = do
        rec ~mphrases <- clause ^. typeClausePhrases % to (g clause')
            let clause' = MplTypeClause 
                        (clause ^. typeClauseName)
                        (clause ^. typeClauseArgs)
                        (clause ^. typeClauseStateVar)
                        mphrases
                        spine
        return $ clause' 
    g :: (MplTypeClause MplTypeChecked t) -> [MplTypePhrase MplRenamed t] ->
        _ ([MplTypePhrase MplTypeChecked t])
    g clause phrases = traverse (kindCheckPhrase . (clause,)) phrases 


class KindCheckObjArgsKindEnv (t :: ObjectDefnTag) where
    kindCheckObjArgsKindEnv :: 
        MplTypeClauseSpine MplRenamed t -> 
        KindCheckEnv

instance KindCheckObjArgsKindEnv (SeqObjTag t) where
    kindCheckObjArgsKindEnv spine = _KindCheckEnv # 
        (_SeqKind # (),  collectKindCheckEnvSeqs args)
      where
        args = Set.toList $ foldMapOf 
                (typeClauseSpineClauses % folded) (\clause -> 
                    clause ^. typeClauseArgs % to Set.fromList
                    <> clause ^. typeClauseStateVar % to Set.singleton) 
                spine 

instance KindCheckObjArgsKindEnv (ConcObjTag t) where
    kindCheckObjArgsKindEnv spine = _KindCheckEnv # 
        ( _ConcKind # ()
        , collectKindCheckEnvSeqs seqargs <> collectKindCheckEnvConcs concargs )
      where
        seqargs = Set.toList $ foldMapOf 
                (typeClauseSpineClauses % folded) (\clause -> 
                    clause ^. typeClauseArgs % _1 % to Set.fromList) 
                spine 
        concargs = Set.toList $ foldMapOf 
                (typeClauseSpineClauses % folded) (\clause -> 
                    clause ^. typeClauseArgs % _2 % to Set.fromList
                    <> clause ^. typeClauseStateVar % to Set.singleton) 
                spine 

class KindCheckPhrase (t :: ObjectDefnTag) where
    kindCheckPhrase :: 
        KindCheck (MplTypeClause MplTypeChecked t, MplTypePhrase MplRenamed t) 
            (MplTypePhrase MplTypeChecked t)
        {-
        ( AsKindCheckErrors e
        , MonadState KindCheckEnv m
        , MonadReader SymTabType m
        , MonadWriter [e] m ) => (MplTypeClause MplTypeChecked t, MplTypePhrase MplRenamed t) ->
            m (MplTypePhrase MplTypeChecked t)
            -}

instance KindCheckPhrase (SeqObjTag DataDefnTag) where
    kindCheckPhrase (clause, phrase) = do
        froms' <- phrase ^. typePhraseFrom 
                    % to ( traverse (\tp -> do 
                            kindCheckExpectedPrimitiveKind .= SeqKind () 
                            primitiveKindCheck tp
                        )
                    )

        kindCheckExpectedPrimitiveKind .= SeqKind () 
        to' <- phrase ^. typePhraseTo % to (primitiveKindCheck . review _TypeVar . ((),))
        return $ MplTypePhrase 
            (phrase ^. typePhraseName) 
            (fromJust $ sequenceA froms')
            (fromJust to')
            clause

instance KindCheckPhrase (SeqObjTag CodataDefnTag) where
    kindCheckPhrase (clause, phrase) = do
        froms' <- phrase ^. typePhraseFrom %  _1
                    % to ( traverse (\tp -> do 
                            kindCheckExpectedPrimitiveKind .= SeqKind () 
                            primitiveKindCheck tp
                        )
                    )

        kindCheckExpectedPrimitiveKind .= SeqKind () 
        fromst' <- phrase ^. typePhraseFrom 
            % _2 
            % to (primitiveKindCheck . review _TypeVar . ((),))

        kindCheckExpectedPrimitiveKind .= SeqKind () 
        to' <- phrase ^. typePhraseTo % to primitiveKindCheck

        return $ MplTypePhrase 
            (phrase ^. typePhraseName) 
            (fromJust $ sequenceA froms', fromJust fromst')
            (fromJust to')
            clause

instance KindCheckPhrase (ConcObjTag ProtocolDefnTag) where
    kindCheckPhrase (clause, phrase) = do
        kindCheckExpectedPrimitiveKind .= ConcKind () 
        from' <- phrase ^. typePhraseFrom % to primitiveKindCheck

        kindCheckExpectedPrimitiveKind .= ConcKind () 
        to' <- phrase ^. typePhraseTo 
            % to (primitiveKindCheck . review _TypeVar . ((),))

        return $ MplTypePhrase 
            (phrase ^. typePhraseName) 
            (fromJust from')
            (fromJust to')
            clause

instance KindCheckPhrase (ConcObjTag CoprotocolDefnTag) where
    kindCheckPhrase (clause, phrase) = do
        kindCheckExpectedPrimitiveKind .= ConcKind () 
        from' <- phrase ^. typePhraseFrom % to 
            (primitiveKindCheck . review _TypeVar . ((),))

        kindCheckExpectedPrimitiveKind .= ConcKind () 
        to' <- phrase ^. typePhraseTo % to primitiveKindCheck

        return $ MplTypePhrase 
            (phrase ^. typePhraseName) 
            (fromJust from')
            (fromJust to')
            clause

class TypeClauseSpineSameVarError (t :: ObjectDefnTag) where
    typeClauseSpineSameVarError :: 
        AsTypeCheckSemanticErrors e => 
        MplTypeClauseSpine MplRenamed t -> 
        [e]
instance TypeClauseSpineSameVarError (SeqObjTag t) where
    typeClauseSpineSameVarError spine = bool [] 
        [_SeqTypeClauseArgsMustContainTheSameTypeVariables # eqclasses] shoulderror
      where
        eqclasses :: [NonEmpty [IdentR]]
        eqclasses = NE.group 
            $ fmap (view typeClauseArgs)
            $ spine ^. typeClauseSpineClauses 

        shoulderror = length eqclasses >= 2

instance TypeClauseSpineSameVarError (ConcObjTag t) where
    typeClauseSpineSameVarError spine = bool [] 
        [_ConcTypeClauseArgsMustContainTheSameTypeVariables # eqclasses]
        shoulderror
      where
        eqclasses :: [NonEmpty ([IdentR], [IdentR])]
        eqclasses = NE.group 
            $ fmap (view typeClauseArgs) 
            $ spine ^. typeClauseSpineClauses 

        shoulderror = length eqclasses >= 2 

class TypePhraseStateVarError t where
    typePhraseStateVarError :: AsTypeCheckSemanticErrors e => IdentR -> MplTypePhrase MplRenamed t -> [e]

instance TypePhraseStateVarError (SeqObjTag DataDefnTag) where
    typePhraseStateVarError st phrase = bool [] 
        (phrase ^. typePhraseTo % to (\vto -> pure $ _ExpectedStateVarButGot # (st, vto)))  
        $ st /= phrase ^. typePhraseTo 

instance TypePhraseStateVarError (SeqObjTag CodataDefnTag) where
    typePhraseStateVarError st phrase = bool [] 
        (phrase ^. typePhraseFrom % _2 % to (\vst -> pure $ _ExpectedStateVarButGot # (st, vst)))  
        $ st /= phrase ^. typePhraseFrom % _2

instance TypePhraseStateVarError (ConcObjTag ProtocolDefnTag) where
    typePhraseStateVarError st phrase = bool [] 
        (phrase ^. typePhraseTo % to (\vto -> pure $ _ExpectedStateVarButGot # (st, vto)))  
        $ st /= phrase ^. typePhraseTo 

instance TypePhraseStateVarError (ConcObjTag CoprotocolDefnTag) where
    typePhraseStateVarError st phrase = bool [] 
        (phrase ^. typePhraseFrom % to (\vfrom -> pure $ _ExpectedStateVarButGot # (st, vfrom)))  
        $ st /= phrase ^. typePhraseFrom 

