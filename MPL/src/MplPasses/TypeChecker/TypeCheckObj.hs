{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.KindCheck 

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
    ( TypeClauseSpineSameVarError t 
    , TypePhraseStateVarError t
    , KindCheckClauseSpine t ) =>
    TypeCheck (MplTypeClauseSpine MplRenamed t) (MplTypeClauseSpine MplTypeChecked t)
typeCheckTypeClauseSpine spine = do
    -- first, check if all the args are the same 
    -- (mutually recursive types must have the same type variables)
    tell $ typeClauseSpineSameVarError spine

    -- now, do additional checks with state variable placements....
    tell $ foldMapOf (typeClauseSpineClauses % folded)  
        (\clause -> foldMapOf 
            (typeClausePhrases % folded) 
            (typePhraseStateVarError (clause ^. typeClauseStateVar)) 
            clause)
        spine

    -- kindchecking
    kindCheckClauseSpine spine

class KindCheckClauseSpine (t :: ObjectDefnTag) where
    kindCheckClauseSpine :: TypeCheck (MplTypeClauseSpine MplRenamed t) (MplTypeClauseSpine MplTypeChecked t)

instance KindCheckClauseSpine (SeqObjTag DataDefnTag) where
    kindCheckClauseSpine spine = do
        let args = Set.toList $ foldMapOf 
                (typeClauseSpineClauses % folded) (\clause -> 
                    clause ^. typeClauseArgs % to Set.fromList
                    <> clause ^. typeClauseStateVar % to Set.singleton) 
                spine 
            kenv = KindCheckEnv (SeqKind ()) $ collectKindCheckEnvSeqs args

        symtab <- guse $ envLcl % typeInfoSymTab % symTabType

        nspine <- (`evalStateT` kenv) . (`runReaderT` symtab) $ mdo
            spine' <- spine ^. typeClauseSpineClauses % to (traverse (f nspine))
            let ~nspine = MplTypeClauseSpine spine' (spine ^. typeClauseSpineExt)
            return nspine 

        return nspine
      where
        f :: _ -> MplTypeClause MplRenamed (SeqObjTag DataDefnTag) -> 
            _ (MplTypeClause MplTypeChecked (SeqObjTag DataDefnTag))
        f spine clause = do
            rec ~mphrases <- clause ^. typeClausePhrases % to (g clause')
                let clause' = MplTypeClause 
                            (clause ^. typeClauseName)
                            (clause ^. typeClauseArgs)
                            (clause ^. typeClauseStateVar)
                            mphrases
                            spine
            return $ clause' 
        g :: (MplTypeClause MplTypeChecked (SeqObjTag DataDefnTag)) -> 
            [MplTypePhrase MplRenamed (SeqObjTag DataDefnTag)] ->
            _ ([MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag)])
        g clause phrases =  traverse (h clause) phrases 

        h :: _ -> MplTypePhrase MplRenamed (SeqObjTag DataDefnTag) ->
            _ (MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag))
        h clause phrase = do
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


class KindCheckPhrase (t :: ObjectDefnTag) where
    kindCheckPhrase :: TypeCheck (MplTypePhrase MplRenamed t) (MplTypePhrase MplTypeChecked t)


class TypeClauseSpineSameVarError (t :: ObjectDefnTag) where
    typeClauseSpineSameVarError :: 
        AsTypeCheckErrors e => 
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
    typePhraseStateVarError :: AsTypeCheckErrors e => IdentR -> MplTypePhrase MplRenamed t -> [e]

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

