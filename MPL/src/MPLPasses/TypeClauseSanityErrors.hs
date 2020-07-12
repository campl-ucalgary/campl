{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module MPLPasses.TypeClauseSanityErrors where

import Optics
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLPasses.SymbolTable
import MPLAST.MPLTypeAST
import MPLAST.MPLProgGraph
import MPLPasses.ToGraphTypes

import Data.Functor.Foldable

import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.State

import Control.Arrow

import Control.Monad
import Data.Maybe
import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import MPLUtil.Data.Either.AccumEither

import Debug.Trace

data TypeClauseSanityCheckError =
    InvalidMutuallyRecursiveTypeArgDec (NonEmpty (TypeClause () () () BnfcIdent BnfcIdent))
    | OverlappingTypeVariables (NonEmpty (TypeClause () () () BnfcIdent BnfcIdent))
    | CodataInputArgStateVarOccurence (TypeClause () () () BnfcIdent BnfcIdent)
    | PhraseToMustBeStateVar (TypeClause () () () BnfcIdent BnfcIdent)

$(makeClassyPrisms ''TypeClauseSanityCheckError)

-- recall that a statevar MUST occur in the input args of
-- codata
codataStateVarOccurenceCheck :: 
    forall e.
    ( AsTypeClauseSanityCheckError e ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    Either (NonEmpty e) ()
codataStateVarOccurenceCheck clauses = void $ liftEither 
    $ runAccumEither $ traverse (liftAEither . f) clauses
  where
    f :: TypeClause () () () BnfcIdent BnfcIdent -> Either (NonEmpty e) ()
    f clause = bool (Left $ _CodataInputArgStateVarOccurence # clause :| []) (return ()) check
      where
        check = all
            ( any ( maybe False 
                    ( (clause ^. typeClauseStateVar % bnfcIdentName ==)
                    . view bnfcIdentName)) . map (preview _TypeVar) 
                . view typePhraseFrom ) 
            ( clause ^. typeClausePhrases)

-- used for data, protocol, 
phraseToVarsAreStateVar ::
    forall e.
    ( AsTypeClauseSanityCheckError e ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    Either (NonEmpty e) ()
phraseToVarsAreStateVar clauses = void $ liftEither 
    $ runAccumEither $ traverse (liftAEither . f) clauses
  where
    f :: TypeClause () () () BnfcIdent BnfcIdent -> Either (NonEmpty e) ()
    f clause = bool (Left $ _PhraseToMustBeStateVar # clause :| []) (return ()) True
      where
        stvname = clause ^. typeClauseStateVar % bnfcIdentName
        check = all 
            ( maybe False ((stvname==) . view bnfcIdentName)
            . preview (typePhraseTo % _TypeVar) ) (clause ^. typeClausePhrases)

-- used for co protocol 
-- mostly duplicated code of switching to checking the from vars
phraseFromVarsAreStateVar ::
    forall e.
    ( AsTypeClauseSanityCheckError e ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    Either (NonEmpty e) ()
phraseFromVarsAreStateVar clauses = void $ liftEither 
    $ runAccumEither $ traverse (liftAEither . f) clauses
  where
    f :: TypeClause () () () BnfcIdent BnfcIdent -> Either (NonEmpty e) ()
    f clause = bool (Left $ _PhraseToMustBeStateVar # clause :| []) (return ()) True
      where
        stvname = clause ^. typeClauseStateVar % bnfcIdentName
        check = all ( g . view typePhraseFrom ) (clause ^. typeClausePhrases)
        g [TypeVar tpvar] = stvname ==  view bnfcIdentName tpvar
        g _ = False

typeClauseArgsSanityCheck ::
    ( AsTypeClauseSanityCheckError e ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    Either (NonEmpty e) ()
typeClauseArgsSanityCheck clause@(TypeClause name args stv phrases () :| rst) 
    | mutuallyrecursivevalidity && overlappingargsvalidity = return ()
    | not mutuallyrecursivevalidity && not overlappingargsvalidity = throwError $ 
        mutuallyrecursivevalidityerror <> overlappingtypevarerror
    | not mutuallyrecursivevalidity = throwError mutuallyrecursivevalidityerror
    | otherwise = throwError $ overlappingtypevarerror
  where
    focusedargsnames = map (view bnfcIdentName) args
    otherclauseargs = map (map (view bnfcIdentName) . view typeClauseArgs) rst
    -- mutually recursive things MUST have the same type variables (as part of 
    -- the programming language specification)..
    mutuallyrecursivevalidity = all (==focusedargsnames) otherclauseargs

    statevars = stv : map (view typeClauseStateVar) rst
    argsandstatevars = args ++ statevars
    overlappingargsvalidity = length (nub argsandstatevars) == length argsandstatevars

    mutuallyrecursivevalidityerror = _InvalidMutuallyRecursiveTypeArgDec # clause :| []
    overlappingtypevarerror = _OverlappingTypeVariables # clause :| []
