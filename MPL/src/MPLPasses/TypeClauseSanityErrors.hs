{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module MPLPasses.TypeClauseSanityErrors where

import Optics
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLAST.MPLProgGraph
import MPLPasses.TieDefnsTypes
import MPLPasses.TieDefnsErrors

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
import Data.Foldable

import MPLUtil.Data.Either.AccumEither

import Debug.Trace

-- recall that a statevar MUST occur as the last argument for
-- each codata phrase...
codataStateVarOccurenceCheck :: 
    forall m e.
    ( AsTypeClauseError e
    , MonadWriter [e] m ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    -- Either (NonEmpty e) ()
    m ()
codataStateVarOccurenceCheck clauses = traverse_ f clauses
  where
    f :: TypeClause () () () BnfcIdent BnfcIdent -> m ()
    f clause = bool (tell [_CodataInputArgStateVarOccurence # clause]) (return ()) check
      where
        check = all
            (maybe False 
                (maybe False (\(ident, args) -> 
                        ident ^. bnfcIdentName == clause ^. typeClauseStateVar % bnfcIdentName 
                        && null args) 
                    . preview _TypeVar ) 
                . lastOf folded 
                . view typePhraseFrom )
            ( clause ^. typeClausePhrases )
        {- previously, we just checked for if it occured...
        check = all
            ( any ( maybe False 
                    (\(ident, args) -> 
                            bool False 
                                (clause ^. typeClauseStateVar % bnfcIdentName 
                                    == ident ^. bnfcIdentName) (null args)
                     ) ) . map (preview _TypeVar)
                . view typePhraseFrom ) 
            ( clause ^. typeClausePhrases)
            -}

-- used for data, protocol, 
phraseToVarsAreStateVarCheck ::
    forall m e.
    ( AsTypeClauseError e
    , MonadWriter [e] m ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    m ()
phraseToVarsAreStateVarCheck clauses = traverse_ f clauses
  where
    f :: TypeClause () () () BnfcIdent BnfcIdent -> m ()
    f clause = bool (tell [_PhraseToMustBeStateVar # clause]) (return ()) check
      where
        stvname = clause ^. typeClauseStateVar % bnfcIdentName
        check = all 
            ( maybe False (\(ident, args) -> 
                        bool False (ident ^. bnfcIdentName == stvname) (null args))
            . preview (typePhraseTo % _TypeVar) ) (clause ^. typeClausePhrases)

-- used for co protocol 
-- mostly duplicated code of switching to checking the from vars
phraseFromVarsAreStateVarCheck ::
    forall m e.
    ( AsTypeClauseError e
    , MonadWriter [e] m ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    m ()
phraseFromVarsAreStateVarCheck clauses = traverse_ f clauses
  where
    f :: TypeClause () () () BnfcIdent BnfcIdent -> m ()
    f clause = bool (tell [_PhraseToMustBeStateVar # clause]) (return ()) check
      where
        stvname = clause ^. typeClauseStateVar % bnfcIdentName
        check = all ( g . view typePhraseFrom ) (clause ^. typeClausePhrases)
        g [TypeVar tpvar []] = stvname ==  view bnfcIdentName tpvar
        g _ = False

typeClauseArgsSanityCheck ::
    forall m e.
    ( AsTypeClauseError e
    , MonadWriter [e] m ) =>
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    m ()
typeClauseArgsSanityCheck clause@(TypeClause name args stv phrases () :| rst) 
    | mutuallyrecursivevalidity && overlappingargsvalidity = return ()
    | not mutuallyrecursivevalidity && not overlappingargsvalidity = 
        tell [mutuallyrecursivevalidityerror, overlappingtypevarerror ]
    | not mutuallyrecursivevalidity = tell [mutuallyrecursivevalidityerror]
    | otherwise = tell [overlappingtypevarerror]
  where
    focusedargsnames = map (view bnfcIdentName) args
    otherclauseargs = map (map (view bnfcIdentName) . view typeClauseArgs) rst
    -- mutually recursive things MUST have the same type variables (as part of 
    -- the programming language specification)..
    mutuallyrecursivevalidity = all (==focusedargsnames) otherclauseargs

    statevars = stv : map (view typeClauseStateVar) rst
    argsandstatevars = args ++ statevars
    overlappingargsvalidity = length (nub argsandstatevars) == length argsandstatevars

    mutuallyrecursivevalidityerror = _InvalidMutuallyRecursiveTypeArgDec # clause 
    overlappingtypevarerror = _OverlappingTypeVariables # clause 
