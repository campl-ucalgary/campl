{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}
module MPLPasses.ToGraph where

import Optics 
import Optics.State
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLPasses.ToGraphErrors 
import MPLPasses.SymbolTable

import MPLPasses.ToGraphTypes
import MPLPasses.ToGraphErrors

import MPLPasses.TieTypeClause
import MPLPasses.TypeClauseSanityErrors
import MPLPasses.Unification
import MPLPasses.InferExprType

import MPLUtil.Data.Tuple.Optics
import MPLUtil.Data.Either.AccumEither

import Data.Maybe
import Control.Monad.RWS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Bifunctor as Bifunctor
import Control.Arrow

import Control.Monad.State
import Control.Monad.Except

progInterfaceToGraph :: 
    Prog (DefnI BnfcIdent) ->
    Prog (DefnG TaggedBnfcIdent) 
progInterfaceToGraph = undefined


stmtInterfaceToGraph ::
    ( MonadState s m 
    , AsToGraphErrors e
    , HasToGraphState s) =>
    Stmt (DefnI BnfcIdent) -> 
    m (Either (NonEmpty e) (SymbolTable, Stmt (DefnG TaggedBnfcIdent)))
stmtInterfaceToGraph (Stmt defs wstmts) = undefined


defInterfaceToGraph :: 
    ( MonadState s m 
    , MonadError e m 
    , HasToGraphState s 
    , HasUniqueTag s 
    , AsToGraphErrors e ) =>
    DefnI BnfcIdent -> 
    m (SymbolTable, DefnG TaggedBnfcIdent)
defInterfaceToGraph n = case n ^. unDefnI of
    DataDefn n -> do
        let sanity = liftAEither $ typeClauseArgsSanityCheck n :: AccumEither (NonEmpty TypeClauseError) ()
            phraseto = liftAEither $ phraseToVarsAreStateVar n :: AccumEither (NonEmpty TypeClauseError) ()
        objectDefnIToGraph (_ToGraphError % _DataDefn) _SymSeqClause [sanity, phraseto] n
        
    CodataDefn n -> do
        let sanity = liftAEither $ typeClauseArgsSanityCheck n :: AccumEither (NonEmpty TypeClauseError) ()
            codatacheck = liftAEither $ codataStateVarOccurenceCheck n :: AccumEither (NonEmpty TypeClauseError) ()
        objectDefnIToGraph (_ToGraphError % _CodataDefn) _SymSeqClause [sanity, codatacheck] n

    ProtocolDefn n -> do
        let sanity = liftAEither $ typeClauseArgsSanityCheck n :: AccumEither (NonEmpty TypeClauseError) ()
            phraseto = liftAEither $ phraseToVarsAreStateVar n :: AccumEither (NonEmpty TypeClauseError) ()
        objectDefnIToGraph (_ToGraphError % _ProtocolDefn) _SymConcClause [sanity, phraseto] n

    CoprotocolDefn n -> do
        let sanity = liftAEither $ typeClauseArgsSanityCheck n :: AccumEither (NonEmpty TypeClauseError) ()
            phrasefrom = liftAEither $ phraseFromVarsAreStateVar n :: AccumEither (NonEmpty TypeClauseError) ()
        objectDefnIToGraph (_ToGraphError % _ProtocolDefn) _SymConcClause [sanity, phrasefrom] n

    FunctionDecDefn n -> undefined
    ProcessDecDefn n ->  undefined

functionDefnIToGraph ::
    FunctionDefnI BnfcIdent 
functionDefnIToGraph = undefined

-- errorprism is the prism to construct the error...
-- matchprism is used to match the type from the symbol table...
-- checks is a list of AccumEither so that we can check the validity of the object
-- n is the typeclause
objectDefnIToGraph errorprism matchprism checks n = do
    -- get the symtable
    symtable <- use toGraphSymbolTable
    -- get the current fresh tag
    uniquetag <- freshUniqueTag
    let symtable' = mapMaybe f symtable
        -- filters the relevant symbol table entries
        f (str, (SymEntry tag info)) = case preview matchprism info of
            Just clauseg -> Just (str, _SymEntry # (tag, _SymTypeClause # clauseg ))
            _ -> Nothing
        tie = liftAEither $ makeTypeClauseGraph DataObj (_TieTypeClauseContext # (symtable', uniquetag)) n

    liftEither $ Bifunctor.first (review errorprism )
        $ runAccumEither 
        $ sequenceA checks

    (uniquetag', clausesgraph) <- liftEither 
        $ Bifunctor.first (review errorprism )
        $ runAccumEither tie

    uniqueTag .= uniquetag'

    return (collectClauseGraphSymbolTable clausesgraph, ObjectG clausesgraph)

collectClauseGraphClauses ::
    ClausesGraph TaggedBnfcIdent -> 
    [(String, TypeClauseG TaggedBnfcIdent)]  
collectClauseGraphClauses graph = map f $ NE.toList $ graph ^. clauseGraphSpine
  where
    f = view (typeClauseName % taggedBnfcIdentName ) &&& id

collectClauseGraphPhrases ::
    ClausesGraph TaggedBnfcIdent -> 
    [(String, TypePhraseG TaggedBnfcIdent)]
collectClauseGraphPhrases graph = concatMap f $ NE.toList $ graph ^. clauseGraphSpine
  where
    f graph = map g (graph ^. typeClausePhrases)
    g = view (typePhraseName % taggedBnfcIdentName) &&& id 

collectClauseGraphSymbolTable :: 
    ClausesGraph TaggedBnfcIdent ->
    SymbolTable
collectClauseGraphSymbolTable graph 
    | graph ^. clauseGraphObjectType == DataObj
        || graph ^. clauseGraphObjectType == CodataObj = 
            map (second (review _SymEntry <<< view (typeClauseName % uniqueTag) &&& SymSeqClause)) clauses 
            ++ map (second (review _SymEntry <<< view (typePhraseName % uniqueTag) &&& SymSeqPhrase)) phrases
    | otherwise = 
            map (second (review _SymEntry <<< view (typeClauseName % uniqueTag) &&& SymConcClause)) clauses 
            ++ map (second (review _SymEntry <<< view (typePhraseName % uniqueTag) &&& SymConcPhrase)) phrases
  where
    clauses = collectClauseGraphClauses graph
    phrases = collectClauseGraphPhrases graph
