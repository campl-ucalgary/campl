{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , MonadError (NonEmpty e) m 
    , HasToGraphState s 
    , HasUniqueTag s 
    , AsToGraphErrors e
    , AsTypeClauseSanityCheckError e 
    , AsTieTypeClauseError e ) =>
    DefnI BnfcIdent -> 
    m (SymbolTable, Stmt (DefnG TaggedBnfcIdent))
defInterfaceToGraph n = case n ^. unDefnI of
    DataDefn n -> do
        symtable <- use toGraphSymbolTable
        uniquetag <- freshUniqueTag
        let sanity = liftAEither $ typeClauseArgsSanityCheck n
            phraseto = liftAEither $ phraseToVarsAreStateVar n
            symtable' = mapMaybe f symtable
            f (str, (SymEntry tag info)) = case info of
                SymSeqClause node   -> Just (str, _SymEntry # (tag, _SymTypeClause # node))
                -- SymCodataClause node -> Just (str, _SymEntry # (tag, _SymTypeClause # node))
                _ -> Nothing
            tie = liftAEither $ makeTypeClauseGraph DataObj (_TieTypeClauseContext # (symtable', uniquetag)) n
        ((), (), (uniquetag', clausesgraph)) <- liftEither $ runAccumEither $ 
            (,,) <$> sanity <*> phraseto <*> tie

        uniqueTag .= uniquetag'

        -- return clausesgraph
        undefined 
        
    CodataDefn n ->  undefined
    ProtocolDefn n ->  undefined
    CoprotocolDefn n ->  undefined
    FunctionDecDefn n ->  undefined
    ProcessDecDefn n ->  undefined

collectClauseGraphPhrases ::
    ClausesGraph TaggedBnfcIdent -> 
    [a]  
collectClauseGraphPhrases = undefined

