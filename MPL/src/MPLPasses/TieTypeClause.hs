{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module MPLPasses.TieTypeClause where

import Optics
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLAST.MPLProgGraph
import MPLPasses.SymbolTable
import MPLPasses.GraphGenCore
import MPLPasses.TieDefnsErrors

import Data.Functor.Foldable

import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Bifunctor as Bifunctor
import Control.Arrow

import Data.Maybe
import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Debug.Trace


tieTypeClauseGraph :: 
    SymbolTable -> 
    ObjectType -> 
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) -> 
    GraphGenCore (ClausesGraph TaggedBnfcIdent)
tieTypeClauseGraph symtab obj clause = do
    rec let cxt = symtab
            clausegraph = _ClausesGraph # (obj, fromJust res)
        ((), st, res) <- (runRWST (unTieTypeClauseT $ tieTypeClauseKnot clause) clausegraph cxt)
    return $ clausegraph

type TieTypeClause a = 
    TieTypeClauseT GraphGenCore a

newtype TieTypeClauseT m a = 
        TieTypeClauseT
        { unTieTypeClauseT :: 
        (RWST 
        (ClausesGraph TaggedBnfcIdent)
        -- | writer monads require a monoid, but ClauseGraphSpine is just a semigroup.
        (Maybe (ClauseGraphSpine TaggedBnfcIdent))
        SymbolTable
        m a )
        }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadState SymbolTable
    , MonadReader (ClausesGraph TaggedBnfcIdent)
    , MonadWriter (Maybe (ClauseGraphSpine TaggedBnfcIdent))
    , MonadFix 
    , MonadRWS 
        (ClausesGraph TaggedBnfcIdent) 
        (Maybe (ClauseGraphSpine TaggedBnfcIdent)) 
        SymbolTable
    , MonadTrans 
    )


tieTypeClauseKnot :: 
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    TieTypeClause ()
tieTypeClauseKnot clauses = do
    args' <- lift $ typeClausesArgs clauses
    equality %= 
        ((map (view taggedBnfcIdentName &&& review _SymEntry . (,SymTypeVar) . view uniqueTag ) 
            args')++)
        -- add the variables to the scope..
    f args' (NE.toList clauses)
  where
    f :: [TaggedBnfcIdent] -> [TypeClause () () () BnfcIdent BnfcIdent] -> TieTypeClause ()
    f args [] = return ()
    f args (TypeClause name _ stv phrases () : rst) = do
        res <- ask

        name' <- lift $ tagBnfcIdent name
        stv'  <- lift $ tagBnfcIdent stv
        rec let clause = TypeClause name' args stv' phrases' (ClausesKnot res)
            tell (Just $ clause :| [])
            -- we do not replace the actual definiition because in type checking,
            -- we will need to substitite the variable anyways with its own
            -- distinct type variables... 
            {-
            tieTypeClauseSymTable %= (( stv' ^. taggedBnfcIdentName
                        , _SymEntry # (stv' ^. uniqueTag
                        , _SymTypeClause # (clause))):)
                        -}
            equality %= (( stv' ^. taggedBnfcIdentName
                        , _SymEntry # (stv' ^. uniqueTag
                        , _SymTypeVar # ())):)
            f args rst
            phrases' <- mapM (g clause) phrases
        return ()

    g clause (TypePhrase () ident fromtys toty) = do
        symtab <- guse equality  

        fromtys' <- lift $ traverse (substituteTyVar symtab) fromtys
        toty' <- lift $ substituteTyVar symtab toty
        ident' <- lift $ tagBnfcIdent ident
        return $ TypePhrase
            (ClausePhraseKnot clause)
            ident'
            fromtys'
            toty'

substituteTyVar :: 
    SymbolTable -> 
    Type () BnfcIdent BnfcIdent -> 
    GraphGenCore (TypeG TaggedBnfcIdent)
substituteTyVar symtab = para f 
  where
    f (TypeWithArgsF ident () args) = do
        args' <- traverse snd args
        ~(SymEntry uniquetag info) <- lookupIdent symtab ident

        return $ case info of 
            SymTypeVar -> TypeVar 
                (_TaggedBnfcIdent # (ident, uniquetag)) args'
            SymClause clauseg -> TypeWithArgs
                (_TaggedBnfcIdent # (ident, uniquetag))
                (TypeClauseNode clauseg) args'

    f (TypeVarF ident (a:as)) = error "higher kinded data not supported yet.."
    f (TypeVarF ident []) = do
        -- TODO literally does NOT support anything with higher kinded data!
        -- in the future, change it so that it will substitute and check arity!
        ~(SymEntry uniquetag info) <- lookupIdent symtab ident

        return $ case info of
            SymTypeVar -> _TypeVar # (_TaggedBnfcIdent # (ident, uniquetag), [])
            SymClause clauseg -> TypeWithArgs
                (_TaggedBnfcIdent # (ident,uniquetag))
                (TypeClauseNode clauseg) []
    f (TypeSeqF n) = TypeSeq <$> case n of
        TypeTupleF (a, b :| rst) -> do
            (a, rst) <- (,) <$> snd a <*> ((:|) <$> snd b <*> traverse snd rst)
            return $ TypeTupleF (a,rst)
        TypeListF ty -> TypeListF <$> snd ty
        -- unique id of built in types do not matter, so we just assign it a new one (so it type checks..)
        TypeIntF ident -> review _TypeIntF 
            . review _TaggedBnfcIdent 
            . (ident,) <$> freshUniqueTag
        TypeCharF ident -> review _TypeCharF 
            . review _TaggedBnfcIdent 
            . (ident,) <$> freshUniqueTag
        TypeDoubleF ident -> review _TypeDoubleF 
            . review _TaggedBnfcIdent 
            . (ident,) <$> freshUniqueTag
        TypeStringF ident -> review _TypeStringF 
            . review _TaggedBnfcIdent 
            . (ident,) <$> freshUniqueTag
        TypeUnitF ident -> review _TypeUnitF 
            . review _TaggedBnfcIdent 
            . (ident,) <$> freshUniqueTag
        -- TODO: implement this / give it more thought...
        -- TypeArrF ident from to -> review _TypeArrF 
        
        {-
        -- Duplicated code..
        TypeIntF ident -> do
            entry <- guses tieTypeClauseSymTable (lookupIdent ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ _TypeIntF # _TaggedBnfcIdent # (ident, uniquetag)
                Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeIntF # ident

        TypeCharF ident -> do
            entry <- guses tieTypeClauseSymTable (lookupIdent ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ _TypeCharF # _TaggedBnfcIdent # (ident, uniquetag)
                Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeCharF # ident
        TypeDoubleF ident -> do
            entry <- guses tieTypeClauseSymTable (lookupIdent ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ _TypeDoubleF # _TaggedBnfcIdent # (ident, uniquetag)
                Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeDoubleF # ident
        TypeStringF ident -> do
            ~entry <- guses tieTypeClauseSymTable (lookupIdent ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ _TypeStringF # _TaggedBnfcIdent # (ident, uniquetag)
                Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeStringF # ident
        TypeUnitF ident -> do
            ~entry <- guses tieTypeClauseSymTable (lookupIdent ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ _TypeUnitF # _TaggedBnfcIdent # (ident, uniquetag)
                Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeUnitF # ident
        -}


    f (TypeConcF n) = TypeConc <$> case n of
        -- TODO -- either implement these cases ore generalize it so it all is a lookup!
        TypeGetF ident a b -> review _TypeGetF <$> 
            ((,,) <$> (review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag) <*> snd a <*> snd b)
        TypePutF ident a b -> review _TypePutF <$> 
            ((,,) <$> (review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag) <*> snd a <*> snd b)
        TypeTensorF ident a b -> review _TypeTensorF <$> 
            ((,,) <$> (review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag) <*> snd a <*> snd b)
        TypeParF ident a b -> review _TypeParF <$> 
            ((,,) <$> (review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag) <*> snd a <*> snd b)
        TypeTopBotF ident -> review _TypeTopBotF <$> (review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag)
        TypeNegF ident t -> review _TypeNegF <$> ((,)
            <$> (review _TaggedBnfcIdent . (ident,) <$> freshUniqueTag)
            <*> snd t)


    lookupIdent ::
        SymbolTable ->
        BnfcIdent ->
        GraphGenCore (SymEntry SymInfo)
    lookupIdent symtab ident = do
        entries <- querySymbolTableBnfcIdentName ident symtab
        snd . fromJust <$> ambiguousLookupCheck entries


    
typeClausesArgs ::
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    GraphGenCore [TaggedBnfcIdent]
typeClausesArgs clause@(TypeClause name args stv phrases () :| rst) = traverse (\n -> tagBnfcIdent n) args

