{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-}
module MPLPasses.TieTypeClause where

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

import qualified Data.Bifunctor as Bifunctor
import Control.Arrow

import Data.Maybe
import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Debug.Trace

data TieTypeClauseSymInfo =
    SymTypeVar
    -- | SymTypeClause (TypeClauseNode TaggedBnfcIdent)
    | SymTypeClause (TypeClauseG TaggedBnfcIdent)
  deriving Show

data TieTypeClauseContext = TieTypeClauseContext  {
    _tieTypeClauseSymTable :: [(String, SymEntry TieTypeClauseSymInfo)]
    , _tieTypeClauseUniqueTagGen :: UniqueTag
} 

$(concat <$> traverse makeLenses 
    [ ''TieTypeClauseContext ]
 )
$(concat <$> traverse makePrisms 
    [ ''TieTypeClauseContext ]
 )

instance HasUniqueTag TieTypeClauseContext where
    uniqueTag = tieTypeClauseUniqueTagGen 

data TieTypeClauseError =
    TypeNotInScope (Type () BnfcIdent BnfcIdent)
  deriving Show

$(concat <$> traverse makeClassyPrisms 
    [ ''TieTypeClauseError 
    , ''TieTypeClauseSymInfo
    ]
 ) 

makeTypeClauseGraph :: 
    AsTieTypeClauseError e =>
    ObjectType -> 
    TieTypeClauseContext -> 
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) -> 
    Either (NonEmpty e) (UniqueTag, ClausesGraph TaggedBnfcIdent)
    -- Either TieTypeClauseError (UniqueTag, ClausesGraph TypeClauseNode TaggedBnfcIdent)
makeTypeClauseGraph obj cxt clause = do
    rec let clausegraph = _ClausesGraph # (obj, fromJust res)
        ((), st, res) <- Bifunctor.first (:|[]) $ runRWST (tieTypeClauseKnot clause) clausegraph cxt
    return $ (st ^. uniqueTag, clausegraph)

type TypeClauseKnotTying a = forall e. 
    AsTieTypeClauseError e => 
        RWST 
        (ClausesGraph TaggedBnfcIdent)
        (Maybe (ClauseGraphSpine TaggedBnfcIdent))
        TieTypeClauseContext
        (Either e)
        a

tieTypeClauseKnot :: 
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    TypeClauseKnotTying ()
tieTypeClauseKnot clauses = do
    args' <- typeClausesArgs clauses
    tieTypeClauseSymTable %= 
        ((map (view taggedBnfcIdentName &&& review _SymEntry . (,SymTypeVar) . view uniqueTag ) args')++)
        -- add the variables to the scope..
    f args' (NE.toList clauses)
  where
    f :: [TaggedBnfcIdent] -> [TypeClause () () () BnfcIdent BnfcIdent] -> TypeClauseKnotTying ()
    f args [] = return ()
    f args (TypeClause name _ stv phrases () : rst) = do
        res <- ask

        name' <- tagBnfcIdent name
        stv'  <- tagBnfcIdent stv
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
            tieTypeClauseSymTable %= (( stv' ^. taggedBnfcIdentName
                        , _SymEntry # (stv' ^. uniqueTag
                        , _SymTypeVar # ())):)
            f args rst
            phrases' <- mapM (g clause) phrases
        return ()

    g clause (TypePhrase () ident fromtys toty) = do
        fromtys' <- mapM substituteTyVar fromtys
        toty' <- substituteTyVar toty
        ident' <- tagBnfcIdent ident
        return $ TypePhrase
            (ClausePhraseKnot clause)
            ident'
            fromtys'
            toty'

    substituteTyVar :: Type () BnfcIdent BnfcIdent -> 
        TypeClauseKnotTying (TypeG TaggedBnfcIdent)
    substituteTyVar = para f 
      where
        f (TypeWithArgsF ident () args) = do
            args' <- traverse snd args
            entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ TypeWithArgs 
                        (_TaggedBnfcIdent # (ident, uniquetag)) 
                        (_TypeClauseLeaf # ()) args'
                    SymTypeClause clauseg ->  return $ TypeWithArgs
                        (_TaggedBnfcIdent # (ident,uniquetag))
                        (TypeClauseNode clauseg) args'
                Nothing -> throwError 
                    $ review _TypeNotInScope (TypeWithArgs ident () (map fst args))
        f (TypeVarF ident) = do
            entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ _TypeVar # _TaggedBnfcIdent # (ident, uniquetag)
                    SymTypeClause clauseg -> return $ TypeWithArgs
                        (_TaggedBnfcIdent # (ident,uniquetag))
                        (TypeClauseNode clauseg) []
                Nothing -> throwError $ _TypeNotInScope # TypeVar ident
        f (TypeSeqF n) = TypeSeq <$> case n of
            TypeTupleF (a, b :| rst) -> do
                (a, rst) <- (,) <$> snd a <*> ((:|) <$> snd b <*> traverse snd rst)
                return $ TypeTupleF (a,rst)
            TypeListF ty -> TypeListF <$> snd ty
            -- unique id of built in types do not matter, so we just assign it a new one (so it type checks..)
            TypeIntF ident -> review _TypeIntF 
                .  review _TaggedBnfcIdent 
                .  (ident,) <$> freshUniqueTag
            TypeCharF ident -> review _TypeCharF 
                .  review _TaggedBnfcIdent 
                .  (ident,) <$> freshUniqueTag
            TypeDoubleF ident -> review _TypeDoubleF 
                .  review _TaggedBnfcIdent 
                .  (ident,) <$> freshUniqueTag
            TypeStringF ident -> review _TypeStringF 
                .  review _TaggedBnfcIdent 
                .  (ident,) <$> freshUniqueTag
            TypeUnitF ident -> review _TypeUnitF 
                .  review _TaggedBnfcIdent 
                .  (ident,) <$> freshUniqueTag
            -- TODO: implement this / give it more thought...
            -- TypeArrF ident from to -> review _TypeArrF 
            
            {-
            -- Duplicated code..
            TypeIntF ident -> do
                entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
                case entry of
                    Just (SymEntry uniquetag n) -> case n of
                        SymTypeVar -> return $ _TypeIntF # _TaggedBnfcIdent # (ident, uniquetag)
                    Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeIntF # ident

            TypeCharF ident -> do
                entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
                case entry of
                    Just (SymEntry uniquetag n) -> case n of
                        SymTypeVar -> return $ _TypeCharF # _TaggedBnfcIdent # (ident, uniquetag)
                    Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeCharF # ident
            TypeDoubleF ident -> do
                entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
                case entry of
                    Just (SymEntry uniquetag n) -> case n of
                        SymTypeVar -> return $ _TypeDoubleF # _TaggedBnfcIdent # (ident, uniquetag)
                    Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeDoubleF # ident
            TypeStringF ident -> do
                entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
                case entry of
                    Just (SymEntry uniquetag n) -> case n of
                        SymTypeVar -> return $ _TypeStringF # _TaggedBnfcIdent # (ident, uniquetag)
                    Nothing -> throwError $ _TypeNotInScope # _TypeSeq # _TypeStringF # ident
            TypeUnitF ident -> do
                entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
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
                    
    lookupSymTable ident [] = Nothing
    lookupSymTable ident (~(str, entry):as) 
        | ident ^. bnfcIdentName == str = Just entry 
        | otherwise = lookupSymTable ident as
    
typeClausesArgs ::
    NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) ->
    TypeClauseKnotTying [TaggedBnfcIdent]
typeClausesArgs clause@(TypeClause name args stv phrases () :| rst) = mapM tagBnfcIdent args

emptyClauseContext = TieTypeClauseContext [] (UniqueTag 0)
