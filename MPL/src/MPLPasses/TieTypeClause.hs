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
import MPLPasses.SymbolTable
import MPLAST.MPLTypeAST
import MPLAST.MPLProgGraph
import MPLPasses.ToGraphTypes

import Data.Functor.Foldable

import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.State

import Control.Arrow

import Data.Maybe
import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Debug.Trace

data TieTypeClauseSymInfo =
    SymTypeVar
    | SymTypeClause TypeClauseNode
  deriving Show

data TieTypeClauseContext = TieTypeClauseContext  {
    _tieTypeClauseSymTable :: [(String, SymEntry TieTypeClauseSymInfo)]
    , _tieTypeClauseUniqueTagGen :: UniqueTag
} 

$(concat <$> traverse makeLenses 
    [ ''TieTypeClauseContext ]
 )

instance HasUniqueTag TieTypeClauseContext where
    uniqueTag = tieTypeClauseUniqueTagGen 

data TieTypeClauseError =
    TypeNotInScope (Type () BnfcIdent)
    | InvalidMutuallyRecursiveTypeArgDec (NonEmpty (TypeClause () () () BnfcIdent))
    | OverlappingTypeVariables (NonEmpty (TypeClause () () () BnfcIdent))
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
    NonEmpty (TypeClause () () () BnfcIdent) -> 
    Either e (UniqueTag, ClausesGraph TypeClauseNode TaggedBnfcIdent)
    -- Either TieTypeClauseError (UniqueTag, ClausesGraph TypeClauseNode TaggedBnfcIdent)
makeTypeClauseGraph obj cxt clause = mdo
    let clausegraph = _ClausesGraph # (obj, fromJust res)
    ((), st, res) <- runRWST (tieTypeClauseKnot clause) clausegraph cxt
    return $ (st ^. uniqueTag, clausegraph)

type TypeClauseKnotTying a = forall e. 
    AsTieTypeClauseError e => 
        RWST 
        (ClausesGraph TypeClauseNode TaggedBnfcIdent)
        (Maybe (ClauseGraphSpine TypeClauseNode TaggedBnfcIdent))
        TieTypeClauseContext
        (Either e)
        a

tieTypeClauseKnot :: 
    NonEmpty (TypeClause () () () BnfcIdent) ->
    TypeClauseKnotTying ()
tieTypeClauseKnot clauses = do
    args' <- typeClausesArgs clauses
    tieTypeClauseSymTable %= 
        ((map (view taggedBnfcIdentName &&& review _SymEntry . (,SymTypeVar) . view uniqueTag ) args')++)
        -- add the variables to the scope..
    f args' (NE.toList clauses)
  where
    f :: [TaggedBnfcIdent] -> [TypeClause () () () BnfcIdent] -> TypeClauseKnotTying ()
    f args [] = return ()
    f args (TypeClause name _ stv phrases () : rst) = do
        res <- ask

        name' <- tagBnfcIdent name
        stv'  <- tagBnfcIdent stv
        rec let clause = TypeClause name' args stv' phrases' (ClausesKnot res)
            tell (Just $ clause :| [])
            tieTypeClauseSymTable %= (( stv' ^. taggedBnfcIdentName
                        , _SymEntry # (stv' ^. uniqueTag
                        , _SymTypeClause # (TypeClauseNode clause))):)
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

    substituteTyVar :: Type () BnfcIdent -> 
        TypeClauseKnotTying (Type TypeClauseNode TaggedBnfcIdent)
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
                    SymTypeClause node ->  return $ TypeWithArgs
                        (_TaggedBnfcIdent # (ident,uniquetag))
                        node args'
                Nothing -> throwError 
                    $ review _TypeNotInScope (TypeWithArgs ident () (map fst args))
        f (TypeVarF ident) = do
            entry <- guses tieTypeClauseSymTable (lookupSymTable ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ _TypeVar # _TaggedBnfcIdent # (ident, uniquetag)
                    SymTypeClause node -> return $ TypeWithArgs
                        (_TaggedBnfcIdent # (ident,uniquetag))
                        node []
                Nothing -> throwError $ _TypeNotInScope # TypeVar ident
        f (TypeSeqF n) = TypeSeq <$> case n of
            TypeTupleF (a, b :| rst) -> do
                (a, rst) <- (,) <$> snd a <*> ((:|) <$> snd b <*> traverse snd rst)
                return $ TypeTupleF (a,rst)
            TypeListF ty -> TypeListF <$> snd ty

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

        f (TypeConcF n) = TypeConc <$> case n of
            -- TODO -- either implement these cases ore generalize it so it all is a lookup!
            _ -> undefined
            


                    

    lookupSymTable ident [] = Nothing
    lookupSymTable ident (~(str, entry):as) 
        | ident ^. bnfcIdentName == str = Just entry 
        | otherwise = lookupSymTable ident as

    
typeClausesArgs ::
    NonEmpty (TypeClause () () () BnfcIdent) ->
    TypeClauseKnotTying [TaggedBnfcIdent]
typeClausesArgs clause@(TypeClause name args stv phrases () :| rst) 
    | mutuallyrecursivevalidity && overlappingargsvalidity =
        mapM tagBnfcIdent args
    | not mutuallyrecursivevalidity = throwError $ 
        review _InvalidMutuallyRecursiveTypeArgDec clause
    -- | not overlappingargsvalidity = throwError $ 
    | otherwise = throwError $ 
        review _OverlappingTypeVariables clause
    -- we can get better error messages if we spread it out
    -- and test overlappingargsvalidity for all clauses
  where
    focusedargsnames = map (view bnfcIdentName) args
    otherclauseargs = map (map (view bnfcIdentName) . view typeClauseArgs) rst
    -- mutually recursive things MUST have the same type variables (as part of 
    -- the programming language specification)..
    mutuallyrecursivevalidity = all (==focusedargsnames) otherclauseargs
    overlappingargsvalidity = length (nub args) == length args


emptyClauseContext = TieTypeClauseContext [] (UniqueTag 0)
