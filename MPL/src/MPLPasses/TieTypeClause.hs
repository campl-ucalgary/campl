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
    -- AsTieTypeClauseError e =>
    ObjectType -> 
    TieTypeClauseContext -> 
    NonEmpty (TypeClause () () () BnfcIdent) -> 
    -- Either e (UniqueTag, ClausesGraph TypeClauseNode TaggedBnfcIdent)
    Either TieTypeClauseError (UniqueTag, ClausesGraph TypeClauseNode TaggedBnfcIdent)
makeTypeClauseGraph obj cxt clause = mdo
    ((), st, res) <- runRWST (tieTypeClauseKnot clause) res cxt
    return $ (st ^. uniqueTag, res)

type TypeClauseKnotTying a = forall e. 
    --AsTieTypeClauseError e => 
        RWST 
        (ClausesGraph TypeClauseNode TaggedBnfcIdent)
        (ClausesGraph TypeClauseNode TaggedBnfcIdent)
        TieTypeClauseContext
        (Either TieTypeClauseError) 
        a

tieTypeClauseKnot :: 
    NonEmpty (TypeClause () () () BnfcIdent) ->
    TypeClauseKnotTying ()
tieTypeClauseKnot clauses = do
    args' <- typeClausesArgs clauses
    f args' (NE.toList clauses)
  where
    f :: [TaggedBnfcIdent] -> [TypeClause () () () BnfcIdent] -> TypeClauseKnotTying ()
    f args [] = return ()
    f args (TypeClause name _ stv phrases () : rst) = do
        res <- ask

        name' <- tagBnfcIdent name
        stv'  <- tagBnfcIdent stv
        rec let clause = TypeClause name' args stv' phrases' (ClausesKnot res)
            -- clauseGraphSpine %= ( _ ((ClausesGraph [clause])<>))
            tell (ClausesGraph [clause])
            tieTypeClauseSymTable %= (( stv' ^. taggedBnfcIdentName
                        , review _SymEntry (stv' ^. uniqueTag
                        , review _SymTypeClause (TypeClauseNode clause))):)
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
            entry <- guses tieTypeClauseSymTable
                    (lookupSymTable ident) 
            case entry of
                Just (SymEntry uniquetag n) -> case n of
                    SymTypeVar -> return $ TypeWithArgs 
                        (review _TaggedBnfcIdent (ident, uniquetag)) 
                        TypeClauseTerminal args'
                    SymTypeClause node ->  return $ TypeWithArgs
                        (review _TaggedBnfcIdent (ident,uniquetag))
                        node args'
                Nothing -> throwError 
                    $ review _TypeNotInScope (TypeWithArgs ident () (map fst args))
        f (TypeVarF ident) = do
            undefined

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
    | not overlappingargsvalidity = throwError $ 
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

testt = makeTypeClauseGraph DataObj emptyClauseContext heknewthewholetime


heknewthewholetime = NE.fromList [
    TypeClause (mkident "Clause") [] (mkident "ST")
        [ TypePhrase ()
            (mkident "Phrase")
            []
            (TypeWithArgs (mkident "Potato") () [])
        ]
        ()
    , TypeClause (mkident "Clause2") [] (mkident "Potato")
        [ TypePhrase ()
            (mkident "Phrase2")
            []
            (TypeWithArgs (mkident "ST") () [])
        ]
        ()
    ]
  where
    mkident str = (review _BnfcIdent (str, (1,1)))




{-
testingtie :: 
    [TypeClause () () () String] ->
    TypeClauseKnotTying ()
    -- State [(String, SymEntry)] [TypeClauseKnot SymEntry String]  
testingtie [] = return ()
testingtie ((TypeClause a args stv phrases ()):as) = do
    env <- ask
    rec let clause = TypeClause a args stv phrases' (ClausesKnot env)
        modify ((stv, SymTypeClause $ TypeClauseNode clause):)
        tell [ clause ]
        testingtie as
        phrases' <- mapM (f clause) phrases
    return ()

  where
    f clause (TypePhrase () ident [] toty) = do
        toty' <- getTypeVar toty
        return $ TypePhrase 
            (ClausePhraseKnot clause)
            ident
            []
            toty'

    getTypeVar :: 
        Type () String -> 
        TypeClauseKnotTying (Type TypeClauseNode String)
    getTypeVar (TypeWithArgs ident () args) = mdo
        v <- gets (foldr lookupp Nothing)
        case v of 
            Just v' -> return (TypeWithArgs ident v' [])
            Nothing -> throwError $ "AHAHAH IN ETHERERROR" ++ ident
      where
        lookupp (a, v) (Just n) = (Just n)
        lookupp (a, v) Nothing
            | a == ident = case v of
                    SymTypeClause n -> Just $ n
                    _ -> Nothing
            | otherwise = Nothing

handtiedd = [clause]
  where
    clause = TypeClause "Clause" [] "ST"
        [ TypePhrase 
            (ClausePhraseKnot $ clause)
            "Phrase"
            []
            (TypeWithArgs "ST" (TypeClauseNode $ clause) [])
        ]
        (ClausesKnot handtiedd)


heknewthewholetime = [
    TypeClause "Clause" [] "ST"
        [ TypePhrase ()
            "Phrase"
            []
            (TypeWithArgs "ST" () [])
        ]
        ()
    , TypeClause "Clause2" [] "Potato"
        [ TypePhrase ()
            "Phrase2"
            []
            (TypeWithArgs "ST" () [])
        ]
        ()
    ]


isitreal = case evaluated of
            [TypeClause _ _ _ phrase b, _ ] -> 
                case phrase of
                    [TypePhrase ctxt ident [] 
                        (TypeWithArgs argident to [])] ->  
                            case to of
                                TypeClauseNode n -> n ^. typeClauseName
isitreal2 = case evaluated of
            [TypeClause _ _ _ phrase b, _ ] -> 
                case phrase of
                    [TypePhrase ctxt ident [] 
                        (TypeWithArgs argident to [])] ->  
                            case to of
                                TypeClauseNode n -> ctxt
    
-}

