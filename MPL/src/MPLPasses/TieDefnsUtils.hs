{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module MPLPasses.TieDefnsUtils where

import Optics 
import Optics.State
import Optics.State.Operators

import Control.Applicative

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLUtil.UniqueSupply
import MPLPasses.SymbolTable
import MPLPasses.TieDefnsTypes
import MPLPasses.TieDefnsErrors
import MPLPasses.Unification
import MPLPasses.GraphGenCore

import MPLPasses.Unification

import Data.Functor.Foldable

import Data.Map ( Map (..) )
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Data.Function
import Control.Applicative
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Foldable
import Data.List

import Control.Arrow
import Debug.Trace

clauseSubstitutions :: 
    ( MonadState s m 
    , HasUniqueSupply s ) =>
    TypeClauseG TaggedBnfcIdent ->
    m ( TypeGTypeTag
      , [TypeTag]
      , [(TaggedBnfcIdent, TypeGTypeTag)] )
    -- ( Clause type of the of the statevar
    -- , fresh vars used to substitte
    -- , substition list of unique tags to corresponsing types
        -- ( this includes the state variables )
clauseSubstitutions clauseg = do
    clauseargsubs <- traverse 
        (\n -> second TypeTag . (n,) <$> freshUniqueTag) 
        (clauseg ^. typeClauseArgs) 

    let clausestatevartype = _TypeWithArgs # 
            ( clauseg ^.  typeClauseName
            , TypeClauseCallDefKnot clauseg
            , map snd argsubstitions )
        clausegraphspine = NE.toList $ 
            clauseg ^. typeClauseNeighbors % clauseGraph % clauseGraphSpine
        argsubstitions = map 
            (second (flip TypeVar [])) 
            clauseargsubs
        statevarsubstitiions = map 
            (\n -> 
                ( n ^. typeClauseStateVar
                , _TypeWithArgs # 
                    ( n ^. typeClauseName
                    , TypeClauseCallDefKnot n 
                    , map snd argsubstitions )
                )
            )
            clausegraphspine

    return 
        ( clausestatevartype
        , map snd clauseargsubs
        , statevarsubstitiions ++ argsubstitions)

-- | Gives us the fresh substititions for each of the type varaibles
-- and state variables for a clause graph
clauseGraphFreshSubstitutions ::
    ( MonadState s m 
    , HasUniqueSupply s ) =>
    ClausesGraph TaggedBnfcIdent -> 
    m ( [TypeTag]
    , [(TaggedBnfcIdent, TypeGTypeTag)])
    -- ( fresh type tags
    -- , substitutions)
    -- Note that the substitutions include the state variables and arguments
clauseGraphFreshSubstitutions clausegraph = do
    freshargs <- traverse (const freshTypeTag) argidents
    return (freshargs, zip argidents (map (flip TypeVar []) freshargs) )
  where
    argidents = statevars ++ typeargs
    statevars = NE.toList $ clauseGraphStateVars clausegraph
    typeargs = clauseGraphTypeArgs clausegraph


annotateTypeIToTypeGAndGenSubs :: 
    [(String, SymEntry SymSeqConcTypeInfo)] -> 
    Type () BnfcIdent BnfcIdent -> 
    GraphGenCore 
        ( [TypeTag]
        , [(TaggedBnfcIdent, TypeGTypeTag)]
        , TypeG TaggedBnfcIdent)
annotateTypeIToTypeGAndGenSubs symtab = f <=< flip evalStateT symtab . annotateTypeIToTypeGAndScopeFreeVars 
  where
    f typeg = do    
        (tags, subs) <- genTypeGSubs typeg
        return (tags, subs, typeg)

genTypeGSubs :: 
    TypeG TaggedBnfcIdent -> 
    GraphGenCore ([TypeTag], [(TaggedBnfcIdent, TypeGTypeTag)])
genTypeGSubs typeg = do 
    let idents = collectTypeGFreeVarsIdents typeg
    ttypes <- traverse (const freshTypeTag) idents
    return (ttypes, zip idents $ map (flip TypeVar []) ttypes)

collectTypeGFreeVarsIdents :: 
    TypeG TaggedBnfcIdent ->
    [TaggedBnfcIdent]
collectTypeGFreeVarsIdents = nubBy ((==) `on` view uniqueTag) .  toList


-- takes an interface type and annotates it with the symbol table
-- moreover, for free variables, it will scope them all appropriately
annotateTypeIToTypeGAndScopeFreeVars :: 
    Type () BnfcIdent BnfcIdent -> 
    StateT [(String, SymEntry SymSeqConcTypeInfo)] GraphGenCore (TypeG TaggedBnfcIdent)
annotateTypeIToTypeGAndScopeFreeVars = cata f
  where
    f :: 
        TypeF () BnfcIdent BnfcIdent (StateT [(String, SymEntry SymSeqConcTypeInfo)] GraphGenCore (TypeG TaggedBnfcIdent)) -> 
        StateT [(String, SymEntry SymSeqConcTypeInfo)] GraphGenCore (TypeG TaggedBnfcIdent)
    f (TypeVarF ident args) = f $ TypeWithArgsF ident () args
    f (TypeWithArgsF ident () args) = do
        args' <- sequenceA args
        symtab <- guse equality

        case lookupBnfcIdent ident symtab of
            -- TODO - we should be doing a kindcheck here i.e.
            -- if we have:
            -- data Test(A,B) -> C = Test :: A,B -> C
            -- Then, a function sig like fun test :: -> Test =
            -- WILL COMPILE when it shouldn't really.... Although
            -- this will fail when unifying with anything since we 
            -- implicitly insert the free type variables...
            Just (SymEntry tag pos entry) -> case entry ^? _SymSeqClause <|> entry ^? _SymConcClause of
                Just clauseg -> return $  
                    _TypeWithArgs #
                    ( _TaggedBnfcIdent # (ident, tag)
                    , TypeClauseCallDefKnot clauseg
                    , args'
                    )
                Nothing -> return $ 
                    TypeVar 
                        (TaggedBnfcIdent ident tag) 
                        args'

            {- 
            -- We need to test for infinite kind checks here..
            -- In otherwords, we should not have stuff like A(A,B)
                    -}
            Nothing -> do
                taggedident <- lift $ tagBnfcIdent ident
                ttype <- lift $ freshTypeTag

                equality %= (
                    ( taggedident ^. taggedBnfcIdentName
                    , _SymEntry # 
                        ( taggedident ^. uniqueTag
                        , taggedident ^. taggedBnfcIdentPos
                        ,  _SymTypeVar # ()
                        )
                    ):)
                --annotateStateFreeVarsTypeTags %= (ttype:)
                -- annotateStateFreeVarsSubs %= ((taggedident, TypeVar ttype []):)

                return $ TypeVar taggedident args'

    f (TypeSeqF seq) = do
        seq' <- sequenceA seq 
        case seq' of
            TypeSeqArrF froms tos -> return $ TypeSeq $ TypeSeqArrF froms tos
            _ -> error "not impletmendyet "
        -- return $ TypeSeq <$> sequenceA seq'
        -- fmap TypeSeq <$> sequenceA seq
    f (TypeConcF conc) = do
        conc' <- sequenceA conc
        TypeConc <$> lift (tagConcTypeF conc')
    

{-
tagTypeSeq :: 
    SeqTypeF BnfcIdent t ->
    GraphGenCore (SeqTypeF TaggedBnfcIdent t)
tagTypeSeq (TypeTupleF n) = return (TypeTupleF n)
tagTypeSeq (TypeIntF n) = return (TypeTupleF n)
-}

data ExprTypeTags = ExprTypeTags {
    _exprTtype :: TypeTag
    , _exprTtypeInternal :: TypeTag
}  deriving Show

data TieExprEnv = TieExprEnv {
    _tieExprEnvTypeTags :: ExprTypeTags
    , _tieExprEnvTagTypeMap :: TagTypeMap
}

$(makeLenses ''TieExprEnv)
$(makePrisms ''TieExprEnv)
$(makeClassy ''ExprTypeTags)
$(makePrisms ''ExprTypeTags)

freshExprTypeTags :: GraphGenCore ExprTypeTags
freshExprTypeTags = ExprTypeTags <$> freshTypeTag <*> freshTypeTag

instance HasExprTypeTags TieExprEnv where
    exprTypeTags = tieExprEnvTypeTags
