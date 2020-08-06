{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Data.Functor.Foldable hiding (fold)

import Data.Map ( Map (..) )
import qualified Data.Map as Map

import Data.Set ( Set (..) )
import qualified Data.Set as Set

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS

import Data.Function
import Control.Applicative
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Foldable
import Data.List
import Data.Bool

import Control.Arrow
import Debug.Trace

data TieDefnsTState = TieDefnsTState {
    _tieDefnsStateSymbolTable :: SymbolTable
    , _tieDefnsTypeEqnsPkg :: TieDefnsTypeEqnsPkg
}

data TieDefnsTypeEqnsPkg = TieDefnsTypeEqnsPkg  {
    _tieDefnsTypeForall :: [TypeTag]
    , _tieDefnsTypeExist :: [TypeTag]
    , _tieDefnsTypeEqns :: [TypeEqns TaggedBnfcIdent TypeTag]
}  

instance Semigroup TieDefnsTypeEqnsPkg where
    TieDefnsTypeEqnsPkg a0 b0 c0 <> TieDefnsTypeEqnsPkg a1 b1 c1 =
        TieDefnsTypeEqnsPkg (a0 <> a1) (b0 <> b1) (c0 <> c1)

instance Monoid TieDefnsTypeEqnsPkg where
    mempty = TieDefnsTypeEqnsPkg [] [] []
    

data ExprTypeTags = ExprTypeTags {
    _exprTtype :: TypeTag
    , _exprTtypeInternal :: TypeTag
}  deriving Show

data TieExprEnv = TieExprEnv {
    _tieExprEnvTypeTags :: ExprTypeTags
    , _tieExprEnvTagTypeMap :: TagTypeMap
}


$(concat <$> traverse makeLenses 
    [ ''TieExprEnv ] 
 )
$(makePrisms ''TieExprEnv)
$(makeClassy ''ExprTypeTags)
$(makePrisms ''ExprTypeTags)

$(makeLenses ''TieDefnsTState)
$(makeLenses ''TieDefnsTypeEqnsPkg)

newtype TieDefnsT m a = 
    TieDefnsT { unTieDefnsT :: RWST 
            TagTypeMap 
            [DefnG TaggedBnfcIdent TypeTag TaggedChIdent] 
            TieDefnsTState 
            m a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState TieDefnsTState
    , MonadReader TagTypeMap 
    , MonadWriter [DefnG TaggedBnfcIdent TypeTag TaggedChIdent] 
    , MonadRWS TagTypeMap [DefnG TaggedBnfcIdent TypeTag TaggedChIdent] TieDefnsTState  
    , MonadTrans 
    )

type TieDefns a = TieDefnsT GraphGenCore a

runTieDefnsT :: 
    TagTypeMap ->
    TieDefnsTState ->
    TieDefns a -> 
    GraphGenCore (a, TieDefnsTState, [DefnG TaggedBnfcIdent TypeTag TaggedChIdent])
runTieDefnsT tagmap st (TieDefnsT m) = runRWST m tagmap st

newtype TieFunT m a =
    TieFunT { 
        unTieFunT :: ReaderT TieExprEnv (StateT SymbolTable m) a
    }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState SymbolTable
    , MonadReader TieExprEnv)
instance MonadTrans TieFunT where
    lift = TieFunT . lift . lift 

type TieFun a = TieFunT GraphGenCore a

runTieFun ::
    TieExprEnv ->
    SymbolTable ->
    TieFun a ->
    GraphGenCore (a, SymbolTable)
runTieFun env sym = flip runStateT sym 
    . flip runReaderT env 
    . unTieFunT

type ChCxt = [(String, SymEntry SymChInfo)]

data TieProcSt = TieProcSt {
    _tieProcSymTab :: SymbolTable
    , _tieProcChCxt :: ChCxt
}
$(makeLenses ''TieProcSt)

newtype TieProcT m a =
    TieProcT {
        unTieProcT :: ReaderT TagTypeMap (StateT TieProcSt m) a
    }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState TieProcSt
    , MonadReader TagTypeMap )

instance MonadTrans TieProcT where
    lift = TieProcT . lift . lift

type TieProc a = TieProcT GraphGenCore a 

runTieProc ::
    TagTypeMap ->
    TieProcSt ->
    TieProc a ->
    GraphGenCore (a, TieProcSt)
runTieProc tagmap st = flip runStateT st 
    . flip runReaderT tagmap 
    . unTieProcT 

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
            -- data Test(A,B) -> C = 
            --      Test :: A,B -> C
            -- Then, a function signature like:
            --      fun test :: -> Test =
            -- WILL COMPILE when it shouldn't really.... Although
            -- this will most /likely/ fail when unifying with anything since we 
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
            -- HOWEVER, currently the system does indeed permit this..
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

freshExprTypeTags :: GraphGenCore ExprTypeTags
freshExprTypeTags = ExprTypeTags <$> freshTypeTag <*> freshTypeTag

instance HasExprTypeTags TieExprEnv where
    exprTypeTags = tieExprEnvTypeTags

-- This will propogate the bound channels and free channels in a Set and remove
-- the bound variables from the free channels to compute the free variables 
-- that should be present in the declaration
-- So, the tuple returned by 'f' is (Command with the deduced variables, (bound channels, free channels) )
processCommandComputeVariableDeclarations ::
    ProcessCommandI BnfcIdent ->
    GraphGenCore (ProcessCommandI BnfcIdent)
processCommandComputeVariableDeclarations = fmap fst . cata f
  where
    f (CCloseF ident) = return (CClose ident, (Set.empty, Set.singleton ident))
    f (CHaltF ident) = return (CHalt ident, (Set.empty, Set.singleton ident))

    f (CGetF pat ident) = return (CGet pat ident, (Set.empty, Set.singleton ident))
    f (CPutF expr ident) = return (CPut expr ident, (Set.empty, Set.singleton ident))

    f (CSplitF spliton (a,b)) = return (CSplit spliton (a,b), (Set.fromList [a,b], Set.singleton spliton))

    -- TODO support this stuff..
    f (CForkF forkon ((a, _ :_, acmds), (b, _, bcmds))) = 
        error "provided context in fork instruction not supported"
    f (CForkF forkon ((a, _, acmds), (b, _:_, bcmds))) = 
        error "provided context in fork instruction not supported"
    f (CForkF forkon ((a, awithchs, acmds), (b, bwithchs, bcmds))) = do
        acmds' <- sequenceA acmds 
        bcmds' <- sequenceA bcmds 
        let afree = traceShowId $ computeFreeVariables (Set.fromList [forkon, a]) $ fmap snd acmds'
            bfree = traceShowId $ computeFreeVariables (Set.fromList [forkon, b]) $ fmap snd bcmds'
            common = afree `Set.intersection` bfree 
        tell $ bool [ _ForkNonDisjointChannels # Set.toList common ] [] $ has _Empty common

        return 
            ( CFork forkon ((a, Set.toList afree, fmap fst acmds'), (b, Set.toList bfree, fmap fst bcmds'))
            , (Set.singleton forkon , afree `Set.union` bfree ) )

    computeFreeVariables cxt = 
        fold
        . snd
        . mapAccumL 
            (\acc (bound,free) -> (acc `Set.union` bound, free `Set.difference` acc))
            cxt


