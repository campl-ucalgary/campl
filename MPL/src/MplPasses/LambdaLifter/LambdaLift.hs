{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.LambdaLifter.LambdaLift where

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled
import MplAST.MplLambdaLifted
import MplAST.MplProgUtil 

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Functor.Foldable hiding  (fold)
import Control.Arrow

import Data.Foldable
import Optics

import Data.Maybe
import Data.Functor.Const

import Data.List.NonEmpty ( NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import MplPasses.LambdaLifter.LambdaLiftUtil

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


import Unsafe.Coerce

lambdaLiftProg ::
    MplProg MplPatternCompiled ->
    [MplDefn MplLambdaLifted]
lambdaLiftProg = undefined

lambdaLiftStmt ::
    MplStmt MplPatternCompiled ->
    [MplDefn MplLambdaLifted]
lambdaLiftStmt = undefined

runLambdaLiftDefn ::
    MplDefn MplPatternCompiled -> 
    [MplDefn MplLambdaLifted]
runLambdaLiftDefn defn = case defn of
    ObjectDefn _ -> [unsafeCoerce defn]
    FunctionDefn fun -> 
        uncurry (:)
        $ runWriter
        $ flip runReaderT (LambdaLiftEnv tpmap callgraph) 
        $ lambdaLiftDefn defn
      where
        funexpr = fun ^. funDefn % to NE.head % _2

        callgraph = callGraphFixedPoint $ callGraphLetsGather funexpr <> callGraphFunDefn fun
        tpmap = mkVarsTpMap funexpr

    ProcessDefn proc -> 
        uncurry (:)
        $ runWriter 
        $ flip runReaderT (LambdaLiftEnv tpmap callgraph) 
        $ lambdaLiftDefn defn
      where
        proccmds = proc ^. procDefn % to NE.head % _2 

        callgraph =  callGraphFixedPoint $ foldMap callGraphLetsGather proccmds
        tpmap = foldMap mkVarsTpMap proccmds


lambdaLiftDefn :: 
    ( HasLambdaLiftEnv r
    , MonadReader r m 
    , MonadWriter [MplDefn MplLambdaLifted] m
    ) => 
    MplDefn MplPatternCompiled -> 
    m (MplDefn MplLambdaLifted)
lambdaLiftDefn defn  = case defn of
    ObjectDefn _ -> pure $ unsafeCoerce defn
    FunctionDefn (MplFunction funname funtp ((patts, defn) :| _)) -> do
        ~(Just (bound, free, calls)) <- gview (lambdaLiftCallGraph % at funname)
        undefined

    ProcessDefn proc -> undefined


-- | computes the fixed point for the call graph
callGraphFixedPoint :: 
    CallGraph -> 
    CallGraph 
callGraphFixedPoint = until (uncurry (==) <<< go &&& id) go
  where
    go :: CallGraph -> CallGraph
    go = uncurry (foldr f) <<< id &&& Map.keys

    f :: FunName -> CallGraph -> CallGraph
    f fun acc = flip (maybe acc) (acc ^? at fun % _Just % _3) $ foldr g acc
      where
        -- @fun@ calls @calls@
        g :: FunName -> CallGraph -> CallGraph
        g calls acc = flip (maybe acc) (acc ^? at calls % _Just % _2) $ \callsfrees -> 
            acc & at fun % _Just %~ \(funbound, funfree, funcalls) -> 
            (funbound, funfree `Set.union` (callsfrees `Set.difference` funbound), funcalls)

{- | computes the call graph of a function definition by collecting: 
        - bound variables 
        - free variables 
        - called functions
Note: this does NOT recurse.
 -}
callGraphFunDefn :: XFunctionDefn MplPatternCompiled -> CallGraph
callGraphFunDefn (MplFunction funName funTp ((patts, expr) :| [])) = 
    Map.singleton funName (args, vars, args `Set.difference` vars) 
  where
    args = Set.fromList $ fmap getPattVarIdent patts 
    vars = collectVarsExpr expr
    calls = collectCallsExpr expr
callGraphFunDefn _ = error "incorrect pattern compilation"

callGraphLetsGather :: 
    TraverseMplExpr t MplPatternCompiled => 
    t MplPatternCompiled  -> 
    CallGraph
callGraphLetsGather = execWriter . traverseMplExpr go
  where
    -- unfortunately, can't just use 'Const' here since the traversal unfortunately needs
    -- the monad instance.. of course, we could write out the instance completely to not 
    -- use the monad, but that's a little bit tedious to write out the traversal completely
    -- without relying on 'Data.Functor.Foldable'
    go :: MplExpr MplPatternCompiled -> Writer CallGraph (MplExpr MplPatternCompiled)
    go = \case 
        ELet ann stmts expr -> 
            return $ ELet ann stmts expr
          where
            f stmt = concatMap f (stmt ^. stmtWhereBindings) <> NE.toList (fmap g (stmt ^. stmtDefns))

            g res = case res of
                FunctionDefn fun -> do
                    tell $ callGraphFunDefn fun
                    return res
                _ -> return res

        res -> pure res


{- | Gets a map of ALL variables types (including those in let bindings) -}
mkVarsTpMap :: 
    TraverseMplExpr t MplPatternCompiled => 
    t MplPatternCompiled -> 
    Map IdentT (MplType MplTypeChecked)
mkVarsTpMap = execWriter . traverseMplExpr go
  where
    go :: MplExpr MplPatternCompiled ->
        Writer (Map IdentT (MplType MplTypeChecked)) (MplExpr MplPatternCompiled)
    go = \case
        EVar tp v -> tell (Map.singleton v tp) >> pure (EVar tp v)
        res -> return res


{- | Gets all the variables in an expression (note this DOES NOT recurse through the let bindings) -}
collectVarsExpr :: 
    MplExpr MplPatternCompiled -> 
    FreeArgs
collectVarsExpr = cata go
  where
    go :: MplExprF MplPatternCompiled FreeArgs -> FreeArgs
    go = \case
        EVarF _ v -> Set.singleton v
        res -> fold res

{- | Gets all the functions called in an expression (note this DOES NOT recurse through the let bindings) -}
collectCallsExpr :: 
    MplExpr MplPatternCompiled -> 
    Set FunName
collectCallsExpr = cata go
  where
    go :: MplExprF MplPatternCompiled FreeArgs -> FreeArgs
    go = \case
        ECallF _ idp rst -> idp `Set.insert` fold rst
        res -> fold res
