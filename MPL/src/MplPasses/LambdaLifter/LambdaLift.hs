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
import Data.Traversable
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

import Control.Exception
import Debug.Trace

import Unsafe.Coerce

runLambdaLiftProg ::
    MplProg MplPatternCompiled ->
    MplProg MplLambdaLifted
runLambdaLiftProg (MplProg prog) = MplProg $ map runlambdaLiftStmt prog

runlambdaLiftStmt ::
    MplStmt MplPatternCompiled ->
    MplStmt MplLambdaLifted
    -- [MplDefn MplLambdaLifted]
runlambdaLiftStmt stmt = MplStmt
    (NE.fromList (foldMapOf (stmtDefns % folded) runLambdaLiftDefn stmt))
    (stmt ^..  stmtWhereBindings % folded % to runlambdaLiftStmt)

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

        callgraph =  callGraphFixedPoint $ callGraphLetsGather funexpr <> callGraphFunDefn fun
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
    LambdaLift 
        (MplDefn MplPatternCompiled)
        (MplDefn MplLambdaLifted)
lambdaLiftDefn defn  = case defn of
    ObjectDefn _ -> pure $ unsafeCoerce defn
    {- Hopefully this is quite straightforward. It is as follows:
        - we compute the extra free variables that need to be added to the function arguments
        - Then, we get all the types of those free variables
        - Then, we lambda lift the function body
        - Then, we put together the function again WITH the updated type of the arguments AND the variable patterns.
     -}
    -- we assert that the body should be null from compilation of pattern matching
    FunctionDefn (MplFunction funname funtp ((patts, expr) :| bdy)) -> assert (null bdy) $ do
        -- ~(Just (bound, free, calls)) <- gview (lambdaLiftCallGraph % at funname) 
        (bound, free, calls) <- gview (lambdaLiftCallGraph % at funname % to (fromMaybe mempty)) 

        let nfrees = Set.toList $ free `Set.difference` bound
        nfreestps <- magnify lambdaLiftTpMap $ for nfrees $ \nvar -> 
            fmap fromJust $ gview (at nvar)
            
        expr' <- lambdaLiftExpr expr

        return $ FunctionDefn $ MplFunction 
            funname 
            (funtp & _2 %~ (++nfreestps))
            ((patts ++ zipWith (curry (review _PVar)) nfreestps nfrees, expr') :| [])

    -- we assert that the body should be null from compilation of pattern matching
    -- Technically, we do NOT need to lift these, so we don't. 
    -- ProcessDefn (MplProcess procname proctp (((patts, ins, outs), cmds) :| bdy )) -> assert (null bdy) $ do
    ProcessDefn (MplProcess procname proctp ((args, cmds) :| bdy )) -> assert (null bdy) $ do
        cmds' <- traverse lambdaLiftCmd cmds 

        return $ ProcessDefn $ MplProcess 
            procname 
            proctp 
            ((args, cmds') :| [] )

lambdaLiftExpr :: 
    LambdaLift
        (MplExpr MplPatternCompiled)
        (MplExpr MplLambdaLifted)
lambdaLiftExpr = cata go 
  where
    go :: MplExprF MplPatternCompiled (_ (MplExpr MplLambdaLifted)) -> _ (MplExpr MplLambdaLifted)
    go = \case
        EPOpsF ann opty l r -> EPOps ann opty <$> l <*> r
        EVarF ann idp -> pure $ EVar ann idp
        EIntF ann v -> pure $ EInt ann v
        ECharF ann v -> pure $ EChar ann v
        EDoubleF ann v -> pure $ EDouble ann v
        EBoolF ann v -> pure $ EBool ann v
        ECaseF ann expr cases -> do
            expr' <- expr
            cases' <- sequenceOf (traversed % _2) cases
            return $ ECase ann expr' cases'
        EObjCallF ann idp exprs -> do
            exprs' <- sequenceA exprs
            return $ EObjCall ann idp exprs'
        ECallF ann idp exprs -> do
            exprs' <- sequenceA exprs

            -- get the extra free variables needed. this is duplciated code from above
            -- Note that sometimes this functino will not be in the callgraph because
            -- it was previously already lifted. e.g.
            -- @
            -- fun testing1 =
            --      a,b -> a
            --
            -- fun testing2 =
            --      a -> testing1(a,0)
            -- @
            -- testing2 will call testing1, but testing1 is not in this functions call graph
            -- ~(Just (bound, free, calls)) <- gview (lambdaLiftCallGraph % at idp) 
            (bound, free, calls) <- gview (lambdaLiftCallGraph % at idp % to (fromMaybe mempty)) 
            let nfrees = Set.toList $ free `Set.difference` bound
            nfreestps <- magnify lambdaLiftTpMap $ for nfrees $ \nvar -> fmap fromJust $ gview (at nvar)

            return $ ECall ann idp $ exprs' <> zipWith (curry (review _EVar)) nfreestps nfrees

        ERecordF ann records -> do
            records' <- sequenceOf (traversed % _3 % _2) records
            return $ ERecord ann records'
        EListF ann exprs -> EList ann <$> sequenceA exprs
        EStringF ann str -> pure $ EString ann str
        EUnitF ann -> pure $ EUnit ann
        ETupleF ann (t0,t1,ts) -> do
            ~(t0':t1':ts') <- sequenceA $ t0:t1:ts
            return $ ETuple ann (t0',t1',ts')
        EProjF ann proj expr -> EProj ann proj <$> expr 
        EBuiltInOpF ann optp l r -> EBuiltInOp ann optp <$> l <*> r
        EIfF ann cond thenc elsec -> EIf ann <$> cond <*> thenc <*> elsec
        ELetF ann stmts expr -> do
            traverse_ f stmts
            expr
          where
            f :: MplStmt MplPatternCompiled -> _ ()
            f stmt = do
                traverse_ f $ stmt ^. stmtWhereBindings
                {- Why is this filter here? 
                 - It's because if there is a concurrent definition here, there is no possible way to call it anyways, 
                 - so there's no need to lambda lift it (since let bindings
                 - only occur in an expression, and expressions may only call
                 - other sequential things)
                -}
                traverse_ (tell . pure <=< lambdaLiftDefn) $ NE.filter (hasn't _ProcessDefn) $ stmt ^. stmtDefns
        -- ESwitch !(XESwitch x) (NonEmpty (MplExpr x, MplExpr x))
        EIllegalInstrF ann -> pure $ EIllegalInstr ann
        -- XExpr !(XXExpr x)
        {-
  | EFold !(XEFold x)
          (MplExpr x)
          (NonEmpty (XEFoldPhrase x, IdP x, [XMplPattern x], MplExpr x))
  | EUnfold !(XEUnfold x)
            (MplExpr x)
            (NonEmpty
               (XEUnfoldPhrase x, XMplPattern x,
                NonEmpty (XEUnfoldSubPhrase x, IdP x, [XMplPattern x], MplExpr x)))
        -}


lambdaLiftCmd :: 
    LambdaLift
        (MplCmd MplPatternCompiled)
        (MplCmd MplLambdaLifted)
lambdaLiftCmd = cata go 
  where
    go :: MplCmdF MplPatternCompiled (_ (MplCmd MplLambdaLifted)) -> _ (MplCmd MplLambdaLifted)
    go = \case 
        CRunF ann idp seqs ins outs -> do
            seqs' <- traverse lambdaLiftExpr seqs
            return $ CRun ann idp seqs' ins outs 
        CCloseF ann chp -> pure $ CClose ann chp
        CHaltF ann chp -> pure $ CHalt ann chp
        CGetF ann patt chp -> pure $ CGet ann patt chp
        CPutF ann expr chp -> do
            expr' <- lambdaLiftExpr expr
            return $ CPut ann expr' chp
        CHCaseF ann chp cases -> CHCase ann chp <$> sequenceOf (traversed % _3 % traversed) cases
        CHPutF ann idp chp -> pure $ CHPut ann idp chp
        CSplitF ann chp splits -> pure $ CSplit ann chp splits
        CForkF ann chp forks -> CFork ann chp <$> sequenceOf (each % _3 % traversed) forks 
        CIdF ann eq -> pure $ CId ann eq
        CIdNegF ann eq -> pure $ CIdNeg ann eq
        CRaceF ann races -> CRace ann <$> sequenceOf (traversed % _2 % traversed) races
        CPlugsF ann phrases ->
            sequenceOf (_1 % _3 % traversed) phrases 
                >>= sequenceOf (_2 % _3 % traversed)
                >>= sequenceOf (_3 % traversed % _3 % traversed)
                >>= return . CPlugs ann 
        CCaseF ann expr cases -> do
            expr' <- lambdaLiftExpr expr
            cases' <- sequenceOf (traversed % _2 % traversed) cases
            return $ CCase ann expr' cases'
            
        CIfF ann cond thenc elsec -> 
            CIf ann 
                <$> lambdaLiftExpr cond
                <*> sequenceA thenc 
                <*> sequenceA elsec 

        CIllegalInstrF ann -> pure $ CIllegalInstr ann
    {-
  | CPlugF !(XCPlug x)
           ((XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r),
            (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty r))
  | CSwitchF !(XCSwitch x) (NonEmpty (XMplExpr x, NonEmpty r))
-}

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
-- we assert that the body should be null from compilation of pattern matching
callGraphFunDefn (MplFunction funName funTp ((patts, expr) :| bdy)) = assert (null bdy) $ 
    Map.singleton funName (args,  vars `Set.difference` args, calls) 
  where
    args = Set.fromList $ fmap getPattVarIdent patts 
    vars = collectFreeVarsExpr expr
    calls = collectCallsExpr expr

{- This will collect the call graph of every let binding -}
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
        ELet ann stmts expr ->  do
            traverse_ f stmts
            return $ ELet ann stmts expr
          where
            f stmt = traverse_ f (stmt ^. stmtWhereBindings) 
                >> traverse_ g (stmt ^. stmtDefns)

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


{- | Gets all the free variables in an expression (note this DOES NOT recurse through the let bindings). Note
the ``free variables'' part means that it removes variables bound by a case statement. 

N.B. Not too sure if I'm missing cases with other potential binders other than just case? TODO write some
test cases to get this resolved.
-}
collectFreeVarsExpr :: 
    MplExpr MplPatternCompiled -> 
    FreeArgs
collectFreeVarsExpr = cata go
  where
    go :: MplExprF MplPatternCompiled FreeArgs -> FreeArgs
    go = \case
        EVarF _ v -> Set.singleton v
        ECaseF _ caseon patts -> (caseon `Set.union` foldMap snd patts) `Set.difference` foldMap (f . fst) patts
          where
            f = \case
                PSimpleConstructor _ _ args -> Set.fromList $ map fst args
                PSimpleListCons _ l r -> Set.fromList $ [l, r]
                PSimpleListEmpty _ -> mempty
                PSimpleUnit _ -> mempty
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
