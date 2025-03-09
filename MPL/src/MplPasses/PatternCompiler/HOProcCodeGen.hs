{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module MplPasses.PatternCompiler.HOProcCodeGen where

import Control.Arrow
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Functor.Const
import Data.Functor.Foldable hiding (fold)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Debug.Trace
import MplAST.MplCore
import qualified MplAST.MplCore as MplPasses
import MplAST.MplLambdaLifted
import MplAST.MplParsed
import MplAST.MplPatternCompiled
import MplAST.MplProgUtil
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplPasses.Parser.ParseUtils
import MplPasses.PatternCompiler.HOProcCodeGenUtils
import MplPasses.Renamer.RenameUtils
import Optics
import Unsafe.Coerce

runHOProcCodeGenDefn ::
  MplDefn MplTypeChecked ->
  [MplDefn MplTypeChecked]
runHOProcCodeGenDefn defn = case defn of
  ObjectDefn _ -> [unsafeCoerce defn]
  _ ->
    uncurry (:) $
      runWriter $
        hoProcCodeGenDefn defn

hoProcCodeGenDefn ::
  HOProcCodeGen
    (MplDefn MplTypeChecked)
    (MplDefn MplTypeChecked)
hoProcCodeGenDefn defn = case defn of
  ProcessDefn (MplProcess procname proctp (((patts, ins, outs), cmds) :| bdy)) -> assert (null bdy) $ do
    let newInputName = modifyIdentRString (++ "_hidden_ctx") procname
    passAlongResult <- mapM passAlong ((patts,newInputName,) <$> cmds)
    let newPatts =
          if any fst passAlongResult
            then let newPatt = PVar (TypeBuiltIn (TypeUnitF Nothing)) newInputName in newPatt : patts
            else patts
    cmds' <- mapM passContext ((patts,newInputName,) <$> (snd <$> passAlongResult))
    return (ProcessDefn (MplProcess procname proctp (((newPatts, ins, outs), cmds') :| bdy)))
  _ -> pure $ unsafeCoerce defn

passContext ::
  HOProcCodeGen
    ([MplPattern MplTypeChecked], IdP MplTypeChecked, MplCmd MplTypeChecked)
    (MplCmd MplTypeChecked)
passContext (ctx, idp, cmd) = case cmd of
  CRun ann (Left id) seqs ins outs -> do
    liftResult <- mapM handleExpr ((ctx,idp,) <$> seqs)
    let fcall = ECall (TypeBuiltIn (TypeUnitF Nothing)) idp (map (\(PVar ann e) -> EVar ann e) ctx)
    let newSeqs = if any fst liftResult then fcall : (snd <$> liftResult) else snd <$> liftResult
    return $ CRun ann (Left id) newSeqs ins outs
  CHCase ann chp cases -> do
    cases' <-
      mapM
        ( \(p, id, cmds) -> do
            cmds' <- mapM passContext ((ctx,id,) <$> cmds)
            return (p, id, cmds')
        )
        cases
    return $ CHCase ann chp cases'
  CFork ann chp ((ch1, f1, cmds1), (ch2, f2, cmds2)) -> do
    cmds1' <- mapM passContext ((ctx,idp,) <$> cmds1)
    cmds2' <- mapM passContext ((ctx,idp,) <$> cmds2)
    return $ CFork ann chp ((ch1, f1, cmds1'), (ch2, f2, cmds2'))
  CRace ann races -> do
    races' <-
      mapM
        ( \(ch, cmds) -> do
            cmds' <- mapM passContext ((ctx,idp,) <$> cmds)
            return (ch, cmds')
        )
        races
    return $ CRace ann races'
  CPlugs ann (ph1, ph2, phs) -> do
    ph1' <- replacePlugPh ph1
    ph2' <- replacePlugPh ph2
    phs' <- mapM replacePlugPh phs
    return $ CPlugs ann (ph1', ph2', phs')
    where
      replacePlugPh (xph, (ins, outs), cmds) = do
        cmds' <- mapM passContext ((ctx,idp,) <$> cmds)
        return (xph, (ins, outs), cmds')
  CCase ann expr cases -> do
    cases' <-
      mapM
        ( \(p, cmds) -> do
            cmds' <- mapM (\c -> passContext (ctx, idp, c)) cmds
            return (p, cmds')
        )
        cases
    return $ CCase ann expr cases'
  CIf ann cond thenc elsec -> do
    thenc' <- mapM passContext ((ctx,idp,) <$> thenc)
    elsec' <- mapM passContext ((ctx,idp,) <$> elsec)
    return $ CIf ann cond thenc' elsec'
  cmd -> pure cmd

passAlong ::
  HOProcCodeGen
    ([XMplPattern MplTypeChecked], IdP MplTypeChecked, MplCmd MplTypeChecked)
    (Bool, MplCmd MplTypeChecked)
passAlong (patts, ctx, cmd) = case cmd of
  CRun ann (Right expr) seqs ins outs -> case expr of
    EVar _ idp ->
      ( if search patts idp
          then return (True, CRun ann (Right expr) (EVar (TypeBuiltIn (TypeUnitF Nothing)) ctx : seqs) ins outs)
          else return (False, CRun ann (Right expr) seqs ins outs)
      )
    _ -> return (True, CRun ann (Right expr) (EVar (TypeBuiltIn (TypeUnitF Nothing)) ctx : seqs) ins outs)
  CHCase ann chp cases -> do
    cases' <-
      mapM
        ( \(p, id, cmds) -> do
            cmds' <- mapM passAlong ((patts,ctx,) <$> cmds)
            return (any fst cmds', (p, id, snd <$> cmds'))
        )
        cases
    return (any fst cases', CHCase ann chp (snd <$> cases'))
  CFork ann chp ((ch1, f1, cmds1), (ch2, f2, cmds2)) -> do
    cmds1' <- mapM passAlong ((patts,ctx,) <$> cmds1)
    cmds2' <- mapM passAlong ((patts,ctx,) <$> cmds2)
    return (any fst cmds1' || any fst cmds2', CFork ann chp ((ch1, f1, snd <$> cmds1'), (ch2, f2, snd <$> cmds2')))
  CRace ann races -> do
    races' <-
      mapM
        ( \(ch, cmds) -> do
            cmds' <- mapM passAlong ((patts,ctx,) <$> cmds)
            return (any fst cmds', (ch, snd <$> cmds'))
        )
        races
    return (any fst races', CRace ann (snd <$> races'))
  CPlugs ann (ph1, ph2, phs) -> do
    ph1' <- replacePlugPh ph1
    ph2' <- replacePlugPh ph2
    phs' <- mapM replacePlugPh phs
    return (any fst (ph1' : ph2' : phs'), CPlugs ann (snd ph1', snd ph2', snd <$> phs'))
    where
      replacePlugPh (xph, (ins, outs), cmds) = do
        cmds' <- mapM passAlong ((patts,ctx,) <$> cmds)
        return (any fst cmds', (xph, (ins, outs), snd <$> cmds'))
  CCase ann expr cases -> do
    cases' <-
      mapM
        ( \(p, cmds) -> do
            cmds' <- mapM (\c -> passAlong (patts, ctx, c)) cmds
            return (any fst cmds', (p, snd <$> cmds'))
        )
        cases
    return (any fst cases', CCase ann expr (snd <$> cases'))
  CIf ann cond thenc elsec -> do
    thenc' <- mapM passAlong ((patts,ctx,) <$> thenc)
    elsec' <- mapM passAlong ((patts,ctx,) <$> elsec)
    return (any fst thenc' || any fst elsec', CIf ann cond (snd <$> thenc') (snd <$> elsec'))
  cmd -> pure (False, cmd)

handleExpr ::
  HOProcCodeGen
    ([XMplPattern MplTypeChecked], IdP MplTypeChecked, MplExpr MplTypeChecked)
    (Bool, MplExpr MplTypeChecked)
handleExpr (ctx, id, expr) = case expr of
  EPOps ann opty l r -> do
    l' <- handleExpr (ctx, id, l)
    r' <- handleExpr (ctx, id, r)
    return (fst l' || fst r', EPOps ann opty (snd l') (snd r'))
  ECase ann expr cases -> do
    expr' <- handleExpr (ctx, id, expr)
    cases' <-
      mapM
        ( \(p, e) -> do
            e' <- handleExpr (ctx, id, e)
            return (fst e', (p, snd e'))
        )
        cases
    return (fst expr' || any fst cases', ECase ann (snd expr') (snd <$> cases'))
  EObjCall ann idp exprs -> do
    exprs' <- mapM (\e -> handleExpr (ctx, id, e)) exprs
    return (any fst exprs', EObjCall ann idp (snd <$> exprs'))
  ECall ann idp exprs -> do
    exprs' <- mapM (\e -> handleExpr (ctx, id, e)) exprs
    return (any fst exprs', ECall ann idp (snd <$> exprs'))
  ERecord ann records -> do
    records' <-
      mapM
        ( \(r, id, (ps, e)) -> do
            e' <- handleExpr (ctx, id, e)
            return (fst e', (r, id, (ps, snd e')))
        )
        records
    return (any fst records', ERecord ann (snd <$> records'))
  EList ann exprs -> do
    exprs' <- mapM (\e -> handleExpr (ctx, id, e)) exprs
    return (any fst exprs', EList ann (snd <$> exprs'))
  EStore tp p -> case p of
    Left idp -> pure (False, EStore tp (Left idp))
    Right ((patts, ins, outs), cmds) -> do
      let tupleDefn = createTuple ctx id
      let pattTuple = createPattTuple ctx
      let procDefn = createProc tp ((pattTuple : patts, ins, outs), cmds)
      f tupleDefn
      f procDefn
      return (True, EStore tp (Left (fst tp)))
      where
        f :: MplDefn MplTypeChecked -> _ ()
        f defn = do
          (tell . runHOProcCodeGenDefn) defn
  ETuple ann (t0, t1, ts) -> do
    ~(t0' : t1' : ts') <- mapM (\e -> handleExpr (ctx, id, e)) (t0 : t1 : ts)
    return (any fst (t0' : t1' : ts'), ETuple ann (snd t0', snd t1', snd <$> ts'))
  EBuiltInOp ann optp l r -> do
    l' <- handleExpr (ctx, id, l)
    r' <- handleExpr (ctx, id, r)
    return (fst l' || fst r', EBuiltInOp ann optp (snd l') (snd r'))
  EIf ann cond thenc elsec -> do
    cond' <- handleExpr (ctx, id, cond)
    thenc' <- handleExpr (ctx, id, thenc)
    elsec' <- handleExpr (ctx, id, elsec)
    return (any fst [cond', thenc', elsec'], EIf ann (snd cond') (snd thenc') (snd elsec'))
  e -> pure (False, e)