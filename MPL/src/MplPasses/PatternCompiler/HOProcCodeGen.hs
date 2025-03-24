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
  Map (IdP MplTypeChecked) (MplDefn MplTypeChecked) ->
  MplDefn MplTypeChecked ->
  [MplDefn MplTypeChecked]
runHOProcCodeGenDefn ptable defn =
  case defn of
    p@(ProcessDefn (MplProcess procname proctp (((patts, ins, outs), cmds) :| bdy))) ->
      hoProcCodeGenDefn ptable p
    _ -> [defn]

hoProcCodeGenDefn ::
  Map (IdP MplTypeChecked) (MplDefn MplTypeChecked) ->
  MplDefn MplTypeChecked ->
  [MplDefn MplTypeChecked]
hoProcCodeGenDefn ptable defn = case defn of
  ProcessDefn (MplProcess procname proctp (((patts, ins, outs), cmds) :| bdy)) ->
    let newInputName = modifyIdentRString (++ "_hidden_ctx") procname
        passAlongResult = NE.map passAlong ((patts,newInputName,) <$> cmds)
        cmds' = NE.map (passContext ptable) ((patts,newInputName,) <$> passAlongResult)
     in (ProcessDefn (MplProcess procname proctp (((patts, ins, outs), snd_3 <$> cmds') :| bdy)) : concatMap trd_3 cmds')
  _ -> [defn]

collectProc ::
  Map (IdP MplTypeChecked) (MplDefn MplTypeChecked) ->
  MplDefn MplTypeChecked ->
  Map (IdP MplTypeChecked) (MplDefn MplTypeChecked)
collectProc map defn = case defn of
  p@(ProcessDefn (MplProcess procname proctp (((patts, ins, outs), cmds) :| bdy))) -> Map.insert procname p map
  _ -> map

passContext ::
  Map (IdP MplTypeChecked) (MplDefn MplTypeChecked) ->
  ([MplPattern MplTypeChecked], IdP MplTypeChecked, MplCmd MplTypeChecked) ->
  ([IdP MplTypeChecked], MplCmd MplTypeChecked, [MplDefn MplTypeChecked])
passContext ptable (ctx, idp, cmd) = case cmd of
  CRun ann (Left id) seqs ins outs ->
    let exprResult = map (handleExpr ptable) ((ctx,idp,) <$> seqs)
     in (concatMap fst_3 exprResult, CRun ann (Left id) (snd_3 <$> exprResult) ins outs, concatMap trd_3 exprResult)
  CPut ann expr ch ->
    let liftResult = handleExpr ptable (ctx, idp, expr)
     in (fst_3 liftResult, CPut ann (snd_3 liftResult) ch, trd_3 liftResult)
  CHCase ann chp cases ->
    let cases' =
          NE.map
            ( \(p, id, cmds) ->
                let cmds' = NE.map (passContext ptable) ((ctx,id,) <$> cmds)
                 in (concatMap fst_3 cmds', (p, id, snd_3 <$> cmds'), concatMap trd_3 cmds')
            )
            cases
     in (concatMap fst_3 (NE.toList cases'), CHCase ann chp (snd_3 <$> cases'), concatMap trd_3 (NE.toList cases'))
  CFork ann chp ((ch1, f1, cmds1), (ch2, f2, cmds2)) ->
    let cmds1' = NE.map (passContext ptable) ((ctx,idp,) <$> cmds1)
        cmds2' = NE.map (passContext ptable) ((ctx,idp,) <$> cmds2)
     in (concatMap fst_3 cmds1' ++ concatMap fst_3 cmds2', CFork ann chp ((ch1, f1, snd_3 <$> cmds1'), (ch2, f2, snd_3 <$> cmds2')), concatMap trd_3 cmds1' ++ concatMap trd_3 cmds2')
  CRace ann races ->
    let races' =
          NE.map
            ( \(ch, cmds) ->
                let cmds' = NE.map (passContext ptable) ((ctx,idp,) <$> cmds)
                 in (concatMap fst_3 cmds', (ch, snd_3 <$> cmds'), concatMap trd_3 cmds')
            )
            races
     in (concatMap fst_3 races', CRace ann (snd_3 <$> races'), concatMap trd_3 races')
  CPlugs ann (ph1, ph2, phs) ->
    let ph1' = replacePlugPh ph1
        ph2' = replacePlugPh ph2
        phs' = map replacePlugPh phs
     in (fst_3 ph1' ++ fst_3 ph2' ++ concatMap fst_3 phs', CPlugs ann (snd_3 ph1', snd_3 ph2', snd_3 <$> phs'), trd_3 ph1' ++ trd_3 ph2' ++ concatMap trd_3 phs')
    where
      replacePlugPh (xph, (ins, outs), cmds) =
        let cmds' = NE.map (passContext ptable) ((ctx,idp,) <$> cmds)
         in (concatMap fst_3 cmds', (xph, (ins, outs), snd_3 <$> cmds'), concatMap trd_3 cmds')
  CCase ann expr cases ->
    let cases' =
          NE.map
            ( \(p, cmds) ->
                let cmds' = NE.map (\c -> passContext ptable (ctx, idp, c)) cmds
                 in (concatMap fst_3 cmds', (p, snd_3 <$> cmds'), concatMap trd_3 cmds')
            )
            cases
     in (concatMap fst_3 cases', CCase ann expr (snd_3 <$> cases'), concatMap trd_3 cases')
  CIf ann cond thenc elsec ->
    let thenc' = NE.map (passContext ptable) ((ctx,idp,) <$> thenc)
        elsec' = NE.map (passContext ptable) ((ctx,idp,) <$> elsec)
     in (concatMap fst_3 thenc' ++ concatMap fst_3 elsec', CIf ann cond (snd_3 <$> thenc') (snd_3 <$> elsec'), concatMap trd_3 thenc' ++ concatMap trd_3 elsec')
  cmd -> ([], cmd, [])

passAlong ::
  ([XMplPattern MplTypeChecked], IdP MplTypeChecked, MplCmd MplTypeChecked) ->
  MplCmd MplTypeChecked
passAlong (patts, ctx, cmd) = case cmd of
  CRun ann (Right expr) seqs ins outs -> case expr of
    EVar evarAnn evar -> CRun ann (Right (EProj () 1 expr)) (EProj () 0 expr : seqs) ins outs
  CHCase ann chp cases ->
    let cases' =
          NE.map
            ( \(p, id, cmds) ->
                let cmds' = NE.map passAlong ((patts,ctx,) <$> cmds)
                 in (p, id, cmds')
            )
            cases
     in CHCase ann chp cases'
  CFork ann chp ((ch1, f1, cmds1), (ch2, f2, cmds2)) ->
    let cmds1' = NE.map passAlong ((patts,ctx,) <$> cmds1)
        cmds2' = NE.map passAlong ((patts,ctx,) <$> cmds2)
     in CFork ann chp ((ch1, f1, cmds1'), (ch2, f2, cmds2'))
  CRace ann races ->
    let races' =
          NE.map
            ( \(ch, cmds) ->
                let cmds' = NE.map passAlong ((patts,ctx,) <$> cmds)
                 in (ch, cmds')
            )
            races
     in CRace ann races'
  CPlugs ann (ph1, ph2, phs) ->
    let ph1' = replacePlugPh ph1
        ph2' = replacePlugPh ph2
        phs' = map replacePlugPh phs
     in CPlugs ann (ph1', ph2', phs')
    where
      replacePlugPh (xph, (ins, outs), cmds) =
        let cmds' = NE.map passAlong ((patts,ctx,) <$> cmds)
         in (xph, (ins, outs), cmds')
  CCase ann expr cases ->
    let cases' =
          NE.map
            ( \(p, cmds) ->
                let cmds' = NE.map (\c -> passAlong (patts, ctx, c)) cmds
                 in (p, cmds')
            )
            cases
     in CCase ann expr cases'
  CIf ann cond thenc elsec ->
    let thenc' = NE.map passAlong ((patts,ctx,) <$> thenc)
        elsec' = NE.map passAlong ((patts,ctx,) <$> elsec)
     in CIf ann cond thenc' elsec'
  cmd -> cmd

handleExpr ::
  Map (IdP MplTypeChecked) (MplDefn MplTypeChecked) ->
  ([XMplPattern MplTypeChecked], IdP MplTypeChecked, MplExpr MplTypeChecked) ->
  ([IdP MplTypeChecked], MplExpr MplTypeChecked, [MplDefn MplTypeChecked])
handleExpr ptable (ctx, id, expr) = case expr of
  EPOps ann opty l r ->
    let (lids, lexpr, ldefns) = handleExpr ptable (ctx, id, l)
        (rids, rexpr, rdefns) = handleExpr ptable (ctx, id, r)
     in -- return (fst l' || fst r', EPOps ann opty (snd l') (snd r'))
        (lids ++ rids, EPOps ann opty lexpr rexpr, ldefns ++ rdefns)
  ECase ann expr cases ->
    let (ids', expr', defns') = handleExpr ptable (ctx, id, expr)
        cases' =
          NE.map
            ( \(p, e) ->
                let (eids, e', edefns) = handleExpr ptable (ctx, id, e)
                 in (eids, (p, e'), edefns)
            )
            cases
     in (ids' ++ concatMap fst_3 (NE.toList cases'), ECase ann expr' (snd_3 <$> cases'), defns' ++ concatMap trd_3 (NE.toList cases'))
  EObjCall ann idp exprs ->
    let exprs' = map (\e -> handleExpr ptable (ctx, id, e)) exprs
     in (concatMap fst_3 exprs', EObjCall ann idp (snd_3 <$> exprs'), concatMap trd_3 exprs')
  ECall ann idp exprs ->
    let exprs' = map (\e -> handleExpr ptable (ctx, id, e)) exprs
     in (concatMap fst_3 exprs', ECall ann idp (snd_3 <$> exprs'), concatMap trd_3 exprs')
  ERecord ann records ->
    let records' =
          NE.map
            ( \(r, id, (ps, e)) ->
                let (eids, e', edefns) = handleExpr ptable (ctx, id, e)
                 in (eids, (r, id, (ps, e')), edefns)
            )
            records
     in (concatMap fst_3 (NE.toList records'), ERecord ann (snd_3 <$> records'), concatMap trd_3 (NE.toList records'))
  EList ann exprs ->
    let exprs' = map (\e -> handleExpr ptable (ctx, id, e)) exprs
     in (concatMap fst_3 exprs', EList ann (snd_3 <$> exprs'), concatMap trd_3 exprs')
  EStore tp p ->
    let tupleDefn = createTuple ctx id
        pattTuple = createPattTuple ctx
        tuple = ETuple (Location (0, 0), TypeBuiltIn (TypeUnitF Nothing))
        fcall = ECall (TypeBuiltIn (TypeUnitF Nothing)) id (map (\(PVar ann e) -> EVar ann e) ctx)
     in case p of
          Left idp -> case Map.lookup idp ptable of
            Nothing -> ([idp], tuple (EUnit (Location (0, 0), TypeBuiltIn (TypeUnitF Nothing)), EStore tp (Left idp), []), [])
            Just procs@(ProcessDefn (MplProcess procname proctp (((patts, ins, outs), cmds) :| bdy))) ->
              let newIdp = modifyIdentRString (++ "__stored") procname
                  newPatts = PUnit (Location (0, 0), TypeBuiltIn (TypeUnitF Nothing)) : patts
                  newproc = ProcessDefn (MplProcess newIdp proctp (((newPatts, ins, outs), cmds) :| bdy))
               in ([idp], tuple (EUnit (Location (0, 0), TypeBuiltIn (TypeUnitF Nothing)), EStore tp (Left newIdp), []), [newproc])
          Right ((patts, ins, outs), cmds) ->
            let procDefn = createProc tp ((pattTuple : patts, ins, outs), cmds)
             in ([], tuple (fcall, EStore tp (Left (fst tp)), []), [tupleDefn, procDefn])
  ETuple ann (t0, t1, ts) ->
    let (t0' : t1' : ts') = map (\e -> handleExpr ptable (ctx, id, e)) (t0 : t1 : ts)
     in (concatMap fst_3 (t0' : t1' : ts'), ETuple ann (snd_3 t0', snd_3 t1', snd_3 <$> ts'), concatMap trd_3 (t0' : t1' : ts'))
  EBuiltInOp ann optp l r ->
    let l' = handleExpr ptable (ctx, id, l)
        r' = handleExpr ptable (ctx, id, r)
     in (fst_3 l' ++ fst_3 r', EBuiltInOp ann optp (snd_3 l') (snd_3 r'), trd_3 l' ++ trd_3 r')
  EIf ann cond thenc elsec ->
    let cond' = handleExpr ptable (ctx, id, cond)
        thenc' = handleExpr ptable (ctx, id, thenc)
        elsec' = handleExpr ptable (ctx, id, elsec)
     in (concatMap fst_3 [cond', thenc', elsec'], EIf ann (snd_3 cond') (snd_3 thenc') (snd_3 elsec'), concatMap trd_3 [cond', thenc', elsec'])
  e -> ([], e, [])