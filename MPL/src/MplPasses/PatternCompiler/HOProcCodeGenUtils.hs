{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MplPasses.PatternCompiler.HOProcCodeGenUtils where

import Control.Monad.RWS
import qualified Data.List.NonEmpty as NE
import MplAST.MplCore
import MplAST.MplPattern
import MplAST.MplTypeChecked (ChIdentT (ChIdentT), TypeT (NamedType))
import MplPasses.LambdaLifter.LambdaLiftUtil
import Optics

type HOProcCodeGen a b =
  forall m.
  ( MonadWriter ([MplDefn MplTypeChecked], [IdP MplTypeChecked]) m
  ) =>
  a ->
  m b

createTuple :: [XMplPattern MplTypeChecked] -> IdP MplTypeChecked -> MplDefn MplTypeChecked
createTuple patts idp =
  let loc = Location (-1, -1)

      (tp, expr) :: (MplType MplTypeChecked, MplExpr MplTypeChecked) = case patts of
        [] -> (TypeBuiltIn (TypeUnitF Nothing), EUnit (loc, TypeBuiltIn (TypeUnitF Nothing)))
        [PVar ann id] -> (ann, EVar ann id)
        ((PVar ann1 id1) : (PVar ann2 id2) : ps) ->
          let tupleTp = TypeBuiltIn (TypeTupleF Nothing (ann1, ann2, map getPattType ps))
           in (tupleTp, ETuple (loc, tupleTp) (EVar ann1 id1, EVar ann2 id2, map (\(PVar ann id) -> EVar ann id) ps))
   in FunctionDefn (_MplFunction # (idp, ([], map getPattType patts, tp), (patts, expr) NE.:| []))

createPattTuple :: [MplPattern MplTypeChecked] -> MplPattern MplTypeChecked
createPattTuple patts =
  let loc = Location (-1, -1)
      tp r = (loc, TypeBuiltIn (TypeTupleF Nothing r))
      defaultTp = TypeBuiltIn (TypeUnitF Nothing)
   in case patts of
        [] -> PUnit (loc, defaultTp)
        [p] -> p
        (p1 : p2 : ps) -> PTuple (tp (getPattType p1, getPattType p2, map getPattType ps)) (p1, p2, ps)

createProc ::
  (IdP MplTypeChecked, XMplType MplTypeChecked) ->
  (([XMplPattern MplTypeChecked], [ChP MplTypeChecked], [ChP MplTypeChecked]), NE.NonEmpty (XMplCmd MplTypeChecked)) ->
  MplDefn MplTypeChecked
createProc tp defn@((patts, ins, outs), cmds) =
  let name = fst tp
      tp' = snd tp
   in ProcessDefn (_MplProcess # (name, ([], map getPattType patts, map (\(ChIdentT _ tp) -> tp) ins, map (\(ChIdentT _ tp) -> tp) outs), defn NE.:| []))

getPattType :: MplPattern MplTypeChecked -> MplType MplTypeChecked
getPattType = \case
  PConstructor (_, tp) _ _ -> tp
  PRecord (_, tp) _ -> tp
  PVar tp _ -> tp
  PNull (_, tp) -> tp
  PUnit (_, tp) -> tp
  PTuple (_, tp) _ -> tp
  PList (_, tp) _ -> tp
  PString (_, tp) _ -> tp
  PListCons (_, tp) _ _ -> tp
  PChar (_, tp) _ -> tp
  PInt (_, tp) _ -> tp
  PBool (_, tp) _ -> tp
  
fst_3 :: (a, b, c) -> a
fst_3 (a, b, c) = a

snd_3 :: (a, b, c) -> b
snd_3 (a, b, c) = b

trd_3 :: (a, b, c) -> c
trd_3 (a, b, c) = c