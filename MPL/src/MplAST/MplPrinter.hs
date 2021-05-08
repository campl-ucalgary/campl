{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MplAST.MplPrinter where

import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplCmd
import MplAST.MplType
import MplAST.MplProg
import MplAST.MplIdent
import MplAST.MplRenamed
import MplAST.MplParsed
import MplAST.MplTypeChecked
import MplAST.MplExt

import MplUtil.UniqueSupply

import qualified Language.AbsMPL as B
import qualified Language.ErrM as B
import qualified Language.LayoutMPL as B
import qualified Language.LexMPL as B
import qualified Language.ParMPL as B
import qualified Language.PrintMPL as B

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Void

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Data
import Data.Kind
import Data.Bool
import Data.Maybe
import Control.Arrow
import Data.Kind

{- Module for a pretty printer of the AST (useful for debugging)
 - Note this just translates back to the BNFC representation and uses
 - BNFC to print the AST.
 -}

class PPrint a where
    pprint :: a -> String

instance PPrint Int where
    pprint n = show n

instance PPrint Double where
    pprint n = show n

instance PPrint Char where
    pprint n = show n

instance PPrint String where
    pprint = id

instance PPrint UniqueTag where
    pprint (UniqueTag (Unique n)) = show n

instance PPrint Polarity where
    pprint Input = "INP"
    pprint Output = "OUT"

instance PPrint IdentP where
    pprint n = case n ^. name of Name str -> str

instance PPrint IdentR where
    pprint n = n ^. identRIdentP % to pprint ++ "__" ++ n ^. uniqueTag % to pprint 

instance PPrint TypeT where
    pprint (NamedType identr) = pprint identr 
    pprint (GenNamedType p) = "T" ++ pprint p

instance PPrint ChIdentR where
    pprint n = n ^. chIdentRIdentR % to pprint ++ "__" ++ n ^. polarity % to pprint

instance PPrint ChIdentT where
    pprint n = n ^. chIdentTChIdentR % to pprint 

instance PPrint B.MplType where
    pprint = B.printTree 

instance (PPrint (IdP x), PPrint (TypeP x) ) => PPrint (MplType x) where
    pprint = B.printTree . mplTypeToBnfc




-- print instances of annotations from type checking (never printed out)
instance PPrint ((), MplSeqObjDefn MplTypeCheckedClause) where
    pprint _ = ""
instance PPrint ((), MplConcObjDefn MplTypeCheckedClause) where
    pprint _ = ""

instance MplPrintConstraints x => PPrint (MplProg x) where
    pprint = mplPprint

instance (PPrint (IdP x),PPrint (ChP x), MplExprToBnfc (XMplExpr x), MplPattToBnfc (XMplPattern x), MplToForkPhrase (ChP x, XCForkPhrase x, NonEmpty (MplCmd x)), MplToPlugPhrase (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty (MplCmd x))) 
    => PPrint (MplCmd x) where
    pprint = B.printTree . mplCmdToBnfc

type MplPrintConstraints x =
    ( UserProvidedTypeToBnfc (XFunType x)
    , UserProvidedTypeToBnfc (XProcType x)
    , MplPattToBnfc (XMplPattern x)
    , MplExprToBnfc (XMplExpr x)
    , MplCmdToBnfc (XMplCmd x)
    , MplTypesToBnfc (XTypePhraseFrom x (SeqObjTag DataDefnTag))
    , MplTypeToBnfc (XTypePhraseTo x (SeqObjTag DataDefnTag))
    , MplTypesToBnfc (XTypePhraseFrom x (SeqObjTag CodataDefnTag))
    , MplTypeToBnfc (XTypePhraseTo x (SeqObjTag CodataDefnTag))

    , MplTypeToBnfc (XTypePhraseFrom x (ConcObjTag ProtocolDefnTag))
    , MplTypeToBnfc (XTypePhraseTo x (ConcObjTag ProtocolDefnTag))
    , MplTypeToBnfc (XTypePhraseFrom x (ConcObjTag CoprotocolDefnTag))
    , MplTypeToBnfc (XTypePhraseTo x (ConcObjTag CoprotocolDefnTag))

    , XDataDefn x ~ MplTypeClauseSpine x (SeqObjTag DataDefnTag)
    , XCodataDefn x ~ MplTypeClauseSpine x (SeqObjTag CodataDefnTag)
    , XProtocolDefn x ~ MplTypeClauseSpine x (ConcObjTag ProtocolDefnTag)
    , XCoprotocolDefn x ~ MplTypeClauseSpine x (ConcObjTag CoprotocolDefnTag)

    , XFunctionDefn x ~ MplFunction x
    , XProcessDefn x ~ MplProcess x
    , PPrint (IdP x)
    , PPrint (ChP x)) 


mplPprint :: MplPrintConstraints x =>
    MplProg x -> 
    String
mplPprint = B.printTree . mplProgToBnfc

mplProgToBnfc :: MplPrintConstraints x =>
    MplProg x -> 
    B.MplProg
mplProgToBnfc (MplProg stmts) = B.MPL_PROG $ map mplStmtToBnfc stmts

mplStmtToBnfc :: MplPrintConstraints x =>
    MplStmt x -> B.MplStmt
mplStmtToBnfc (MplStmt defns []) = 
    B.MPL_DEFN_STMS $ NE.toList $ fmap mplDefnToBnfc defns
mplStmtToBnfc (MplStmt defns wheres) = 
    B.MPL_DEFN_STMS_WHERE (NE.toList $ fmap (mplDefnToBnfc) defns) (map (B.MPL_WHERE . mplStmtToBnfc) wheres)

mplDefnToBnfc :: MplPrintConstraints x =>
    MplDefn x -> 
    B.MplDefn
mplDefnToBnfc (ObjectDefn obj) = mplObjDefnToBnfc obj
mplDefnToBnfc (FunctionDefn (MplFunction id tp body)) = B.MPL_FUNCTION_DEFN $ case userProvidedTypeToBnfc tp of
    Just tp -> B.INTERNAL_TYPED_FUNCTION_DEFN id' tp body'
    Nothing -> B.FUNCTION_DEFN id' body'
  where
    id' = toBnfcIdent id
    body' = NE.toList $ fmap (uncurry B.PATTERN_TO_EXPR <<< map mplPattToBnfc *** mplExprToBnfc) body
mplDefnToBnfc (ProcessDefn (MplProcess id tp body)) = B.MPL_PROCESS_DEFN $ case userProvidedTypeToBnfc tp of
    Just tp -> B.INTERNAL_TYPED_PROCESS_DEFN id' tp body'
    Nothing -> B.PROCESS_DEFN id' body'
        
  where
    id' = toBnfcIdent id
    body' = NE.toList $ fmap f body

    f ((patts, ins, outs), cmds) = 
        B.PROCESS_PHRASE (map mplPattToBnfc patts) (map toBnfcIdent ins) (map toBnfcIdent outs) $ mplCmdsToBnfc cmds

mplObjDefnToBnfc :: 
    MplPrintConstraints x =>
    MplObjectDefn x -> B.MplDefn
mplObjDefnToBnfc (SeqObjDefn (DataDefn x)) = 
    B.MPL_SEQUENTIAL_TYPE_DEFN 
        $ B.DATA_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap mplClauseToBnfc ))
mplObjDefnToBnfc (SeqObjDefn (CodataDefn x)) = 
    B.MPL_SEQUENTIAL_TYPE_DEFN 
        $ B.CODATA_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap mplClauseToBnfc ))
mplObjDefnToBnfc (ConcObjDefn (ProtocolDefn x)) = 
    B.MPL_CONCURRENT_TYPE_DEFN 
        $ B.PROTOCOL_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap mplClauseToBnfc ))
mplObjDefnToBnfc (ConcObjDefn (CoprotocolDefn x)) = 
    B.MPL_CONCURRENT_TYPE_DEFN 
        $ B.COPROTOCOL_DEFN 
        $ (x ^. typeClauseSpineClauses % to (NE.toList . fmap mplClauseToBnfc ))

class UserProvidedTypeToBnfc t where
    userProvidedTypeToBnfc :: t -> Maybe B.MplType

instance PPrint ident => UserProvidedTypeToBnfc (Maybe ([ident], [MplType MplRenamed], [MplType MplRenamed], [MplType MplRenamed])) where

    userProvidedTypeToBnfc Nothing = Nothing
    userProvidedTypeToBnfc (Just (foralls, seqs, ins, outs)) = Just $ 
        B.MPL_CONC_ARROW_TYPE (map toBnfcIdent foralls) (map mplTypeToBnfc seqs) (map mplTypeToBnfc ins) (map mplTypeToBnfc outs)
        
instance PPrint ident => UserProvidedTypeToBnfc (Maybe ([ident], [MplType MplRenamed], MplType MplRenamed)) where
    userProvidedTypeToBnfc Nothing = Nothing
    userProvidedTypeToBnfc (Just (foralls, froms, to)) = Just $ 
        B.MPL_SEQ_ARROW_TYPE (map toBnfcIdent foralls) (map mplTypeToBnfc froms) (mplTypeToBnfc to) 


instance PPrint ident => UserProvidedTypeToBnfc ([ident], [MplType MplTypeChecked], MplType MplTypeChecked) where
    userProvidedTypeToBnfc (foralls, froms, to) = Just $
        B.MPL_SEQ_ARROW_TYPE (map toBnfcIdent foralls) (map mplTypeToBnfc froms) (mplTypeToBnfc to) 

instance PPrint ident => UserProvidedTypeToBnfc ([ident], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked]) where
    userProvidedTypeToBnfc (foralls, seqs, ins, outs) = Just $ 
        B.MPL_CONC_ARROW_TYPE (map toBnfcIdent foralls) (map mplTypeToBnfc seqs) (map mplTypeToBnfc ins) (map mplTypeToBnfc outs)


class MplPattToBnfc t where
    mplPattToBnfc ::  t -> B.Pattern

instance ( PPrint (IdP x) ) => MplPattToBnfc (MplPattern x) where
    mplPattToBnfc = f
      where
        f (PVar _ id) = B.VAR_PATTERN $ toBnfcIdent id
        f (PConstructor _ id args) = 
            B.CONSTRUCTOR_PATTERN_ARGS (toBnfcIdent id) bnfcKeyword (map f args) bnfcKeyword 
        f (PRecord _ args) = 
            B.RECORD_PATTERN bnfcKeyword (NE.toList $ fmap g args) bnfcKeyword
          where
            g (_, id, patt) = B.DESTRUCTOR_PATTERN_PHRASE (toBnfcIdent id) (f patt)
        f (PNull _) = B.NULL_PATTERN bnfcKeyword

class MplTypeToBnfc t where
    mplTypeToBnfc :: t -> B.MplType

instance MplTypeToBnfc IdentR where
    mplTypeToBnfc i = mplTypeToBnfc (_TypeVar # ((), i) :: MplType MplRenamed)

instance ( PPrint (IdP x), PPrint (TypeP x) ) => MplTypeToBnfc (MplType x) where
    mplTypeToBnfc = f
      where
        f (TypeVar cxt tp) = B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent tp
        f (TypeWithNoArgs cxt tp) = B.MPL_UIDENT_NO_ARGS_TYPE (toBnfcIdent tp)

        f (TypeSeqWithArgs cxt tp args) = 
            B.MPL_UIDENT_ARGS_TYPE (toBnfcIdent tp) bnfcKeyword (map f args) bnfcKeyword
        f (TypeSeqVarWithArgs cxt tp args) = 
            B.MPL_UIDENT_ARGS_TYPE (
                (\case (B.UIdent (pos, str)) -> B.UIdent (pos, str ++ "TVAR") )
                (toBnfcIdent tp :: B.UIdent)) 
                bnfcKeyword (map f args) bnfcKeyword
        f (TypeConcWithArgs cxt tp (seqs, chs)) = 
            B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE (toBnfcIdent tp) bnfcKeyword (map f seqs) (map f chs) bnfcKeyword
        f (TypeConcVarWithArgs cxt tp (seqs, chs)) = 
            B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE (toBnfcIdent tp) bnfcKeyword (map f seqs) (map f chs) bnfcKeyword
        f (TypeBuiltIn n) = case n of
            TypeSeqArrF cxt froms to ->
                B.MPL_SEQ_ARROW_TYPE [] (NE.toList $ fmap f froms) (f to)
            TypeConcArrF cxt seqs froms tos -> 
                B.MPL_CONC_ARROW_TYPE [] (map f seqs) (map f froms) (map f tos)
            TypeTensorF cxt l r -> 
                B.TENSOR_TYPE (f l) bnfcKeyword (f r)
            TypeParF cxt l r -> 
                B.PAR_TYPE (f l) bnfcKeyword (f r)

            TypeGetF cxt seq conc ->
                B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
                    (toBnfcIdent "Get") 
                    bnfcKeyword 
                    [f seq]
                    [f conc]
                    bnfcKeyword
            TypePutF cxt seq conc ->
                B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
                    (toBnfcIdent "Put") 
                    bnfcKeyword 
                    [f seq]
                    [f conc]
                    bnfcKeyword
            TypeTopBotF cxt -> B.MPL_UIDENT_NO_ARGS_TYPE (toBnfcIdent "TopBot")
            TypeNegF cxt tp -> B.MPL_UIDENT_ARGS_TYPE (toBnfcIdent "Neg") bnfcKeyword [f tp] bnfcKeyword

            TypeIntF _cxt -> B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent "Int"
            TypeDoubleF _cxt -> B.MPL_UIDENT_NO_ARGS_TYPE $ toBnfcIdent "Double"

class MplClauseToBnfc x t res | t -> res where
    mplClauseToBnfc :: MplTypeClause x t -> res

instance MplPrintConstraints x => MplClauseToBnfc x (SeqObjTag DataDefnTag) B.SeqTypeClauseDefn where
    mplClauseToBnfc clause = 
        B.SEQ_TYPE_CLAUSE name st $ clause ^. typeClausePhrases % to (map mplPhraseToBnfc)
      where
        name = B.MPL_UIDENT_ARGS_TYPE (clause ^. typeClauseName % to toBnfcIdent) 
            bnfcKeyword (clause ^. typeClauseArgs % to 
                (map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent))) bnfcKeyword
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent )

instance MplPrintConstraints x => MplClauseToBnfc x (SeqObjTag CodataDefnTag) B.SeqTypeClauseDefn where
    mplClauseToBnfc clause = 
        B.SEQ_TYPE_CLAUSE st name $ clause ^. typeClausePhrases % to (map mplPhraseToBnfc)
      where
        name = B.MPL_UIDENT_ARGS_TYPE (clause ^. typeClauseName % to toBnfcIdent) 
            bnfcKeyword (clause ^. typeClauseArgs % to 
                (map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent))) bnfcKeyword
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent )

instance MplPrintConstraints x => MplClauseToBnfc x (ConcObjTag ProtocolDefnTag) B.ConcurrentTypeClauseDefn where
    mplClauseToBnfc clause = 
        B.CONCURRENT_TYPE_CLAUSE name st $ clause ^. typeClausePhrases % to (map mplPhraseToBnfc)
        -- error "to do in pretty printer"
      where
        name = B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
            (clause ^. typeClauseName % to toBnfcIdent) 
            bnfcKeyword 
            seqs
            concs
            bnfcKeyword
        (seqs, concs) = clause ^. typeClauseArgs % to 
                ( map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent)
                        *** map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent))
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent )

instance MplPrintConstraints x => MplClauseToBnfc x (ConcObjTag CoprotocolDefnTag) B.ConcurrentTypeClauseDefn where
    mplClauseToBnfc clause = 
        B.CONCURRENT_TYPE_CLAUSE st name $ clause ^. typeClausePhrases % to (map mplPhraseToBnfc)
        -- error "to do in pretty printer"
      where
        name = B.MPL_UIDENT_SEQ_CONC_ARGS_TYPE 
            (clause ^. typeClauseName % to toBnfcIdent) 
            bnfcKeyword 
            seqs
            concs
            bnfcKeyword
        (seqs, concs) = clause ^. typeClauseArgs % to 
                ( map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent)
                        *** map (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent))
        st = clause ^. typeClauseStateVar % to (B.MPL_UIDENT_NO_ARGS_TYPE . toBnfcIdent )

class MplPhraseToBnfc x t res | t -> res where
    mplPhraseToBnfc :: MplTypePhrase x t -> res

instance MplPrintConstraints x => MplPhraseToBnfc x (SeqObjTag DataDefnTag) B.SeqTypePhraseDefn where
    mplPhraseToBnfc phrase = 
        B.SEQ_TYPE_PHRASE [phrase ^. typePhraseName % to toBnfcIdent] 
            (phrase ^. typePhraseFrom % to mplTypesToBnfc) (phrase ^. typePhraseTo % to mplTypeToBnfc)

instance MplPrintConstraints x => MplPhraseToBnfc x (SeqObjTag CodataDefnTag) B.SeqTypePhraseDefn where
    mplPhraseToBnfc phrase = 
        B.SEQ_TYPE_PHRASE [phrase ^. typePhraseName % to toBnfcIdent] 
            (phrase ^. typePhraseFrom % to mplTypesToBnfc) (phrase ^. typePhraseTo % to mplTypeToBnfc)

instance MplPrintConstraints x => MplPhraseToBnfc x (ConcObjTag ProtocolDefnTag) B.ConcurrentTypePhraseDefn where
    mplPhraseToBnfc phrase = 
        B.CONCURRENT_TYPE_PHRASE [phrase ^. typePhraseName % to toBnfcIdent]
            (phrase ^. typePhraseFrom % to mplTypeToBnfc) (phrase ^. typePhraseTo % to mplTypeToBnfc)
            
instance MplPrintConstraints x => MplPhraseToBnfc x (ConcObjTag CoprotocolDefnTag) B.ConcurrentTypePhraseDefn where
    mplPhraseToBnfc phrase = 
        B.CONCURRENT_TYPE_PHRASE [phrase ^. typePhraseName % to toBnfcIdent]
            (phrase ^. typePhraseFrom % to mplTypeToBnfc) (phrase ^. typePhraseTo % to mplTypeToBnfc)

class MplTypesToBnfc t where
    mplTypesToBnfc :: t -> [B.MplType]

-- not totally sure why we need this instance?
instance MplTypesToBnfc (MplType MplRenamed) where
    mplTypesToBnfc = pure . mplTypeToBnfc 

instance MplTypesToBnfc (MplType MplTypeChecked) where
    mplTypesToBnfc = pure . mplTypeToBnfc 

instance MplTypesToBnfc [MplType MplRenamed] where
    mplTypesToBnfc = map mplTypeToBnfc 

instance MplTypesToBnfc [MplType MplTypeChecked] where
    mplTypesToBnfc = map mplTypeToBnfc 

instance MplTypesToBnfc ([MplType MplRenamed], MplType MplRenamed) where
    mplTypesToBnfc (as, a) = map mplTypeToBnfc (as++[a])

instance MplTypesToBnfc ([MplType MplTypeChecked], MplType MplTypeChecked) where
    mplTypesToBnfc (as, a) = map mplTypeToBnfc (as++[a])

instance MplTypesToBnfc ([MplType MplRenamed], IdentR) where
    mplTypesToBnfc (as, a) = map mplTypeToBnfc (as++ [_TypeVar # ((),a)])

    
instance MplTypesToBnfc IdentR where
    mplTypesToBnfc a = [mplTypeToBnfc (_TypeVar # ((), a) :: MplType MplRenamed)]

class MplCmdToBnfc t where
    mplCmdToBnfc :: t -> B.ProcessCommand


instance ( MplToForkPhrase (CForkPhrase x), MplToPlugPhrase (CPlugPhrase x), PPrint (IdP x), PPrint (ChP x), MplExprToBnfc (XMplExpr x), MplPattToBnfc (XMplPattern x) ) => MplCmdToBnfc (MplCmd x) where
    mplCmdToBnfc = f
      where
        f (CRun _ id seqs ins outs) = 
            B.PROCESS_RUN (toBnfcIdent id) bnfcKeyword (map mplExprToBnfc seqs) 
                (map toBnfcIdent ins) (map toBnfcIdent outs) bnfcKeyword
        f (CClose _ ch) = B.PROCESS_CLOSE bnfcKeyword $ toBnfcIdent ch
        f (CHalt _ ch) = B.PROCESS_HALT bnfcKeyword $ toBnfcIdent ch
        f (CGet _ patt ch) = B.PROCESS_GET bnfcKeyword (mplPattToBnfc patt) (toBnfcIdent ch)
        f (CPut _ expr ch) = B.PROCESS_PUT bnfcKeyword (mplExprToBnfc expr) (toBnfcIdent ch)
        f (CHCase _ ch cases) = B.PROCESS_HCASE bnfcKeyword (toBnfcIdent ch) $ NE.toList $ fmap g cases
          where
            g (_, id, cmds) = B.HCASE_PHRASE (toBnfcIdent id) (mplCmdsToBnfc cmds)
        f (CHPut _ id ch) = B.PROCESS_HPUT bnfcKeyword (toBnfcIdent id) (toBnfcIdent ch)
        f (CSplit _ ch (s,t)) = B.PROCESS_SPLIT bnfcKeyword (toBnfcIdent ch) (map toBnfcIdent [s,t])
        -- TODO -- we should actually show the generated context here!
        f (CFork _ ch (a,b)) = 
            B.PROCESS_FORK bnfcKeyword (toBnfcIdent ch) $ map mplToForkPhrase [a, b]
        f (CId _ (a,b)) = B.PROCESS_ID (toBnfcIdent a) bnfcKeyword (toBnfcIdent b)
        f (CIdNeg _ (a,b)) = B.PROCESS_NEG 
            (toBnfcIdent a) bnfcKeyword 
            (toBnfcIdent b)
        f (CRace _ races) = B.PROCESS_RACE $ NE.toList $ fmap g races
          where
            g (ch, cmds) = B.RACE_PHRASE (toBnfcIdent ch) $ mplCmdsToBnfc cmds
        -- TODO -- we should actually show the generated context here!??
        f (CPlugs _ (a,b,c)) = B.PROCESS_PLUG $ map mplToPlugPhrase (a:b:c)

        f (CCase cxt caseon cases) = B.PROCESS_CASE bnfcKeyword (mplExprToBnfc caseon) $ NE.toList $ fmap g cases 
          where
            g (patt, cmds) = B.PROCESS_CASE_PHRASE (mplPattToBnfc patt) (mplCmdsToBnfc cmds)
        f (CSwitch cxt switches) = B.PROCESS_SWITCH $ NE.toList $ fmap g switches
          where
            g (expr, cmds) = B.PROCESS_SWITCH_PHRASE (mplExprToBnfc expr) (mplCmdsToBnfc cmds)


-- we can configure a fork phrase to have a context..
class MplToForkPhrase t where
    mplToForkPhrase :: t -> B.ForkPhrase
          {-
          where
            g (a,cxt, cmds) = undefined
                -- Just cxt -> B.FORK_WITH_PHRASE (toBnfcIdent a) (map toBnfcIdent cxt) $ mplCmdsToBnfc cmds
                -- Nothing -> B.FORK_PHRASE (toBnfcIdent a) $ mplCmdsToBnfc cmds
           -}
instance ( PPrint ident, MplCmdToBnfc t) => MplToForkPhrase (ident, [ident], NonEmpty t) where
    mplToForkPhrase (ch, cxt, cmds) = B.FORK_WITH_PHRASE 
        (toBnfcIdent ch) (map toBnfcIdent cxt) $ mplCmdsToBnfc cmds

class MplToPlugPhrase t where
    mplToPlugPhrase :: t -> B.PlugPhrase
          {-
          where
            g (_, (a,b), cmds) = B.PLUG_PHRASE_AS 
                (map toBnfcIdent a) (map toBnfcIdent b) (mplCmdsToBnfc cmds)
            -}
instance ( PPrint ident, MplCmdToBnfc cmd) => MplToPlugPhrase ((), ([ident], [ident]), NonEmpty cmd) where
    mplToPlugPhrase (cxt, (ins, outs), cmds) = B.PLUG_PHRASE_AS 
        (map toBnfcIdent ins) (map toBnfcIdent outs) $ mplCmdsToBnfc cmds


mplCmdsToBnfc :: MplCmdToBnfc t => NonEmpty t -> B.ProcessCommandsBlock
mplCmdsToBnfc (cmd :| []) = B.PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK $ mplCmdToBnfc cmd
mplCmdsToBnfc cmds = B.PROCESS_COMMANDS_DO_BLOCK $ NE.toList $ fmap mplCmdToBnfc cmds

class MplExprToBnfc t where
    mplExprToBnfc :: t -> B.Expr

instance MplPrintConstraints x => MplExprToBnfc (MplExpr x) where
    mplExprToBnfc = f
      where
        f (EPOps _ op exp0 exp1) = case op of
            PrimitiveAdd ->  B.INFIXL5_EXPR (f exp0) (toBnfcIdent op) (f exp1)
            PrimitiveSub ->  B.INFIXL5_EXPR (f exp0) (toBnfcIdent op) (f exp1)
            _ ->  error "primitive op not implemented yet"
        f (EVar _ id) = B.VAR_EXPR $ toBnfcIdent id
        f (EInt _ id) = B.INT_EXPR $ toBnfcIdent id
        -- f (EChar _ id) = B.CHAR_EXPR id
        f (EChar _ id) = error "char not implemented"
        -- f (EDouble _ id) = B.DOUBLE_EXPR id
        f (EDouble _ id) = B.DOUBLE_EXPR $ toBnfcIdent id

        f (ECase _ expr pattexprs) = B.CASE_EXPR bnfcKeyword (f expr) $ NE.toList $ fmap g pattexprs
          where
            g (patt, expr) = B.PATTERN_TO_EXPR [mplPattToBnfc patt] (mplExprToBnfc expr)
        f (EObjCall _ id exprs) = B.DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR  
            (toBnfcIdent id) bnfcKeyword (map f exprs) bnfcKeyword 
        f (ECall _ id exprs) = B.FUN_EXPR  
            (toBnfcIdent id) bnfcKeyword (map f exprs) bnfcKeyword 
        f (ELet _ stmts expr) = B.LET_EXPR (NE.toList (fmap g stmts)) (f expr)
          where
            g = B.LET_EXPR_PHRASE . mplStmtToBnfc
        f (ERecord _ exprs) = B.RECORD_EXPR bnfcKeyword (NE.toList $ fmap g exprs) bnfcKeyword
          where
            g (cxt, ident, (patts, expr)) = 
                B.RECORD_EXPR_HIGHER_ORDER_PHRASE (toBnfcIdent ident) 
                    $ B.PATTERN_TO_EXPR (map mplPattToBnfc patts) (f expr)
        f (EFold cxt expr phrases) = B.FOLD_EXPR (f expr) $ NE.toList $ fmap g phrases
          where
            g (cxt, ident, patts, expr) = 
                B.FOLD_EXPR_PHRASE (toBnfcIdent ident) bnfcKeyword (map mplPattToBnfc patts) (f expr)
        f (EUnfold cxt expr phrases) = B.UNFOLD_EXPR (f expr) $ NE.toList $ fmap g phrases
          where
            g (cxt, patt, foldphrases) = 
                B.UNFOLD_EXPR_PHRASE (mplPattToBnfc patt) $ NE.toList $ fmap h foldphrases
            h (cxt, ident, patts, expr) = 
                B.FOLD_EXPR_PHRASE (toBnfcIdent ident) bnfcKeyword (map mplPattToBnfc patts) (f expr)

        {-
        EVar !(XEVar x) (IdP x)
        EInt !(XEInt x) Int
        EChar !(XEChar x) Char
        EDouble !(XEDouble x) Double
        ECase !(XECase x) (MplExpr x) (NonEmpty (XMplPattern x, MplExpr x))
    
        EObjCall !(XEObjCall x) (IdP x) [MplExpr x]
        ECall !(XECall x) (IdP x) [MplExpr x]
        ERecord !(XERecord x) (NonEmpty (XERecordPhrase x, IdP x, ([XMplPattern x], MplExpr x)))
    
         built in...
        EList !(XEList x) [MplExpr x]
        EString !(XEString x) [MplExpr x]
        EUnit !(XEUnit x) 
        ETuple !(XETuple x) (MplExpr x, MplExpr x, [MplExpr x])
        EBuiltInOp !(XEBuiltInOp x) BuiltInOperators (MplExpr x) (MplExpr x)
    
    
        EIf !(XEIf x) (MplExpr x) (MplExpr x) (MplExpr x)
        ELet !(XELet x) (NonEmpty (MplStmt x)) (MplExpr x)
        EFold !(XEFold x) (MplExpr x)
            (NonEmpty 
                ( XEFoldPhrase x
                , IdP x
                , [XMplPattern x]
                , (MplExpr x)
                ) 
            )
        | EUnfold 
            !(XEUnfold x) 
            (MplExpr x)
            ( NonEmpty 
                ( XEUnfoldPhrase x
                , XMplPattern x
                , NonEmpty (XEFoldPhrase x, IdP x, [XMplPattern x], (MplExpr x))
                )
            )
        | ESwitch !(XESwitch x) (NonEmpty ((MplExpr x), (MplExpr x)))
    
        | XExpr !(XXExpr x) -}

instance PPrint PrimitiveOperators where
    pprint = go
      where
        go PrimitiveAdd = "+"
        go PrimitiveSub = "-"
        go _ = error "error in print -- not implemented yet"

class ToBnfcIdent t where
    toBnfcIdent :: PPrint a => a -> t

instance ToBnfcIdent B.PIdent where
    toBnfcIdent n =  B.PIdent ((-1,-1), pprint n)

instance ToBnfcIdent B.PDouble where
    toBnfcIdent n =  B.PDouble ((-1,-1), pprint n)

instance ToBnfcIdent B.UIdent where
    toBnfcIdent n =  B.UIdent ((-1,-1), pprint n)

instance ToBnfcIdent B.UPIdent where
    toBnfcIdent n =  B.UPIdent ((-1,-1), pprint n)

instance ToBnfcIdent B.SplitChannel where
    toBnfcIdent n =  B.SPLIT_CHANNEL $ toBnfcIdent n

instance ToBnfcIdent B.PInteger where
    toBnfcIdent n =  B.PInteger ((-1,-1), pprint n)

instance ToBnfcIdent B.ForkChannel where
    toBnfcIdent n =  B.FORK_CHANNEL $ toBnfcIdent n

instance ToBnfcIdent B.ForallVarList where
    toBnfcIdent n =  B.MPL_SEQ_FUN_TYPE_FORALL_LIST $ toBnfcIdent n

instance ToBnfcIdent B.TypeHandleName where
    toBnfcIdent n =  B.TYPE_HANDLE_NAME $ toBnfcIdent n


instance ToBnfcIdent B.Infixl5op where
    toBnfcIdent n =  B.Infixl5op ((-1,-1), pprint n) 

class BnfcKeyword t where
    bnfcKeyword :: t

instance BnfcKeyword B.Case where
    bnfcKeyword = B.Case ((-1,-1), "case")

instance BnfcKeyword B.LBracket where
    bnfcKeyword = B.LBracket ((-1,-1), "(")

instance BnfcKeyword B.RBracket where
    bnfcKeyword = B.RBracket ((-1,-1), ")")

instance BnfcKeyword B.NullPattern where
    bnfcKeyword = B.NullPattern ((-1,-1), "_")

instance BnfcKeyword B.Close where
    bnfcKeyword = B.Close ((-1,-1), "close")

instance BnfcKeyword B.Halt where
    bnfcKeyword = B.Halt ((-1,-1), "halt")

instance BnfcKeyword B.Get where
    bnfcKeyword = B.Get ((-1,-1), "get")

instance BnfcKeyword B.Put where
    bnfcKeyword = B.Put ((-1,-1), "put")

instance BnfcKeyword B.HCase where
    bnfcKeyword = B.HCase ((-1,-1), "hcase")

instance BnfcKeyword B.HPut where
    bnfcKeyword = B.HPut ((-1,-1), "hput")

instance BnfcKeyword B.Split where
    bnfcKeyword = B.Split ((-1,-1), "split")

instance BnfcKeyword B.Fork where
    bnfcKeyword = B.Fork ((-1,-1), "fork")

instance BnfcKeyword B.ChId where
    bnfcKeyword = B.ChId ((-1,-1), "|=|")

instance BnfcKeyword B.Par where
    bnfcKeyword = B.Par ((-1,-1), "(+)")

instance BnfcKeyword B.Tensor where
    bnfcKeyword = B.Tensor ((-1,-1), "(*)")

instance BnfcKeyword B.Colon where
    bnfcKeyword = B.Colon ((-1,-1), ":")
