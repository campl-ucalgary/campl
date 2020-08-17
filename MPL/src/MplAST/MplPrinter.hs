{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
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

import qualified Language.AbsMPL as B
import qualified Language.ErrM as B
import qualified Language.LayoutMPL as B
import qualified Language.LexMPL as B
import qualified Language.ParMPL as B
import qualified Language.PrintMPL as B
import qualified Language.SkelMPL as B

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import GHC.Generics 
import Data.Void

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Data
import Data.Kind
import Data.Bool
import Data.Maybe

class PPrint a where
    pprint :: a -> String

instance PPrint Int where
    pprint n = show n


{-
instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (Type calldef ident typevar) where
    pprint = printTree . translateTypeToBnfcType

instance (PPrint ident, Eq typevar, PPrint typevar, PPrint chident) => PPrint (ExprG ident typevar chident) where
    pprint = printTree . translateExprGToBnfcExpr

instance (PPrint ident, Eq typevar, PPrint typevar) => PPrint (PatternG ident typevar) where
    pprint = printTree . translatePatternGtoBnfcPattern

instance (PPrint ident, Eq typevar, PPrint typevar, PPrint chident) => PPrint (FunctionDefG ident typevar chident) where
    pprint = printTree . translateFunctionGToBnfcFunction

instance (PPrint ident, Eq typevar, PPrint typevar, PPrint chident) => PPrint (ProcessDefG ident typevar chident) where
    pprint = printTree . translateProcessGToBnfcProcess
    -}

class MplPattToBnfc t where
    mplPattToBnfc ::  t -> B.Pattern

instance ( PPrint (IdP x) ) => MplPattToBnfc (MplPattern x) where
    mplPattToBnfc = f
      where
        f (PVar _ id) = B.VAR_PATTERN $ toBnfcIdent id
        f (PConstructor _ id args) = 
            B.CONSTRUCTOR_PATTERN_ARGS (toBnfcIdent id) bnfcKeyword (map f args) bnfcKeyword 
        f (PNull _) = B.NULL_PATTERN bnfcKeyword

class MplCmdToBnfc t where
    mplCmdToBnfc :: t -> B.ProcessCommand

instance ( PPrint (IdP x), PPrint (ChP x), MplExprToBnfc (XMplExpr x), MplPattToBnfc (XMplPattern x) ) => MplCmdToBnfc (MplCmd x) where
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
            B.PROCESS_FORK bnfcKeyword (toBnfcIdent ch) $ map g [a, b]
          where
            g (a,_, cmds)= B.FORK_PHRASE (toBnfcIdent a) $ mplCmdsToBnfc cmds
        f (CId _ (a,b)) = B.PROCESS_ID (toBnfcIdent a) bnfcKeyword (toBnfcIdent b)
        f (CIdNeg _ (a,b)) = B.PROCESS_NEG (toBnfcIdent a) bnfcKeyword (toBnfcIdent b)
        f (CRace _ races) = B.PROCESS_RACE $ NE.toList $ fmap g races
          where
            g (ch, cmds) = B.RACE_PHRASE (toBnfcIdent ch) $ mplCmdsToBnfc cmds
        -- TODO -- we should actually show the generated context here!
        f (CPlugs _ (a,b,c)) = B.PROCESS_PLUG $ map g (a:b:c)
          where
            g (_, (a,b), cmds) = B.PLUG_PHRASE_AS 
                (map toBnfcIdent a) (map toBnfcIdent b) (mplCmdsToBnfc cmds)

mplCmdsToBnfc :: MplCmdToBnfc t => NonEmpty t -> B.ProcessCommandsBlock
mplCmdsToBnfc (cmd :| []) = B.PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK $ mplCmdToBnfc cmd
mplCmdsToBnfc cmds = B.PROCESS_COMMANDS_DO_BLOCK $ NE.toList $ fmap mplCmdToBnfc cmds

class MplExprToBnfc t where
    mplExprToBnfc :: t -> B.Expr

instance ( PPrint (IdP x), MplPattToBnfc (XMplPattern x) ) => MplExprToBnfc (MplExpr x) where
    mplExprToBnfc = f
      where
        f (EPOps _ op exp0 exp1) = undefined
        f (EVar _ id) = B.VAR_EXPR $ toBnfcIdent id
        f (EInt _ id) = B.INT_EXPR $ toBnfcIdent id
        f (EChar _ id) = B.CHAR_EXPR id
        f (EDouble _ id) = B.DOUBLE_EXPR id
        f (ECase _ expr pattexprs) = B.CASE_EXPR bnfcKeyword (f expr) $ NE.toList $ fmap g pattexprs
          where
            g (patt, expr) = B.PATTERN_TO_EXPR [mplPattToBnfc patt] (mplExprToBnfc expr)
        f (EObjCall _ id exprs) = B.DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR  
            (toBnfcIdent id) bnfcKeyword (map f exprs) bnfcKeyword 
        f (ECall _ id exprs) = B.FUN_EXPR  
            (toBnfcIdent id) bnfcKeyword (map f exprs) bnfcKeyword 
    
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

class ToBnfcIdent t where
    toBnfcIdent :: PPrint a => a -> t

instance ToBnfcIdent B.PIdent where
    toBnfcIdent n =  B.PIdent ((-1,-1), pprint n)

instance ToBnfcIdent B.UIdent where
    toBnfcIdent n =  B.UIdent ((-1,-1), pprint n)

instance ToBnfcIdent B.UPIdent where
    toBnfcIdent n =  B.UPIdent ((-1,-1), pprint n)

instance ToBnfcIdent B.SplitChannel where
    toBnfcIdent n =  B.SPLIT_CHANNEL $ toBnfcIdent n

instance ToBnfcIdent B.PInteger where
    toBnfcIdent n =  B.PInteger ((-1,-1), pprint n)

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
