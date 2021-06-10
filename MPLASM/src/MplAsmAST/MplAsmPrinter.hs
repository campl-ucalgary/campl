{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module MplAsmAST.MplAsmPrinter where

import Optics
import Control.Arrow
import Data.Coerce

import MplAsmAST.MplAsmCommand
import MplAsmAST.MplAsmProg

import qualified MplAsmLanguage.ParMPLASM as B
import qualified MplAsmLanguage.AbsMPLASM as B
import qualified MplAsmLanguage.PrintMPLASM as B

import Data.Functor.Foldable

-- | type class for pretty printing things regarding 
class PPrint t where
    pprint :: t -> String

instance PPrint Int where
    pprint = show

instance PPrint Word where
    pprint = show

instance PPrint Char where
    pprint = show

instance PPrint Bool where
    pprint = show

instance PPrint Name where
    pprint = coerce

instance ( PPrint (IdP x) ) => PPrint (MplAsmProg x) where
    pprint = B.printTree . mplAsmProgToBnfc

-- | type class for converting types to a bnfc ident
class ToBnfcIdent t where
    toBnfcIdent :: PPrint a => a -> t

instance ToBnfcIdent B.PIdent where
    toBnfcIdent idp = B.PIdent 
        (invalidPosition, pprint idp)

instance ToBnfcIdent B.UIdent where
    toBnfcIdent idp = B.UIdent 
        (invalidPosition, pprint idp)

instance ToBnfcIdent B.PInteger where
    toBnfcIdent idp = B.PInteger (invalidPosition, pprint idp)

instance ToBnfcIdent B.Character where
    toBnfcIdent idp = B.Character (invalidPosition, pprint idp)

instance ToBnfcIdent B.BBool where
    toBnfcIdent idp = B.BBool (invalidPosition, pprint idp)

invalidPosition :: (Int, Int)
invalidPosition = (-1,-1)


mplAsmProgToBnfc :: 
    ( PPrint (IdP x) ) => 
    MplAsmProg x -> 
    B.AMPLCODE
mplAsmProgToBnfc prog = B.AMPLCODE
    (map mplAsmStmtToBnfc (view mplAsmStmts prog))
    $ case view mplAsmMain prog of
        Just (mainname, (seqs, ins, outs), coms) -> 
            B.MAIN 
                bnfcKeyword 
                (B.MAIN_CHANNELS 
                    (map toBnfcIdent ins) 
                    (map toBnfcIdent outs))
                (B.Prog (map mplAsmComToBnfcCom coms))
        Nothing -> B.NO_MAIN

{- | converts a statement into the corresponding bnfc type -}
mplAsmStmtToBnfc ::
    ( PPrint (IdP x) ) => 
    MplAsmStmt x ->
    B.AmplConstructs
mplAsmStmtToBnfc stmt = case stmt of
    Protocols typesandpecs -> 
        B.PROTOCOL_CONSTRUCT 
        $ B.PROTOCOLS 
        $ concobj typesandpecs 
    Coprotocols typesandpecs -> 
        B.COPROTOCOL_CONSTRUCT 
        $ B.COPROTOCOLS 
        $ concobj typesandpecs 
    Constructors typesandpecs -> 
        B.CONSTRUCTOR_CONSTRUCT 
        $ B.CONSTRUCTORS 
        $ seqobj typesandpecs 
    Destructors typesandpecs -> 
        B.DESTRUCTOR_CONSTRUCT 
        $ B.DESTRUCTORS 
        $ seqobj typesandpecs 

    Functions funs -> 
        B.FUNCTIONS_CONSTRUCT
        $ B.FUNCTIONS
        $ funs <&> \(fname, fargs, cmds) -> 
            B.FUNCTION_SPEC
                (toBnfcIdent fname) 
                (map toBnfcIdent fargs)
                (B.Prog (map mplAsmComToBnfcCom cmds))
    Processes procs -> 
        B.PROCESSES_CONSTRUCT 
        $ B.PROCESSES 
        $ procs <&> \(pname, (seqs, ins, outs), cmds) ->
            B.PROCESS_SPEC
                (toBnfcIdent pname)
                (map toBnfcIdent seqs)
                (map toBnfcIdent ins)
                (map toBnfcIdent outs)
                (B.Prog (map mplAsmComToBnfcCom cmds))

  where
    concobj = map f
      where
        f (TypeAndConcSpecs a handles) =
            B.PROTOCOL_COPROTOCOL_SPEC 
                (toBnfcIdent a)
                (map (B.HANDLE_NAME . toBnfcIdent) handles)

    seqobj = map f
      where
        f (TypeAndSeqSpecs a handles) = 
            B.STRUCT_SPEC 
                (toBnfcIdent a)
                (map (uncurry B.STRUCT <<< toBnfcIdent *** toBnfcIdent ) handles)

{- | converts 'MplAsmCom x' to the corresponding bnfc type -}
mplAsmComToBnfcCom :: 
    ( PPrint (IdP x) ) => 
    MplAsmCom x ->
    B.Com
mplAsmComToBnfcCom = cata go
  where
    go = \case
        CAssignF _ idp com -> 
            B.AC_ASSIGN (toBnfcIdent idp) com

        CStoreF _ idp -> B.AC_LOAD bnfcKeyword (toBnfcIdent idp)
        CLoadF _ idp -> B.AC_LOAD bnfcKeyword (toBnfcIdent idp)

        CBoolF _ bval -> B.AC_BOOL bnfcKeyword (toBnfcIdent bval)

        CRetF _ -> B.AC_RET bnfcKeyword
        CCallF _ idp args ->
            B.AC_CALL_FUN bnfcKeyword
                (toBnfcIdent idp)
                (map toBnfcIdent args)
        CIntF _ n ->  
            B.AC_INT 
                bnfcKeyword
                (toBnfcIdent n)
        CCharF _ c -> 
            B.AC_CHAR
                bnfcKeyword
                (toBnfcIdent c)
        CEqIntF _ -> 
            B.AC_EQI bnfcKeyword
        CEqCharF _ -> 
            B.AC_EQC bnfcKeyword

        CAddF _ -> B.AC_ADD bnfcKeyword
        CSubF _ -> B.AC_SUB bnfcKeyword
        CMulF _ -> B.AC_MUL bnfcKeyword
        CConstructorF _ (TypeAndSpec  a b) args ->
            B.AC_CONSTRUCTOR_ARGS 
                (toBnfcIdent a) 
                (toBnfcIdent b) 
                (map toBnfcIdent args)

        CDestructorF _ (TypeAndSpec a b) args destruct -> 
            B.AC_DEST_ARGS 
                (toBnfcIdent a) 
                (toBnfcIdent b)
                (map toBnfcIdent args)
                (toBnfcIdent destruct)
            
        CCaseF _ caseon casephrases ->
            B.AC_CASE bnfcKeyword
                (toBnfcIdent caseon)
                (map toLabelledSeqComs casephrases)
        CRecordF _ records -> 
            B.AC_RECORD 
                bnfcKeyword
                (map toLabelledSeqComs records)

        CIfF _ idp thenc elsec ->
            B.AC_IF 
                bnfcKeyword 
                (toBnfcIdent idp) 
                (B.Prog thenc) 
                (B.Prog elsec)

        CTupleF _ args -> 
            B.AC_PROD (map toBnfcIdent args)
        CProjF _ projnum tuple ->
            B.AC_PRODELEM 
                (toBnfcIdent $ (fromIntegral projnum :: Int)) 
                (toBnfcIdent tuple)
        CGetF _ v ch -> B.AC_GET 
            bnfcKeyword (toBnfcIdent v) (toBnfcIdent ch)

        CPutF _ v ch -> B.AC_PUT
            bnfcKeyword (toBnfcIdent v) (toBnfcIdent ch)

        CHPutF _ (TypeAndSpec a b) ch ->
            B.AC_HPUT bnfcKeyword 
                (toBnfcIdent a)
                (toBnfcIdent b)
                (toBnfcIdent ch)
        CHCaseF _ ch labels ->
            B.AC_HCASE 
                bnfcKeyword 
                (toBnfcIdent ch)
                (map toLabelledConcComs labels)

        CSplitF _ ch (ch0, ch1) ->
            B.AC_SPLIT 
                bnfcKeyword 
                    (toBnfcIdent ch)
                    (toBnfcIdent ch0)
                    (toBnfcIdent ch1)
        CForkF _ ch ((ch0, ch0withs, cmds0), (ch1, ch1withs, cmds1)) ->
            B.AC_FORK bnfcKeyword 
                (toBnfcIdent ch)
                (toBnfcIdent ch0) 
                    (map toBnfcIdent ch0withs) 
                    (B.Prog cmds0)
                (toBnfcIdent ch1) 
                    (map toBnfcIdent ch1withs) 
                    (B.Prog cmds1)
        CPlugF _ plugs ((withs0, cmds0), (withs1, cmds1)) ->
            B.AC_PLUG 
                bnfcKeyword
                (map toBnfcIdent plugs)
                (map toBnfcIdent withs0)
                    (B.Prog cmds0)
                (map toBnfcIdent withs1)
                    (B.Prog cmds1)

        CRunF _ runner (seqs, ins, outs) -> 
            B.AC_RUN 
                bnfcKeyword
                (toBnfcIdent runner)
                (map toBnfcIdent seqs)
                (map toBnfcIdent ins)
                (map toBnfcIdent outs)

        CIdF _ (l, r) ->
            B.AC_ID (toBnfcIdent l) bnfcKeyword (toBnfcIdent r)

        CRaceF _ phrases -> 
            B.AC_RACE 
                bnfcKeyword 
                (map f phrases)
          where
            f (idp, coms) = B.AC_RACE_PHRASE
                (toBnfcIdent idp)
                (B.Prog coms)
        CCloseF _ idp -> B.AC_CLOSE bnfcKeyword (toBnfcIdent idp)
        CHaltF _ idp -> B.AC_HALT bnfcKeyword (toBnfcIdent idp)


    toLabelledSeqComs (TypeAndSpec a b, args, coms) = 
        B.AC_LABELLED_COMS
            (toBnfcIdent a)
            (toBnfcIdent b)
            (map toBnfcIdent args)
            (B.Prog coms)

    toLabelledConcComs (TypeAndSpec a b, coms) = 
        B.AC_LABELLED_COMS_NO_ARGS
            (toBnfcIdent a)
            (toBnfcIdent b)
            (B.Prog coms) 

class BnfcKeyword t where
    bnfcKeyword :: t

instance BnfcKeyword B.Load where
    bnfcKeyword = B.Load (invalidPosition, "load")

instance BnfcKeyword B.Store where
    bnfcKeyword = B.Store (invalidPosition, "store")

instance BnfcKeyword B.Call where
    bnfcKeyword = B.Call (invalidPosition, "call")

instance BnfcKeyword B.Ret where
    bnfcKeyword = B.Ret (invalidPosition, "ret")

instance BnfcKeyword B.CInt where
    bnfcKeyword = B.CInt (invalidPosition, "cInt")

instance BnfcKeyword B.CChar where
    bnfcKeyword = B.CChar (invalidPosition, "cChar")

instance BnfcKeyword B.EqI where
    bnfcKeyword = B.EqI (invalidPosition, "eqi")

instance BnfcKeyword B.EqC where
    bnfcKeyword = B.EqC (invalidPosition, "eqc")

instance BnfcKeyword B.Case where
    bnfcKeyword = B.Case (invalidPosition, "case")

instance BnfcKeyword B.Add where
    bnfcKeyword = B.Add (invalidPosition, "add")

instance BnfcKeyword B.Subtract where
    bnfcKeyword = B.Subtract (invalidPosition, "subtract")

instance BnfcKeyword B.Mul where
    bnfcKeyword = B.Mul (invalidPosition, "mul")

instance BnfcKeyword B.Rec where
    bnfcKeyword = B.Rec (invalidPosition, "rec")

instance BnfcKeyword B.If where
    bnfcKeyword = B.If (invalidPosition, "if")

instance BnfcKeyword B.Get where
    bnfcKeyword = B.Get (invalidPosition, "get")

instance BnfcKeyword B.Put where
    bnfcKeyword = B.Put (invalidPosition, "put")

instance BnfcKeyword B.Hput where
    bnfcKeyword = B.Hput (invalidPosition, "hput")

instance BnfcKeyword B.Hcase where
    bnfcKeyword = B.Hcase (invalidPosition, "hcase")

instance BnfcKeyword B.Split where
    bnfcKeyword = B.Split (invalidPosition, "split")

instance BnfcKeyword B.Fork where
    bnfcKeyword = B.Fork (invalidPosition, "fork")

instance BnfcKeyword B.Plug where
    bnfcKeyword = B.Plug (invalidPosition, "plug")

instance BnfcKeyword B.Run where
    bnfcKeyword = B.Run (invalidPosition, "run")

instance BnfcKeyword B.Ch_Id where
    bnfcKeyword = B.Ch_Id (invalidPosition, "|=|")

instance BnfcKeyword B.Race where
    bnfcKeyword = B.Race (invalidPosition, "race")

instance BnfcKeyword B.Close where
    bnfcKeyword = B.Close (invalidPosition, "close")

instance BnfcKeyword B.Halt where
    bnfcKeyword = B.Halt (invalidPosition, "halt")

instance BnfcKeyword B.Main_run where
    bnfcKeyword = B.Main_run (invalidPosition, "run")

instance BnfcKeyword B.CBool where
    bnfcKeyword = B.CBool (invalidPosition, "cBool")
