{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module MplAsmPasses.Parse.Parse where

import MplAsmAST.MplAsmCore
import MplAsmPasses.Parse.ParseErrors
import MplAsmPasses.Parse.ParseAST

import Optics
import Data.Function

import MplAsmPasses.Parse.BnfcParse
import Control.Monad.Writer

import qualified MplAsmLanguage.AbsMPLASM as B

import Data.Coerce

{- | parses a string into a list of @MplAsmStmt MplAsmParsed@ -}
mplAsmParse :: 
    AsParseError err =>
    String ->
    Either [err] (MplAsmProg MplAsmParsed)
mplAsmParse str = bnfcParse str >>= go
  where
    -- go :: B.AMPLCODE -> Either [_] [MplAsmStmt MplAsmParsed]
    go (B.AMPLCODE constrs mainf) = case runWriter $ (,) <$> mplAsmParseStmts constrs <*> mplAsmParseMain mainf of
        (a, []) -> Right $ _MplAsmProg # a

        (_, errs) -> Left errs

mplAsmParseStmts :: 
    ( MonadWriter [err] m
    , AsParseError err ) =>
    [B.AmplConstructs] -> 
    m [MplAsmStmt MplAsmParsed]
mplAsmParseStmts constrs = traverse go constrs
  where
    go = \case 
        B.IMPORT_CONSTRUCT _ -> error "no importing implemented yet TODO"

        B.PROTOCOL_CONSTRUCT (B.PROTOCOLS procs) -> fmap Protocols $ traverse f procs
        B.COPROTOCOL_CONSTRUCT (B.COPROTOCOLS procs) -> fmap Coprotocols $ traverse f procs
        B.CONSTRUCTOR_CONSTRUCT (B.CONSTRUCTORS structorspec) -> fmap Constructors $ traverse g structorspec
        B.DESTRUCTOR_CONSTRUCT (B.DESTRUCTORS structorspec) -> fmap Destructors $ traverse g structorspec

        B.PROCESSES_CONSTRUCT (B.PROCESSES procs) -> fmap Processes $ traverse k procs
          where
            -- k :: B.ProcessesSpec -> m (IdP x, ([IdP x], [IdP x], [IdP x]), MplAsmComs x)
            k (B.PROCESS_SPEC pname seqs ins outs coms) = do
                coms' <- mplAsmParseComs coms
                return (toLocatedName pname, (map toLocatedName seqs, map toLocatedName ins, map toLocatedName outs), coms')
        B.FUNCTIONS_CONSTRUCT (B.FUNCTIONS funs) -> fmap Functions $ traverse k funs
          where
            -- k :: B.FunctionsSpec -> m (Name, [Name], MplAsmComs MplAsmParsed)
            k (B.FUNCTION_SPEC fname args coms) = do
                coms' <- mplAsmParseComs coms
                return (toLocatedName fname, map toLocatedName args, coms')

    -- f :: B.ProtocolCoprotocolSpec -> m (TypeAndConcSpecs x)
    f (B.PROTOCOL_COPROTOCOL_SPEC tp handles) = return $ TypeAndConcSpecs (toLocatedName tp) (map (\(B.HANDLE_NAME h) -> toLocatedName h) handles)

    -- g :: B.StructorSpec -> m (TypeAndSeqSpecs x)
    g (B.STRUCT_SPEC tp structs) = return $ TypeAndSeqSpecs (toLocatedName tp) (map (\(B.STRUCT spec numargs) -> (toLocatedName spec, fromIntegral $ pIntToInt numargs)) structs)




mplAsmParseMain ::
    ( MonadWriter [err] m
    , AsParseError err ) =>
    B.Main ->
    m (
        Maybe 
            ( IdP MplAsmParsed
            , ([IdP MplAsmParsed], [IdP MplAsmParsed], [IdP MplAsmParsed])
            , MplAsmComs MplAsmParsed
            )
        )
mplAsmParseMain mainf = case mainf of
    B.MAIN mainident (B.MAIN_CHANNELS ins outs) coms -> do
        coms' <- mplAsmParseComs coms
        return $ Just (toLocatedName mainident, ([], map toLocatedName ins, map toLocatedName outs), coms')
    B.NO_MAIN -> return Nothing

mplAsmParseComs :: 
    ( MonadWriter [err] m
    , AsParseError err ) =>
    B.Coms -> 
    m (MplAsmComs MplAsmParsed)
mplAsmParseComs (B.Prog coms) = traverse mplAsmParseCom coms

mplAsmParseCom ::
    ( MonadWriter [err] m
    , AsParseError err ) =>
    B.Com -> 
    m (MplAsmCom MplAsmParsed)
mplAsmParseCom = go
  where
    go = \case 
        B.AC_ASSIGN ident com -> 
            CAssign (toRowCol ident) (toLocatedName ident) <$> go com
        B.AC_LOAD _ ident -> 
            return $ CLoad (toRowCol ident) $ toLocatedName ident
        B.AC_RET keyword ->  return $  CRet $ toRowCol keyword 
        B.AC_CALL_FUN keyword ident args ->
            pure $ CCall (toRowCol keyword) (toLocatedName ident) $ map toLocatedName args
        B.AC_INT keyword pint -> pure $ CInt (toRowCol keyword) (pIntToInt pint)
        B.AC_CHAR keyword cchar -> 
            pure $ CChar (toRowCol keyword) (pCharToChar cchar)
{-
    B.AC_STRING CString String
    B.AC_TOSTR ToStr
    B.AC_TOINT ToInt
    B.AC_AND And
    B.AC_OR Or
    B.AC_APPEND Append
-}
        B.AC_BOOL keyword bbool -> pure $ CBool (toRowCol keyword) (pBoolToBool bbool)
    {-
    B.AC_UNSTRING Unstring
    -}
        B.AC_LEQ leqi -> pure $ CLeqInt (toRowCol leqi)
        B.AC_EQI eqi -> pure $ CEqInt (toRowCol eqi)
        B.AC_LEQC leqc -> pure $ CLeqChar (toRowCol leqc)
        B.AC_EQC eqc -> pure $ CEqChar (toRowCol eqc)
    {-
    | B.AC_LEQS Leqs
    | B.AC_EQS Eqs
    | B.AC_CONCAT ConcatS Integer
    -}
        B.AC_ADD keyword -> pure $ CAddInt (toRowCol keyword)
        B.AC_SUB keyword -> pure $ CSubInt (toRowCol keyword)
        B.AC_MUL keyword -> pure $ CMulInt (toRowCol keyword)
        B.AC_DIVR keyword -> pure $ CDivInt (toRowCol keyword)
    {-
    | B.AC_DIVQ Quot
    | B.AC_DIVR Rem
    -}
        B.AC_CONSTRUCTOR tp spec -> 
            pure $ CConstructor (toRowCol tp) 
                (TypeAndSpec (toLocatedName tp) (toLocatedName spec)) []

        B.AC_CONSTRUCTOR_ARGS tp spec args -> 
            pure $ CConstructor 
                (toRowCol tp) 
                (TypeAndSpec (toLocatedName tp) (toLocatedName spec)) 
                (map toLocatedName args)

        B.AC_CASE keyword expr labelcoms -> do
            labelcoms' <- traverse labelledSeqComs labelcoms
            return $ CCase 
                (toRowCol keyword)
                (toLocatedName expr)
                labelcoms'

        B.AC_IF keyword expr thenc elsec -> do
            thenc' <- mplAsmParseComs thenc
            elsec' <- mplAsmParseComs elsec
            return $ CIf 
                (toRowCol keyword) 
                (toLocatedName expr) 
                thenc' 
                elsec'

        B.AC_RECORD keyword labelcoms -> do
            labelcoms' <- traverse labelledSeqComs labelcoms
            return $ CRecord 
                (toRowCol keyword)
                labelcoms'

        B.AC_DEST tp spec expr -> 
            return $ CDestructor 
                (toRowCol tp) 
                (TypeAndSpec (toLocatedName tp) (toLocatedName spec)) 
                [] 
                (toLocatedName expr)

        B.AC_DEST_ARGS tp spec args expr ->
            return $ CDestructor 
                (toRowCol tp) 
                (TypeAndSpec (toLocatedName tp) (toLocatedName spec)) 
                (map toLocatedName args) 
                (toLocatedName expr)

        B.AC_PROD args -> 
            -- TODO: welp 
            return $ CTuple (-1,-1) $ map toLocatedName args

        B.AC_PRODELEM projnum expr -> 
            return $ CProj 
                (toRowCol projnum) 
                (fromIntegral $ pIntToInt projnum)
                (toLocatedName expr)
    {-
    | B.AC_EMSG String
    -}

        B.AC_GET keyword patt ch ->
            pure $ CGet 
                (toRowCol keyword) 
                (toLocatedName patt)
                (toLocatedName ch)
            
        B.AC_PUT keyword expr ch ->
            pure $ CPut 
                (toRowCol keyword) 
                (toLocatedName expr)
                (toLocatedName ch)

        B.AC_HPUT keyword tp spec ch ->
            pure $ 
                CHPut
                (toRowCol keyword)
                (TypeAndSpec (toLocatedName tp) (toLocatedName spec))
                (toLocatedName ch)

        B.AC_HCASE keyword ch labelledcoms -> do
            labelledcoms' <- traverse f labelledcoms 
            return $ CHCase 
                (toRowCol keyword)
                (toLocatedName ch)
                labelledcoms'
          where
            f labelledcoms = case labelledcoms of
                B.AC_LABELLED_COMS_NO_ARGS tp spec coms -> do
                    coms' <- mplAsmParseComs coms
                    return (TypeAndSpec (toLocatedName tp) (toLocatedName spec), coms' )

                B.AC_LABELLED_COMS tp spec [] coms -> do
                    coms' <- mplAsmParseComs coms
                    return (TypeAndSpec (toLocatedName tp) (toLocatedName spec), coms' )

                B.AC_LABELLED_COMS tp spec _ coms -> do
                    tell [_BnfcParseError # "Illegal hcase expression"]
                    coms' <- mplAsmParseComs coms
                    return (TypeAndSpec (toLocatedName tp) (toLocatedName spec), coms' )
        
        B.AC_SPLIT keyword ch lch rch -> 
            return $ CSplit 
                (toRowCol keyword)
                (toLocatedName ch)
                (toLocatedName lch, toLocatedName rch)
        B.AC_FORK keyword ch ch0 withs0 coms0 ch1 withs1 coms1 -> do
            coms0' <- mplAsmParseComs coms0
            coms1' <- mplAsmParseComs coms1
            return $ CFork 
                (toRowCol keyword)
                (toLocatedName ch)
                ( (toLocatedName ch0, map toLocatedName withs0, coms0')
                , (toLocatedName ch1, map toLocatedName withs1, coms1')
                )

        B.AC_PLUG keyword plugs withsins0 withsouts0 coms0 withsins1 withsouts1 coms1 -> do
            coms0' <- mplAsmParseComs coms0
            coms1' <- mplAsmParseComs coms1

            return $ CPlug 
                (toRowCol keyword)
                (map toLocatedName plugs)
                ( ((map toLocatedName withsins0, map toLocatedName withsouts0), coms0')
                , ((map toLocatedName withsins1, map toLocatedName withsouts1), coms1')
                )
        B.AC_RUN keyword proc seqs ins outs -> 
            return $ CRun 
                (toRowCol keyword)
                False
                (toLocatedName proc)
                ( map toLocatedName seqs
                , map toLocatedName ins
                , map toLocatedName outs)

        B.AC_ID ch0 keyword ch1 -> 
            return $ CId 
                (toRowCol keyword)
                (toLocatedName ch0, toLocatedName ch1)

        B.AC_RACE keyword phrases -> do
            phrases' <- traverse f phrases
            return $ CRace (toRowCol keyword) phrases'
          where
            f (B.AC_RACE_PHRASE ch coms) = do
                coms' <- mplAsmParseComs coms
                return (toLocatedName ch, coms')

        B.AC_CLOSE keyword ch -> 
            return $ CClose 
                (toRowCol keyword)
                (toLocatedName ch)
        B.AC_HALT keyword ch -> 
            return $ CHalt 
                (toRowCol keyword)
                (toLocatedName ch)

    labelledSeqComs labelledcoms = case labelledcoms of
        B.AC_LABELLED_COMS_NO_ARGS tp spec coms -> do
            coms' <- mplAsmParseComs coms
            return (TypeAndSpec (toLocatedName tp) (toLocatedName spec), [], coms' )
        B.AC_LABELLED_COMS tp spec args coms -> do
            coms' <- mplAsmParseComs coms
            return (TypeAndSpec (toLocatedName tp) (toLocatedName spec), map toLocatedName args, coms' )
        

