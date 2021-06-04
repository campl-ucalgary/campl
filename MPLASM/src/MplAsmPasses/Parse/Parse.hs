{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
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
                return (toIdent pname, (map toIdent seqs, map toIdent ins, map toIdent outs), coms')
        B.FUNCTIONS_CONSTRUCT (B.FUNCTIONS funs) -> fmap Functions $ traverse k funs
          where
            -- k :: B.FunctionsSpec -> m (Ident, [Ident], MplAsmComs MplAsmParsed)
            k (B.FUNCTION_SPEC fname args coms) = do
                coms' <- mplAsmParseComs coms
                return (toIdent fname, map toIdent args, coms')

    -- f :: B.ProtocolCoprotocolSpec -> m (TypeAndConcSpecs x)
    f (B.PROTOCOL_COPROTOCOL_SPEC tp handles) = return $ TypeAndConcSpecs (toIdent tp) (map (\(B.HANDLE_NAME h) -> toIdent h) handles)

    -- g :: B.StructorSpec -> m (TypeAndSeqSpecs x)
    g (B.STRUCT_SPEC tp structs) = return $ TypeAndSeqSpecs (toIdent tp) (map (\(B.STRUCT spec numargs) -> (toIdent spec, fromIntegral $ pIntToInt numargs)) structs)




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
        return $ Just (toIdent mainident, ([], map toIdent ins, map toIdent outs), coms')
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
            CAssign (toRowCol ident) (toIdent ident) <$> go com
        B.AC_LOAD _ ident -> 
            return $ CLoad (toRowCol ident) $ toIdent ident
        B.AC_RET keyword ->  return $  CRet $ toRowCol keyword 
        B.AC_CALL_FUN keyword ident args ->
            pure $ CCall (toRowCol keyword) (toIdent ident) $ map toIdent args
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
        B.AC_ADD keyword -> pure $ CAdd (toRowCol keyword)
        B.AC_SUB keyword -> pure $ CSub (toRowCol keyword)
        B.AC_MUL keyword -> pure $ CMul (toRowCol keyword)
    {-
    | B.AC_DIVQ Quot
    | B.AC_DIVR Rem
    -}
        B.AC_CONSTRUCTOR tp spec -> 
            pure $ CConstructor (toRowCol tp) 
                (TypeAndSpec (toIdent tp) (toIdent spec)) []

        B.AC_CONSTRUCTOR_ARGS tp spec args -> 
            pure $ CConstructor 
                (toRowCol tp) 
                (TypeAndSpec (toIdent tp) (toIdent spec)) 
                (map toIdent args)

        B.AC_CASE keyword expr labelcoms -> do
            labelcoms' <- traverse labelledSeqComs labelcoms
            return $ CCase 
                (toRowCol keyword)
                (toIdent expr)
                labelcoms'

        B.AC_IF keyword expr thenc elsec -> do
            thenc' <- mplAsmParseComs thenc
            elsec' <- mplAsmParseComs elsec
            return $ CIf 
                (toRowCol keyword) 
                (toIdent expr) 
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
                (TypeAndSpec (toIdent tp) (toIdent spec)) 
                [] 
                (toIdent expr)

        B.AC_DEST_ARGS tp spec args expr ->
            return $ CDestructor 
                (toRowCol tp) 
                (TypeAndSpec (toIdent tp) (toIdent spec)) 
                (map toIdent args) 
                (toIdent expr)

        B.AC_PROD args -> 
            -- TODO: welp 
            return $ CTuple (-1,-1) $ map toIdent args

        B.AC_PRODELEM projnum expr -> 
            return $ CProj 
                (toRowCol projnum) 
                (fromIntegral $ pIntToInt projnum)
                (toIdent expr)
    {-
    | B.AC_EMSG String
    -}

        B.AC_GET keyword patt ch ->
            pure $ CGet 
                (toRowCol keyword) 
                (toIdent patt)
                (toIdent ch)
            
        B.AC_PUT keyword expr ch ->
            pure $ CPut 
                (toRowCol keyword) 
                (toIdent expr)
                (toIdent ch)

        B.AC_HPUT keyword tp spec ch ->
            pure $ 
                CHPut
                (toRowCol keyword)
                (TypeAndSpec (toIdent tp) (toIdent spec))
                (toIdent ch)

        B.AC_HCASE keyword ch labelledcoms -> do
            labelledcoms' <- traverse f labelledcoms 
            return $ CHCase 
                (toRowCol keyword)
                (toIdent ch)
                labelledcoms'
          where
            f labelledcoms = case labelledcoms of
                B.AC_LABELLED_COMS_NO_ARGS tp spec coms -> do
                    coms' <- mplAsmParseComs coms
                    return (TypeAndSpec (toIdent tp) (toIdent spec), coms' )

                B.AC_LABELLED_COMS tp spec [] coms -> do
                    coms' <- mplAsmParseComs coms
                    return (TypeAndSpec (toIdent tp) (toIdent spec), coms' )

                B.AC_LABELLED_COMS tp spec _ coms -> do
                    tell [_BnfcParseError # "Illegal hcase expression"]
                    coms' <- mplAsmParseComs coms
                    return (TypeAndSpec (toIdent tp) (toIdent spec), coms' )
        
        B.AC_SPLIT keyword ch lch rch -> 
            return $ CSplit 
                (toRowCol keyword)
                (toIdent ch)
                (toIdent lch, toIdent rch)
        B.AC_FORK keyword ch ch0 withs0 coms0 ch1 withs1 coms1 -> do
            coms0' <- mplAsmParseComs coms0
            coms1' <- mplAsmParseComs coms1
            return $ CFork 
                (toRowCol keyword)
                (toIdent ch)
                ( (toIdent ch0, map toIdent withs0, coms0')
                , (toIdent ch1, map toIdent withs1, coms1')
                )

        B.AC_PLUG keyword plugs withs0 coms0 withs1 coms1 -> do
            coms0' <- mplAsmParseComs coms0
            coms1' <- mplAsmParseComs coms1

            return $ CPlug 
                (toRowCol keyword)
                (map toIdent plugs)
                ( (map toIdent withs0, coms0')
                , (map toIdent withs1, coms1')
                )
        B.AC_RUN keyword proc seqs ins outs -> 
            return $ CRun 
                (toRowCol keyword)
                (toIdent proc)
                ( map toIdent seqs
                , map toIdent ins
                , map toIdent outs)

        B.AC_ID ch0 keyword ch1 -> 
            return $ CId 
                (toRowCol keyword)
                (toIdent ch0, toIdent ch1)

        B.AC_RACE keyword phrases -> do
            phrases' <- traverse f phrases
            return $ CRace (toRowCol keyword) phrases'
          where
            f (B.AC_RACE_PHRASE ch coms) = do
                coms' <- mplAsmParseComs coms
                return (toIdent ch, coms')

        B.AC_CLOSE keyword ch -> 
            return $ CClose 
                (toRowCol keyword)
                (toIdent ch)
        B.AC_HALT keyword ch -> 
            return $ CHalt 
                (toRowCol keyword)
                (toIdent ch)

    labelledSeqComs labelledcoms = case labelledcoms of
        B.AC_LABELLED_COMS_NO_ARGS tp spec coms -> do
            coms' <- mplAsmParseComs coms
            return (TypeAndSpec (toIdent tp) (toIdent spec), [], coms' )
        B.AC_LABELLED_COMS tp spec args coms -> do
            coms' <- mplAsmParseComs coms
            return (TypeAndSpec (toIdent tp) (toIdent spec), map toIdent args, coms' )
        

