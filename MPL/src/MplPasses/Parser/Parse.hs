{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module MplPasses.Parser.Parse where

import qualified MplPasses.Parser.BnfcParse as B

import Optics 
import MplAST.MplCore
import MplAST.MplParsed
import MplPasses.Parser.ParseErrors 
import MplPasses.Parser.ParseUtils 
import MplPasses.Parser.ParseMplType 
import MplPasses.Parser.ParseMplPattern 

import Debug.Trace
import Control.Monad.Writer
import Control.Monad.Except
import Data.Maybe

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Control.Arrow

import Data.Coerce

{- Module for running the parser -}

-- | Runs the Parser for an Mpl program (meant to be the interface function called
-- externally.. all other functions are meant for internal use only...)
runParse' :: 
    ( AsParseErrors err ) =>
    B.MplProg -> 
    Either [err] (MplProg MplParsed)
runParse' = 
    \case (Left _, w) -> Left w
          (Right prg, w) -> Right prg
    . runWriter 
    . runExceptT 
    . runParse

-- | Parses an Mpl program
runParse :: BnfcParse B.MplProg (MplProg MplParsed)
runParse (B.MPL_PROG prog) = do
    MplProg <$> traverseTryEach parseBnfcStmt prog 

-- | Parses a statement
parseBnfcStmt :: BnfcParse B.MplStmt (MplStmt MplParsed)
parseBnfcStmt (B.MPL_DEFN_STMS_WHERE defs wheres) = do
    MplStmt 
        <$> traverseTryEach parseBnfcDefn (NE.fromList defs) 
        <*> traverseTryEach (\case (B.MPL_WHERE stmt) -> parseBnfcStmt stmt) wheres
parseBnfcStmt (B.MPL_DEFN_STMS defs) = 
    parseBnfcStmt (B.MPL_DEFN_STMS_WHERE defs [])
parseBnfcStmt (B.MPL_STMT def) = 
    parseBnfcStmt (B.MPL_DEFN_STMS_WHERE [def] [])

-- | Parses a definition
parseBnfcDefn :: BnfcParse B.MplDefn (MplDefn MplParsed)
parseBnfcDefn (B.MPL_SEQUENTIAL_TYPE_DEFN (B.DATA_DEFN clauses)) =  
    review (_SeqObjDefn % _DataDefn) . UMplTypeClauseSpine . NE.fromList 
        <$> traverseTryEach f clauses
 where
    f (B.SEQ_TYPE_CLAUSE from to handles) = do
        ((name, args), st) <- parseTypeWithArgsSeqAndStateVar from to
        handles' <- traverseTryEach g handles
        return $ _MplTypeClause # (name, args, st, concat handles', ())

    g (B.SEQ_TYPE_PHRASE handles fromtypes totype) = do
        fromtypes' <- traverseTryEach parseBnfcType fromtypes 
        totype' <- parseTypeVariable <=< parseBnfcType $ totype
        return $ map 
            ( review _MplTypePhrase 
            . (,fromtypes', totype',()) 
            . toTermIdentP
            ) handles
parseBnfcDefn (B.MPL_SEQUENTIAL_TYPE_DEFN (B.CODATA_DEFN clauses)) =  
    review (_SeqObjDefn % _CodataDefn) . UMplTypeClauseSpine . NE.fromList 
        <$> traverseTryEach f clauses
 where
    f (B.SEQ_TYPE_CLAUSE from to handles) = do
        ((name, args), st) <- parseStateVarAndTypeWithArgsSeq from to
        handles' <- traverseTryEach g handles
        return $ _MplTypeClause # (name, args, st, concat handles', ())

    g (B.SEQ_TYPE_PHRASE handles fromtypes totype) = do
        fromtypes' <- traverseTryEach parseBnfcType (init fromtypes)
        fromtypesst' <- parseTypeVariable <=< parseBnfcType $ last fromtypes
        totype' <- parseBnfcType totype 
        if null fromtypes
            then tell [_ExpectedCodataPhraseToHaveFromArgsButHasNone # map toTermIdentP handles ]  >> throwError ()
            else return $ map 
                    ( review _MplTypePhrase 
                    . (,(fromtypes', fromtypesst'), totype',()) 
                    . toTermIdentP
                    ) handles

parseBnfcDefn (B.MPL_CONCURRENT_TYPE_DEFN (B.PROTOCOL_DEFN clauses)) =  
    review (_ConcObjDefn % _ProtocolDefn) . UMplTypeClauseSpine . NE.fromList 
        <$> traverseTryEach f clauses
 where
    f (B.CONCURRENT_TYPE_CLAUSE from to handles) = do
        ((name, args), st) <- parseTypeWithArgsConcAndStateVar from to
        handles' <- traverseTryEach g handles
        return $ _MplTypeClause # (name, args, st, concat handles', ())

    g (B.CONCURRENT_TYPE_PHRASE handles fromtype totype) = do
        fromtype' <- parseBnfcType fromtype
        totype' <- parseTypeVariable <=< parseBnfcType $ totype
        return $ map 
                ( review _MplTypePhrase 
                . (,fromtype', totype',()) 
                . toChIdentP
                ) handles

-- duplicated code 
parseBnfcDefn (B.MPL_CONCURRENT_TYPE_DEFN (B.COPROTOCOL_DEFN clauses)) =  
    review (_ConcObjDefn % _CoprotocolDefn) . UMplTypeClauseSpine . NE.fromList 
        <$> traverseTryEach f clauses
 where
    f (B.CONCURRENT_TYPE_CLAUSE from to handles) = do
        -- ((name, args), st) <- parseTypeWithArgsConcAndStateVar from to
        ((name, args), st) <- parseStateVarAndTypeWithArgsConc from to
        handles' <- traverseTryEach g handles
        return $ _MplTypeClause # (name, args, st, concat handles', ())

    g (B.CONCURRENT_TYPE_PHRASE handles fromtype totype) = do
        fromtype' <- parseTypeVariable <=< parseBnfcType $ fromtype
        totype' <- parseBnfcType totype 
        return $ map 
                ( review _MplTypePhrase 
                . (,fromtype', totype',()) 
                . toChIdentP
                ) handles

parseBnfcDefn (B.MPL_FUNCTION_DEFN fun) =  
    FunctionDefn <$> parseBnfcFunction fun
parseBnfcDefn (B.MPL_PROCESS_DEFN proc) =  
    ProcessDefn <$> parseBnfcProcess proc
parseBnfcDefn B.MPL_DEFNTEST = error "hehe a little easter egg from the developer -- no writing potato as they're better eaten than thought about ;)"


-- FUNCTION PARSING
---------------------------------
parseBnfcFunction :: BnfcParse B.FunctionDefn (MplFunction MplParsed)
parseBnfcFunction (B.INTERNAL_TYPED_FUNCTION_DEFN _ _ _) =
    error "Bnfc error" 
parseBnfcFunction (B.TYPED_FUNCTION_DEFN ident froms to pattsexprs) = do
    froms' <- traverseTryEach parseBnfcType froms
    to' <- parseBnfcType to
    pattsexprs' <- traverseTryEach parseBnfcPattsExpr pattsexprs
    return $ MplFunction (toTermIdentP ident) (Just (froms',to')) (NE.fromList pattsexprs')
parseBnfcFunction (B.FUNCTION_DEFN ident pattsexprs) = do
    pattsexprs' <- traverseTryEach parseBnfcPattsExpr pattsexprs
    return $ MplFunction (toTermIdentP ident) Nothing (NE.fromList pattsexprs')

parseBnfcPattsExpr ::   
    BnfcParse B.PattExprPhrase ([MplPattern MplParsed], MplExpr MplParsed)
parseBnfcPattsExpr (B.PATTERN_TO_EXPR patts expr) = do
    patts' <- traverseTryEach parseBnfcPattern patts
    expr' <- parseBnfcExpr expr
    return (patts', expr')

parseBnfcExpr :: BnfcParse B.Expr (MplExpr MplParsed)
parseBnfcExpr (B.EXPR expr) = parseBnfcExpr expr
parseBnfcExpr (B.IF_EXPR iif ithen ielse) = do
    ~[iif', ithen', ielse'] <- traverseTryEach parseBnfcExpr [iif, ithen, ielse]
    return $ _EIf # ((), iif', ithen', ielse')
parseBnfcExpr (B.LET_EXPR stmts expr) = do
    stmts' <- traverseTryEach f stmts
    expr' <- parseBnfcExpr expr
    return $ _ELet # ((), NE.fromList stmts', expr')
  where
    f (B.LET_EXPR_PHRASE n) = parseBnfcStmt n

-- TODO
parseBnfcExpr (B.INFIXR0_EXPR a colon b) = error "not implemented instr"
parseBnfcExpr (B.INFIXL1_EXPR a op b) = error "not implemented instr"
parseBnfcExpr (B.INFIXL2_EXPR a op b) = error "not implemented instr"
parseBnfcExpr (B.INFIXL3_EXPR a op b) = error "not implemented instr"
parseBnfcExpr (B.INFIXL4_EXPR a op b) = error "not implemented instr"
parseBnfcExpr (B.INFIXL5_EXPR a (B.Infixl5op (pos, op)) b) = do
    ~[a', b']<- traverseTryEach parseBnfcExpr [a,b]
    case op of
        "+" -> return $ _EPOps #  ((), PrimitiveAdd, a',b') 
        "-" -> return $ _EPOps #  ((), PrimitiveSub, a',b') 
        _ -> error $ "not implemented: " ++ op
parseBnfcExpr (B.INFIXL6_EXPR a (B.Infixl6op (pos,op)) b) = do
    ~[a', b']<- traverseTryEach parseBnfcExpr [a,b]
    case op of
        "*" -> return $ _EPOps #  ((), PrimitiveMul, a',b') 
        "/" -> return $ _EPOps #  ((), PrimitiveDiv, a',b') 
        _ -> error $ "not implemented: " ++ op

parseBnfcExpr (B.INFIXR7_EXPR a op b) = error "not implemented instr"
parseBnfcExpr (B.INFIXL8_EXPR a op b) = error "not implemented instr"

parseBnfcExpr (B.VAR_EXPR v) = return $ review _EVar ((), toTermIdentP v)
parseBnfcExpr (B.INT_EXPR v) = 
    case pIntegerToLocationInt v of
        Just n -> return $ _EInt # n
        Nothing -> tell [_InvalidInt # toTermIdentP v] >> throwError ()
parseBnfcExpr (B.DOUBLE_EXPR v) = 
    case pDoubleToLocationDouble v of 
        Just n -> return $ _EDouble # n
        Nothing -> tell [_InvalidDouble # toTermIdentP v] >> throwError ()
        
parseBnfcExpr (B.CHAR_EXPR v) = 
    case pCharToLocationChar v of
        Just n -> return $ _EChar # n
        Nothing -> tell [_InvalidChar # toTermIdentP v] >> throwError ()

parseBnfcExpr (B.LIST_EXPR lbr exprs rbr) = do
    exprs' <- traverse parseBnfcExpr exprs
    return $ _EList # (toLocation lbr, exprs')
parseBnfcExpr (B.STRING_EXPR (B.PString (loc, str))) = 
    return $ _EString # (toLocation loc, init $ tail str)

parseBnfcExpr (B.UNIT_EXPR lbr rbr) = 
    return $ _EUnit # toLocation lbr 

parseBnfcExpr (B.FOLD_EXPR expr phrases) = do
    expr' <- parseBnfcExpr expr
    phrases' <- traverseTryEach parseBnfcFoldPhrase phrases
    return $  _EFold # ((), expr', NE.fromList phrases')
parseBnfcExpr (B.UNFOLD_EXPR expr phrases) = do
    expr' <- parseBnfcExpr expr
    phrases' <- traverseTryEach parseBnfcUnfoldPhrase phrases
    return $  _EUnfold # ((), expr', NE.fromList phrases')

parseBnfcExpr (B.CASE_EXPR cxt expr pattsexprs) = do
    expr' <- parseBnfcExpr expr
    pattexprs' <- traverseTryEach (f <=< parseBnfcPattsExpr) pattsexprs
    return $ _ECase # ((), expr', NE.fromList pattexprs')
  where
    f ([patt], expr) = return (patt, expr)
    f (n, expr) = tell [_CaseExpectedExactlyOnePatternButGot # n] >> throwError ()

parseBnfcExpr (B.SWITCH_EXP switches) = do
    switches' <- traverseTryEach f switches
    return $ _ESwitch # ((), NE.fromList switches')
  where
    f (B.SWITCH_EXPR_PHRASE a b) = do
        ~[a',b'] <- traverseTryEach parseBnfcExpr [a,b]
        return (a',b')
parseBnfcExpr (B.DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR ident) =
    return $ _EObjCall # ((), toTermIdentP ident, [] ) 
parseBnfcExpr (B.DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR ident _ exprs _) = do
    exprs' <- traverseTryEach parseBnfcExpr exprs
    return $ _EObjCall # ((), toTermIdentP ident, exprs') 

parseBnfcExpr (B.TUPLE_EXPR lbr t0 (t1:ts) rbr) = do
    ~(t0':t1':ts') <- traverseTryEach f (B.TUPLE_EXPR_LIST t0 : t1 :ts)
    return $ _ETuple # (toSpanLocation lbr rbr, (t0', t1', ts'))
  where
    f (B.TUPLE_EXPR_LIST e) = parseBnfcExpr e
parseBnfcExpr (B.TUPLE_EXPR lbr t0 _ rbr) =
    error "bnfc error -- bad tuple"

parseBnfcExpr (B.FUN_EXPR fun lbr args rbr) = do
    args' <- traverseTryEach parseBnfcExpr args
    return $ _ECall # ((), toTermIdentP fun, args')
parseBnfcExpr (B.RECORD_EXPR lbr phrases rbr) = do
    phrases' <- traverseTryEach f phrases
    return $ _ERecord # (toSpanLocation lbr rbr, NE.fromList phrases')
  where
    f (B.RECORD_EXPR_PHRASE ident expr) = 
        f (B.RECORD_EXPR_HIGHER_ORDER_PHRASE ident (B.PATTERN_TO_EXPR [] expr))
    f (B.RECORD_EXPR_HIGHER_ORDER_PHRASE ident pattsexpr) = do
        pattsexpr' <- parseBnfcPattsExpr pattsexpr
        return ((), toTermIdentP ident, pattsexpr')
parseBnfcExpr (B.BRACKETED_EXPR _ expr _) =  parseBnfcExpr expr
parseBnfcExpr (B.TYPED_EXPR _ _) =  
    error "cannot happen -- bnfc does not parse this TYPED_EXPR" 

parseBnfcFoldPhrase (B.FOLD_EXPR_PHRASE ident colon patts expr) = do
    patts' <- traverseTryEach parseBnfcPattern patts
    expr' <- parseBnfcExpr expr
    return $ ((), toTermIdentP ident, patts', expr')

parseBnfcUnfoldPhrase (B.UNFOLD_EXPR_PHRASE patt foldphrases) = do
    patt' <- parseBnfcPattern patt
    foldphrases' <- traverseTryEach parseBnfcFoldPhrase foldphrases
    return $ ((), patt', NE.fromList foldphrases')

---------------------------------
-- PROCESS PARSING
---------------------------------
parseBnfcProcess :: BnfcParse B.ProcessDefn (MplProcess MplParsed)
parseBnfcProcess (B.PROCESS_DEFN ident phrases) = do
    phrases' <- traverseTryEach parseBnfcProcessPhrase phrases
    return $ MplProcess (toChIdentP ident) Nothing $ NE.fromList phrases'
parseBnfcProcess (B.INTERNAL_TYPED_PROCESS_DEFN _ _ _) = error "bnfc does not parse INTERNAL_TYPED_PROCESS_DEFN"
parseBnfcProcess (B.TYPED_PROCESS_DEFN ident seqtype intype outtype phrases) = do
    ~[seqtype', intype', outtype'] <- traverseTryEach (traverseTryEach parseBnfcType) [seqtype, intype, outtype]
    phrases' <- traverseTryEach parseBnfcProcessPhrase phrases
    return $ MplProcess (toChIdentP ident) (Just (seqtype', intype', outtype')) $ NE.fromList phrases'

parseBnfcProcessPhrase :: BnfcParse 
    B.ProcessPhrase 
    (([MplPattern MplParsed], [ChP MplParsed], [ChP MplParsed]), NonEmpty (MplCmd MplParsed))
parseBnfcProcessPhrase (B.PROCESS_PHRASE patts inchs outchs cmdsblock) = do
    patts' <- traverseTryEach parseBnfcPattern patts
    cmds <- parseBnfcCmdBlock cmdsblock
    return $ ((patts', map toChIdentP inchs, map toChIdentP outchs), cmds)

parseBnfcCmdBlock ::
    BnfcParse
    B.ProcessCommandsBlock
    (NonEmpty (MplCmd MplParsed))
parseBnfcCmdBlock (B.PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK cmd) =
    parseBnfcCmdBlock (B.PROCESS_COMMANDS_DO_BLOCK [cmd])
parseBnfcCmdBlock (B.PROCESS_COMMANDS_DO_BLOCK cmds) =
    NE.fromList <$> traverseTryEach parseBnfcCmd cmds

parseBnfcCmd :: 
    BnfcParse 
    B.ProcessCommand
    (MplCmd MplParsed)
parseBnfcCmd (B.PROCESS_RUN ident _ seqs inchs outchs _) = do
    seqs' <- traverseTryEach parseBnfcExpr seqs
    return $ _CRun # 
        ( ()
        , toChIdentP ident
        , seqs'
        , map toChIdentP inchs
        , map toChIdentP outchs
        )
parseBnfcCmd (B.PROCESS_CLOSE cxt ident) = 
    return $ _CClose # (coerce $ toNameOcc cxt, toChIdentP ident)
parseBnfcCmd (B.PROCESS_HALT cxt ident) = 
    return $ _CHalt # (coerce $ toNameOcc cxt, toChIdentP ident)

parseBnfcCmd (B.PROCESS_GET cxt patt ident) = do
    patt' <- parseBnfcPattern patt
    return $ _CGet # (coerce $ toNameOcc cxt, patt' , toChIdentP ident)
parseBnfcCmd (B.PROCESS_PUT cxt expr ident) = do
    expr' <- parseBnfcExpr expr
    return $ _CPut # (coerce $ toNameOcc cxt, expr', toChIdentP ident)
parseBnfcCmd (B.PROCESS_HCASE cxt ident phrases) = do
    phrases' <- traverseTryEach f phrases
    return $ _CHCase # (coerce $ toNameOcc cxt, toChIdentP ident, NE.fromList phrases')
  where
    f (B.HCASE_PHRASE uident cmdblk) = do
        cmds <- parseBnfcCmdBlock cmdblk
        return $ ((), toChIdentP uident, cmds)
parseBnfcCmd (B.PROCESS_HPUT cxt s t) = do
    return $ _CHPut # (coerce $ toNameOcc cxt, toChIdentP s, toChIdentP t)

parseBnfcCmd (B.PROCESS_SPLIT cxt s chs) = do
    let chs' = map f chs in case chs' of
        [p,q] -> return $ _CSplit # (coerce $ toNameOcc cxt, toChIdentP s, (p, q))
        _ -> tell [_SplitExpectedExactlyTwoChannelsButGot # chs'] >> throwError ()
  where
    f (B.SPLIT_CHANNEL ch) = toChIdentP ch

parseBnfcCmd (B.PROCESS_FORK cxt ch phrases) = do
    phrases' <- traverseTryEach f phrases
    case phrases' of
        [p,q] -> return $ _CFork # (coerce $ toNameOcc cxt, toChIdentP ch, (p,q))
        _ -> tell [_ForkExpectedExactlyTwoForkedChannelsButGot # phrases'] >> throwError ()
  where
    f (B.FORK_PHRASE ch cmds) = do
        cmds' <- parseBnfcCmdBlock cmds
        return $ (toChIdentP ch, Nothing, cmds')
    f (B.FORK_WITH_PHRASE ch cxt cmds) = do
        cmds' <- parseBnfcCmdBlock cmds
        return $ (toChIdentP ch, Just (map toChIdentP cxt), cmds')

parseBnfcCmd (B.PROCESS_ID a cxt b) =
    return $ _CId # (coerce $ toNameOcc cxt, (toChIdentP a, toChIdentP b))
parseBnfcCmd (B.PROCESS_NEG a cxt b) =
    return $ _CIdNeg # (coerce $ toNameOcc cxt, (toChIdentP a, toChIdentP b))

parseBnfcCmd (B.PROCESS_RACE races) = do
    races' <- traverseTryEach f races
    return $ _CRace # 
        (  buggedKeywordNameOcc "race"
        , NE.fromList races'
        )
  where
    f (B.RACE_PHRASE ch cmds) = do
        cmds' <- parseBnfcCmdBlock cmds
        return $ (toChIdentP ch, cmds')

parseBnfcCmd (B.PROCESS_PLUG phrases) = do
    phrases' <- traverseTryEach f phrases
    case phrases' of
        (a:b:cs) -> return $ _CPlugs # 
            ( ( buggedKeywordNameOcc "plug"
              , Nothing
                -- Note: as of now, it is impossible for a user to supply 
                -- their own context. This is a limitation of bnfc that a layout
                -- word cannot be used in multiple ways. Indeed, we can bypass
                -- this by adding a keyword after plug, and using the new keyword
                -- as the layout word instead of plug.
                --
                -- TODO: currently, with the grammar given, it is impossible to explictly provide channels to
                -- be plugged against, but this system should support this in the future.
                )
            , (a, b, cs))
        as -> tell [_PlugExpectedTwoOrMorePhrasesButGot # listToMaybe as] >> throwError ()
        -- <=1
  where
    -- TODO
    -- f :: B.PlugPhrase -> m (XCPlugPhrase x, ([ChP x], [ChP x]), NonEmpty (MplCmd x))
    f (B.PLUG_PHRASE (B.PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK (B.PROCESS_RUN ident _ seqs inchs outchs _))) = do
        seqs' <- traverseTryEach parseBnfcExpr seqs
        let inchs' = map toChIdentP inchs
            outchs' = map toChIdentP outchs
            cmd = _CRun # (() , toChIdentP ident , seqs' , inchs' , outchs') 
        return ((), (inchs', outchs'), cmd :| [])
    f (B.PLUG_PHRASE cmds) = do
        cmds' <- parseBnfcCmdBlock cmds
        tell $ [_PlugExpectedARunProcessCallButGot # cmds']
        throwError ()
    f (B.PLUG_PHRASE_AS ins outs cmds) = do
        cmds' <- parseBnfcCmdBlock cmds
        return ((), (map toChIdentP ins, map toChIdentP outs), cmds')

parseBnfcCmd (B.PROCESS_CASE cxt expr pcases) = do
    expr' <- parseBnfcExpr expr
    pcases' <- traverseTryEach f pcases
    return $ _CCase # (coerce $ toNameOcc cxt, expr', NE.fromList pcases')
  where
    f (B.PROCESS_CASE_PHRASE patt cmds) = do
        patt' <- parseBnfcPattern patt
        cmds' <- parseBnfcCmdBlock cmds
        return (patt', cmds')

parseBnfcCmd (B.PROCESS_SWITCH pswitch) = do
    pswitch' <- traverseTryEach f pswitch
    return $ _CSwitch # 
        ( buggedKeywordNameOcc "switch"
        , NE.fromList pswitch' )
  where
    f (B.PROCESS_SWITCH_PHRASE expr cmds) = do
        expr' <- parseBnfcExpr expr
        cmds' <- parseBnfcCmdBlock cmds
        return (expr', cmds')

buggedKeywordNameOcc :: String -> KeyWordNameOcc
buggedKeywordNameOcc str = KeyWordNameOcc $ toNameOcc (trace "TODO: Fix location info.." $ (-1 :: Int ,-1 :: Int), str)
