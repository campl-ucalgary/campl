module Language.SkelMPL where

-- Haskell module generated by the BNF converter

import Language.AbsMPL
import Language.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transUIdent :: UIdent -> Result
transUIdent x = case x of
  UIdent string -> failure x
transPIdent :: PIdent -> Result
transPIdent x = case x of
  PIdent string -> failure x
transUPIdent :: UPIdent -> Result
transUPIdent x = case x of
  UPIdent string -> failure x
transPInteger :: PInteger -> Result
transPInteger x = case x of
  PInteger string -> failure x
transPar :: Par -> Result
transPar x = case x of
  Par string -> failure x
transTensor :: Tensor -> Result
transTensor x = case x of
  Tensor string -> failure x
transLBracket :: LBracket -> Result
transLBracket x = case x of
  LBracket string -> failure x
transRBracket :: RBracket -> Result
transRBracket x = case x of
  RBracket string -> failure x
transLSquareBracket :: LSquareBracket -> Result
transLSquareBracket x = case x of
  LSquareBracket string -> failure x
transRSquareBracket :: RSquareBracket -> Result
transRSquareBracket x = case x of
  RSquareBracket string -> failure x
transNullPattern :: NullPattern -> Result
transNullPattern x = case x of
  NullPattern string -> failure x
transColon :: Colon -> Result
transColon x = case x of
  Colon string -> failure x
transInfixl1op :: Infixl1op -> Result
transInfixl1op x = case x of
  Infixl1op string -> failure x
transInfixl2op :: Infixl2op -> Result
transInfixl2op x = case x of
  Infixl2op string -> failure x
transInfixl3op :: Infixl3op -> Result
transInfixl3op x = case x of
  Infixl3op string -> failure x
transInfixl4op :: Infixl4op -> Result
transInfixl4op x = case x of
  Infixl4op string -> failure x
transInfixl5op :: Infixl5op -> Result
transInfixl5op x = case x of
  Infixl5op string -> failure x
transInfixl6op :: Infixl6op -> Result
transInfixl6op x = case x of
  Infixl6op string -> failure x
transInfixr7op :: Infixr7op -> Result
transInfixr7op x = case x of
  Infixr7op string -> failure x
transInfixl8op :: Infixl8op -> Result
transInfixl8op x = case x of
  Infixl8op string -> failure x
transClose :: Close -> Result
transClose x = case x of
  Close string -> failure x
transHalt :: Halt -> Result
transHalt x = case x of
  Halt string -> failure x
transGet :: Get -> Result
transGet x = case x of
  Get string -> failure x
transPut :: Put -> Result
transPut x = case x of
  Put string -> failure x
transHCase :: HCase -> Result
transHCase x = case x of
  HCase string -> failure x
transHPut :: HPut -> Result
transHPut x = case x of
  HPut string -> failure x
transSplit :: Split -> Result
transSplit x = case x of
  Split string -> failure x
transFork :: Fork -> Result
transFork x = case x of
  Fork string -> failure x
transChId :: ChId -> Result
transChId x = case x of
  ChId string -> failure x
transCase :: Case -> Result
transCase x = case x of
  Case string -> failure x
transMplProg :: MplProg -> Result
transMplProg x = case x of
  MPL_PROG mplstmts -> failure x
transMplStmt :: MplStmt -> Result
transMplStmt x = case x of
  MPL_DEFN_STMS_WHERE mpldefns mplstmts -> failure x
  MPL_DEFN_STMS mpldefns -> failure x
  MPL_STMT mpldefn -> failure x
transMplDefn :: MplDefn -> Result
transMplDefn x = case x of
  MPL_SEQUENTIAL_TYPE_DEFN sequentialtypedefn -> failure x
  MPL_CONCURRENT_TYPE_DEFN concurrenttypedefn -> failure x
  MPL_FUNCTION_DEFN functiondefn -> failure x
  MPL_PROCESS_DEFN processdefn -> failure x
  MPL_DEFNTEST -> failure x
transMplType :: MplType -> Result
transMplType x = case x of
  MPL_TYPE mpltype -> failure x
  PAR_TYPE mpltype1 par mpltype2 -> failure x
  TENSOR_TYPE mpltype1 tensor mpltype2 -> failure x
  GETPUT_TYPE uident lbracket mpltype1 mpltype2 rbracket -> failure x
  MPL_UIDENT_ARGS_TYPE uident lbracket mpltypes rbracket -> failure x
  MPL_UIDENT_NO_ARGS_TYPE uident -> failure x
  MPL_UNIT_TYPE lbracket rbracket -> failure x
  MPL_BRACKETED_TYPE lbracket mpltype rbracket -> failure x
  MPL_LIST_TYPE lsquarebracket mpltype rsquarebracket -> failure x
  MPL_TUPLE_TYPE lbracket mpltype tuplelisttypes rbracket -> failure x
  MPL_SEQ_ARROW_TYPE forallvarlists mpltypes mpltype -> failure x
  MPL_CONC_ARROW_TYPE forallvarlists mpltypes1 mpltypes2 mpltypes3 -> failure x
transTupleListType :: TupleListType -> Result
transTupleListType x = case x of
  TUPLE_LIST_TYPE mpltype -> failure x
transForallVarList :: ForallVarList -> Result
transForallVarList x = case x of
  MPL_SEQ_FUN_TYPE_FORALL_LIST uident -> failure x
transSequentialTypeDefn :: SequentialTypeDefn -> Result
transSequentialTypeDefn x = case x of
  DATA_DEFN seqtypeclausedefns -> failure x
  CODATA_DEFN seqtypeclausedefns -> failure x
transSeqTypeClauseDefn :: SeqTypeClauseDefn -> Result
transSeqTypeClauseDefn x = case x of
  SEQ_TYPE_CLAUSE mpltype1 mpltype2 seqtypephrasedefns -> failure x
transSeqTypePhraseDefn :: SeqTypePhraseDefn -> Result
transSeqTypePhraseDefn x = case x of
  SEQ_TYPE_PHRASE typehandlenames mpltypes mpltype -> failure x
transConcurrentTypeDefn :: ConcurrentTypeDefn -> Result
transConcurrentTypeDefn x = case x of
  PROTOCOL_DEFN concurrenttypeclausedefns -> failure x
  COPROTOCOL_DEFN concurrenttypeclausedefns -> failure x
transConcurrentTypeClauseDefn :: ConcurrentTypeClauseDefn -> Result
transConcurrentTypeClauseDefn x = case x of
  CONCURRENT_TYPE_CLAUSE mpltype1 mpltype2 concurrenttypephrasedefns -> failure x
transConcurrentTypePhraseDefn :: ConcurrentTypePhraseDefn -> Result
transConcurrentTypePhraseDefn x = case x of
  CONCURRENT_TYPE_PHRASE typehandlenames mpltype1 mpltype2 -> failure x
transTypeHandleName :: TypeHandleName -> Result
transTypeHandleName x = case x of
  TYPE_HANDLE_NAME uident -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EXPR expr -> failure x
  TYPED_EXPR expr mpltype -> failure x
  IF_EXPR expr1 expr2 expr3 -> failure x
  LET_EXPR letexprphrases expr -> failure x
  INFIXR0_EXPR expr1 colon expr2 -> failure x
  INFIXL1_EXPR expr1 infixlop expr2 -> failure x
  INFIXL2_EXPR expr1 infixlop expr2 -> failure x
  INFIXL3_EXPR expr1 infixlop expr2 -> failure x
  INFIXL4_EXPR expr1 infixlop expr2 -> failure x
  INFIXL5_EXPR expr1 infixlop expr2 -> failure x
  INFIXL6_EXPR expr1 infixlop expr2 -> failure x
  INFIXR7_EXPR expr1 infixrop expr2 -> failure x
  INFIXL8_EXPR expr1 infixlop expr2 -> failure x
  LIST_EXPR lsquarebracket exprs rsquarebracket -> failure x
  VAR_EXPR pident -> failure x
  INT_EXPR pinteger -> failure x
  STRING_EXPR string -> failure x
  CHAR_EXPR char -> failure x
  DOUBLE_EXPR double -> failure x
  UNIT_EXPR lbracket rbracket -> failure x
  FOLD_EXPR expr foldexprphrases -> failure x
  UNFOLD_EXPR expr unfoldexprphrases -> failure x
  CASE_EXPR case_ expr pattexprphrases -> failure x
  SWITCH_EXP switchexprphrases -> failure x
  DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR uident lbracket exprs rbracket -> failure x
  DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR uident -> failure x
  TUPLE_EXPR lbracket expr tupleexprlists rbracket -> failure x
  FUN_EXPR pident lbracket exprs rbracket -> failure x
  RECORD_EXPR lbracket recordexprphrases rbracket -> failure x
  BRACKETED_EXPR lbracket expr rbracket -> failure x
transUnfoldExprPhrase :: UnfoldExprPhrase -> Result
transUnfoldExprPhrase x = case x of
  UNFOLD_EXPR_PHRASE pattern foldexprphrases -> failure x
transFoldExprPhrase :: FoldExprPhrase -> Result
transFoldExprPhrase x = case x of
  FOLD_EXPR_PHRASE uident colon patterns expr -> failure x
transLetExprPhrase :: LetExprPhrase -> Result
transLetExprPhrase x = case x of
  LET_EXPR_PHRASE mplstmt -> failure x
transTupleExprList :: TupleExprList -> Result
transTupleExprList x = case x of
  TUPLE_EXPR_LIST expr -> failure x
transRecordExprPhrase :: RecordExprPhrase -> Result
transRecordExprPhrase x = case x of
  RECORD_EXPR_PHRASE uident expr -> failure x
  RECORD_EXPR_HIGHER_ORDER_PHRASE uident pattexprphrase -> failure x
transSwitchExprPhrase :: SwitchExprPhrase -> Result
transSwitchExprPhrase x = case x of
  SWITCH_EXPR_PHRASE expr1 expr2 -> failure x
transPattExprPhrase :: PattExprPhrase -> Result
transPattExprPhrase x = case x of
  PATTERN_TO_EXPR patterns expr -> failure x
transPattern :: Pattern -> Result
transPattern x = case x of
  PATTERN pattern -> failure x
  TYPED_PATTERN pattern mpltype -> failure x
  LIST_COLON_PATTERN pattern1 colon pattern2 -> failure x
  CONSTRUCTOR_PATTERN_ARGS uident lbracket patterns rbracket -> failure x
  CONSTRUCTOR_PATTERN_NO_ARGS uident -> failure x
  UNIT_PATTERN lbracket rbracket -> failure x
  RECORD_PATTERN lbracket destructorpatternphrases rbracket -> failure x
  LIST_PATTERN lsquarebracket patterns rsquarebracket -> failure x
  TUPLE_PATTERN lbracket pattern tuplelistpatterns rbracket -> failure x
  VAR_PATTERN pident -> failure x
  STR_PATTERN string -> failure x
  INT_PATTERN pinteger -> failure x
  NULL_PATTERN nullpattern -> failure x
  BRACKETED_PATTERN lbracket pattern rbracket -> failure x
transTupleListPattern :: TupleListPattern -> Result
transTupleListPattern x = case x of
  TUPLE_LIST_PATTERN pattern -> failure x
transDestructorPatternPhrase :: DestructorPatternPhrase -> Result
transDestructorPatternPhrase x = case x of
  DESTRUCTOR_PATTERN_PHRASE uident pattern -> failure x
transFunctionDefn :: FunctionDefn -> Result
transFunctionDefn x = case x of
  INTERNAL_TYPED_FUNCTION_DEFN pident mpltype pattexprphrases -> failure x
  TYPED_FUNCTION_DEFN pident mpltypes mpltype pattexprphrases -> failure x
  FUNCTION_DEFN pident pattexprphrases -> failure x
transProcessDefn :: ProcessDefn -> Result
transProcessDefn x = case x of
  TYPED_PROCESS_DEFN pident mpltypes1 mpltypes2 mpltypes3 processphrases -> failure x
  INTERNAL_TYPED_PROCESS_DEFN pident mpltype processphrases -> failure x
  PROCESS_DEFN pident processphrases -> failure x
transProcessPhrase :: ProcessPhrase -> Result
transProcessPhrase x = case x of
  PROCESS_PHRASE patterns pidents1 pidents2 processcommandsblock -> failure x
transProcessCommandsBlock :: ProcessCommandsBlock -> Result
transProcessCommandsBlock x = case x of
  PROCESS_COMMANDS_DO_BLOCK processcommands -> failure x
  PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK processcommand -> failure x
transProcessCommand :: ProcessCommand -> Result
transProcessCommand x = case x of
  PROCESS_RUN pident lbracket exprs pidents1 pidents2 rbracket -> failure x
  PROCESS_CLOSE close pident -> failure x
  PROCESS_HALT halt pident -> failure x
  PROCESS_GET get pattern pident -> failure x
  PROCESS_PUT put expr pident -> failure x
  PROCESS_HCASE hcase pident hcasephrases -> failure x
  PROCESS_HPUT hput uident pident -> failure x
  PROCESS_SPLIT split pident splitchannels -> failure x
  PROCESS_FORK fork pident forkphrases -> failure x
  PROCESS_ID pident1 chid pident2 -> failure x
  PROCESS_NEG pident1 chid pident2 -> failure x
  PROCESS_RACE racephrases -> failure x
  PROCESS_PLUG plugphrases -> failure x
  PROCESS_CASE case_ expr processcasephrases -> failure x
  PROCESS_SWITCH processswitchphrases -> failure x
transHCasePhrase :: HCasePhrase -> Result
transHCasePhrase x = case x of
  HCASE_PHRASE uident processcommandsblock -> failure x
transSplitChannel :: SplitChannel -> Result
transSplitChannel x = case x of
  SPLIT_CHANNEL pident -> failure x
transForkPhrase :: ForkPhrase -> Result
transForkPhrase x = case x of
  FORK_PHRASE pident processcommandsblock -> failure x
  FORK_WITH_PHRASE pident forkchannels processcommandsblock -> failure x
transForkChannel :: ForkChannel -> Result
transForkChannel x = case x of
  FORK_CHANNEL pident -> failure x
transRacePhrase :: RacePhrase -> Result
transRacePhrase x = case x of
  RACE_PHRASE pident processcommandsblock -> failure x
transPlugPhrase :: PlugPhrase -> Result
transPlugPhrase x = case x of
  PLUG_PHRASE processcommandsblock -> failure x
transProcessCasePhrase :: ProcessCasePhrase -> Result
transProcessCasePhrase x = case x of
  PROCESS_CASE_PHRASE pattern processcommandsblock -> failure x
transProcessSwitchPhrase :: ProcessSwitchPhrase -> Result
transProcessSwitchPhrase x = case x of
  PROCESS_SWITCH_PHRASE expr processcommandsblock -> failure x

