-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Language.AbsMPL where

newtype UIdent = UIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PInteger = PInteger ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Infixl1op = Infixl1op String
  deriving (Eq, Ord, Show, Read)

newtype Infixl2op = Infixl2op String
  deriving (Eq, Ord, Show, Read)

newtype Infixl3op = Infixl3op String
  deriving (Eq, Ord, Show, Read)

newtype Infixl4op = Infixl4op String
  deriving (Eq, Ord, Show, Read)

newtype Infixl5op = Infixl5op String
  deriving (Eq, Ord, Show, Read)

newtype Infixl6op = Infixl6op String
  deriving (Eq, Ord, Show, Read)

newtype Infixr7op = Infixr7op String
  deriving (Eq, Ord, Show, Read)

newtype Infixl8op = Infixl8op String
  deriving (Eq, Ord, Show, Read)

data MplProg = MPL_PROG [MplStmt]
  deriving (Eq, Ord, Show, Read)

data MplStmt
    = MPL_DEFN_STMS_WHERE [MplDefn] [MplStmt]
    | MPL_DEFN_STMS [MplDefn]
    | MPL_STMT MplDefn
  deriving (Eq, Ord, Show, Read)

data MplDefn
    = MPL_SEQUENTIAL_TYPE_DEFN SequentialTypeDefn
    | MPL_CONCURRENT_TYPE_DEFN ConcurrentTypeDefn
    | MPL_FUNCTION_DEFN FunctionDefn
    | MPL_PROCESS_DEFN ProcessDefn
    | MPLDEFNTEST
  deriving (Eq, Ord, Show, Read)

data MplType
    = MPL_TYPE MplType
    | PAR_TYPE MplType MplType
    | TENSOR_TYPE MplType MplType
    | GETPUT_TYPE UIdent MplType MplType
    | MPL_UIDENT_ARGS_TYPE UIdent [MplType]
    | MPL_UIDENT_NO_ARGS_TYPE UIdent
    | MPL_UNIT_TYPE
    | MPL_BRACKETED_TYPE MplType
    | MPL_LIST_TYPE MplType
    | MPL_TUPLE_TYPE MplType [TupleListType]
  deriving (Eq, Ord, Show, Read)

data TupleListType = TUPLE_LIST_TYPE MplType
  deriving (Eq, Ord, Show, Read)

data SequentialTypeDefn
    = DATA_DEFN [SeqTypeClauseDefn] | CODATA_DEFN [SeqTypeClauseDefn]
  deriving (Eq, Ord, Show, Read)

data SeqTypeClauseDefn
    = SEQ_TYPE_CLAUSE MplType MplType [SeqTypePhraseDefn]
  deriving (Eq, Ord, Show, Read)

data SeqTypePhraseDefn
    = SEQ_TYPE_PHRASE [TypeHandleName] [MplType] MplType
  deriving (Eq, Ord, Show, Read)

data ConcurrentTypeDefn
    = PROTOCOL_DEFN [ConcurrentTypeClauseDefn]
    | COPROTOCOL_DEFN [ConcurrentTypeClauseDefn]
  deriving (Eq, Ord, Show, Read)

data ConcurrentTypeClauseDefn
    = CONCURRENT_TYPE_CLAUSE MplType MplType [ConcurrentTypePhraseDefn]
  deriving (Eq, Ord, Show, Read)

data ConcurrentTypePhraseDefn
    = CONCURRENT_TYPE_PHRASE [TypeHandleName] MplType MplType
  deriving (Eq, Ord, Show, Read)

data TypeHandleName = TYPE_HANDLE_NAME UIdent
  deriving (Eq, Ord, Show, Read)

data Expr
    = EXPR Expr
    | IF_EXPR Expr Expr Expr
    | LET_EXPR [LetExprPhrase] Expr
    | INFIXR0_EXPR Expr Expr
    | INFIXL1_EXPR Expr Infixl1op Expr
    | INFIXL2_EXPR Expr Infixl2op Expr
    | INFIXL3_EXPR Expr Infixl3op Expr
    | INFIXL4_EXPR Expr Infixl4op Expr
    | INFIXL5_EXPR Expr Infixl5op Expr
    | INFIXL6_EXPR Expr Infixl6op Expr
    | INFIXR7_EXPR Expr Infixr7op Expr
    | INFIXL8_EXPR Expr Infixl8op Expr
    | LIST_EXPR [Expr]
    | VAR_EXPR PIdent
    | INT_EXPR PInteger
    | STRING_EXPR String
    | CHAR_EXPR Char
    | DOUBLE_EXPR Double
    | UNIT_EXPR
    | FOLD_EXPR Expr [FoldExprPhrase]
    | UNFOLD_EXPR Expr [UnfoldExprPhrase]
    | CASE_EXPR Expr [PattExprPhrase]
    | SWITCH_EXP [SwitchExprPhrase]
    | DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR UIdent [Expr]
    | DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR UIdent
    | TUPLE_EXPR Expr [TupleExprList]
    | FUN_EXPR PIdent [Expr]
    | RECORD_EXPR [RecordExprPhrase]
  deriving (Eq, Ord, Show, Read)

data UnfoldExprPhrase = UNFOLD_EXPR_PHRASE Expr [FoldExprPhrase]
  deriving (Eq, Ord, Show, Read)

data FoldExprPhrase = FOLD_EXPR_PHRASE UIdent [PIdent] Expr
  deriving (Eq, Ord, Show, Read)

data LetExprPhrase = LET_EXPR_PHRASE MplStmt
  deriving (Eq, Ord, Show, Read)

data TupleExprList = TUPLE_EXPR_LIST Expr
  deriving (Eq, Ord, Show, Read)

data RecordExprPhrase = RECORD_EXPR_PHRASE UIdent Expr
  deriving (Eq, Ord, Show, Read)

data SwitchExprPhrase = SWITCH_EXPR_PHRASE Expr Expr
  deriving (Eq, Ord, Show, Read)

data PattExprPhrase = PATTERN_TO_EXPR [Pattern] Expr
  deriving (Eq, Ord, Show, Read)

data Pattern
    = PATTERN Pattern
    | LIST_COLON_PATTERN Pattern Pattern
    | CONSTRUCTOR_PATTERN_ARGS UIdent [Pattern]
    | CONSTRUCTOR_PATTERN_NO_ARGS UIdent
    | UNIT_PATTERN
    | RECORD_PATTERN DestructorPatternPhrase [DestructorPatternPhrase]
    | LIST_PATTERN [Pattern]
    | TUPLE_PATTERN Pattern [TupleListPattern]
    | VAR_PATTERN PIdent
    | STR_PATTERN String
    | INT_PATTERN PInteger
    | NULL_PATTERN
  deriving (Eq, Ord, Show, Read)

data TupleListPattern = TUPLE_LIST_PATTERN Pattern
  deriving (Eq, Ord, Show, Read)

data DestructorPatternPhrase
    = DESTRUCTOR_PATTERN_PHRASE UIdent Pattern
  deriving (Eq, Ord, Show, Read)

data FunctionDefn
    = TYPED_FUNCTION_DEFN PIdent [MplType] MplType [PattExprPhrase]
    | FUNCTION_DEFN PIdent [PattExprPhrase]
  deriving (Eq, Ord, Show, Read)

data ProcessDefn
    = TYPED_PROCESS_DEFN PIdent [MplType] [MplType] [MplType] [ProcessPhrase]
    | PROCESS_DEFN PIdent [ProcessPhrase]
  deriving (Eq, Ord, Show, Read)

data ProcessPhrase
    = PROCESS_PHRASE [Pattern] [Pattern] [Pattern] ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ProcessCommandsBlock
    = PROCESS_COMMANDS_DO_BLOCK [ProcessCommand]
    | PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK ProcessCommand
  deriving (Eq, Ord, Show, Read)

data ProcessCommand
    = PROCESS_RUN PIdent [Expr] [PIdent] [PIdent]
    | PROCESS_CLOSE PIdent
    | PROCESS_HALT PIdent
    | PROCESS_GET Pattern PIdent
    | PROCESS_PUT Expr PIdent
    | PROCESS_HCASE PIdent [HCasePhrase]
    | PROCESS_HPUT UIdent PIdent
    | PROCESS_SPLIT PIdent [PIdent]
    | PROCESS_FORK PIdent [ForkPhrase]
    | PROCESS_ID PIdent PIdent
    | PROCESS_NEG PIdent PIdent
    | PROCESS_RACE [RacePhrase]
    | PROCESS_PLUG [PlugPhrase]
    | PROCESS_CASE Expr [ProcessCasePhrase]
    | PROCESS_SWITCH [ProcessSwitchPhrase]
  deriving (Eq, Ord, Show, Read)

data HCasePhrase = HCASE_PHRASE UIdent ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ForkPhrase
    = FORK_WITH_PHRASE PIdent [PIdent] ProcessCommandsBlock
    | FORK_PHRASE PIdent ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data RacePhrase = RACE_PHRASE PIdent ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data PlugPhrase = PLUG_PHRASE ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ProcessCasePhrase
    = PROCESS_CASE_PHRASE [Pattern] ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ProcessSwitchPhrase
    = PROCESS_SWITCH_PHRASE Expr ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

