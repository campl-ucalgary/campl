

module Language.AbsMPL where

-- Haskell module generated by the BNF converter




newtype UIdent = UIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype UPIdent = UPIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PInteger = PInteger ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Par = Par ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype Tensor = Tensor ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype LBracket = LBracket ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype RBracket = RBracket ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype LSquareBracket = LSquareBracket ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype RSquareBracket = RSquareBracket ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype NullPattern = NullPattern ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Colon = Colon ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype Infixl1op = Infixl1op String deriving (Eq, Ord, Show, Read)
newtype Infixl2op = Infixl2op String deriving (Eq, Ord, Show, Read)
newtype Infixl3op = Infixl3op String deriving (Eq, Ord, Show, Read)
newtype Infixl4op = Infixl4op String deriving (Eq, Ord, Show, Read)
newtype Infixl5op = Infixl5op String deriving (Eq, Ord, Show, Read)
newtype Infixl6op = Infixl6op String deriving (Eq, Ord, Show, Read)
newtype Infixr7op = Infixr7op String deriving (Eq, Ord, Show, Read)
newtype Infixl8op = Infixl8op String deriving (Eq, Ord, Show, Read)
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
    | MPL_DEFNTEST
  deriving (Eq, Ord, Show, Read)

data MplType
    = MPL_TYPE MplType
    | PAR_TYPE MplType Par MplType
    | TENSOR_TYPE MplType Tensor MplType
    | GETPUT_TYPE UIdent LBracket MplType MplType RBracket
    | MPL_UIDENT_ARGS_TYPE UIdent LBracket [MplType] RBracket
    | MPL_UIDENT_NO_ARGS_TYPE UIdent
    | MPL_UNIT_TYPE LBracket RBracket
    | MPL_BRACKETED_TYPE LBracket MplType RBracket
    | MPL_LIST_TYPE LSquareBracket MplType RSquareBracket
    | MPL_TUPLE_TYPE LBracket MplType [TupleListType] RBracket
    | MPL_SEQ_ARROW_TYPE [ForallVarList] [MplType] MplType
    | MPL_CONC_ARROW_TYPE [ForallVarList] [MplType] [MplType] [MplType]
  deriving (Eq, Ord, Show, Read)

data TupleListType = TUPLE_LIST_TYPE MplType
  deriving (Eq, Ord, Show, Read)

data ForallVarList = MPL_SEQ_FUN_TYPE_FORALL_LIST UIdent
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
    | TYPED_EXPR Expr MplType
    | IF_EXPR Expr Expr Expr
    | LET_EXPR [LetExprPhrase] Expr
    | INFIXR0_EXPR Expr Colon Expr
    | INFIXL1_EXPR Expr Infixl1op Expr
    | INFIXL2_EXPR Expr Infixl2op Expr
    | INFIXL3_EXPR Expr Infixl3op Expr
    | INFIXL4_EXPR Expr Infixl4op Expr
    | INFIXL5_EXPR Expr Infixl5op Expr
    | INFIXL6_EXPR Expr Infixl6op Expr
    | INFIXR7_EXPR Expr Infixr7op Expr
    | INFIXL8_EXPR Expr Infixl8op Expr
    | LIST_EXPR LSquareBracket [Expr] RSquareBracket
    | VAR_EXPR PIdent
    | INT_EXPR PInteger
    | STRING_EXPR String
    | CHAR_EXPR Char
    | DOUBLE_EXPR Double
    | UNIT_EXPR LBracket RBracket
    | FOLD_EXPR Expr [FoldExprPhrase]
    | UNFOLD_EXPR Expr [UnfoldExprPhrase]
    | CASE_EXPR Expr [PattExprPhrase]
    | SWITCH_EXP [SwitchExprPhrase]
    | DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR UIdent LBracket [Expr] RBracket
    | DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR UIdent
    | TUPLE_EXPR LBracket Expr [TupleExprList] RBracket
    | FUN_EXPR PIdent LBracket [Expr] RBracket
    | RECORD_EXPR LBracket [RecordExprPhrase] RBracket
    | BRACKETED_EXPR LBracket Expr RBracket
  deriving (Eq, Ord, Show, Read)

data UnfoldExprPhrase = UNFOLD_EXPR_PHRASE Pattern [FoldExprPhrase]
  deriving (Eq, Ord, Show, Read)

data FoldExprPhrase = FOLD_EXPR_PHRASE UIdent Colon [Pattern] Expr
  deriving (Eq, Ord, Show, Read)

data LetExprPhrase = LET_EXPR_PHRASE MplStmt
  deriving (Eq, Ord, Show, Read)

data TupleExprList = TUPLE_EXPR_LIST Expr
  deriving (Eq, Ord, Show, Read)

data RecordExprPhrase
    = RECORD_EXPR_PHRASE UIdent Expr
    | RECORD_EXPR_HIGHER_ORDER_PHRASE UIdent PattExprPhrase
  deriving (Eq, Ord, Show, Read)

data SwitchExprPhrase = SWITCH_EXPR_PHRASE Expr Expr
  deriving (Eq, Ord, Show, Read)

data PattExprPhrase = PATTERN_TO_EXPR [Pattern] Expr
  deriving (Eq, Ord, Show, Read)

data Pattern
    = PATTERN Pattern
    | TYPED_PATTERN Pattern MplType
    | LIST_COLON_PATTERN Pattern Colon Pattern
    | CONSTRUCTOR_PATTERN_ARGS UIdent LBracket [Pattern] RBracket
    | CONSTRUCTOR_PATTERN_NO_ARGS UIdent
    | UNIT_PATTERN LBracket RBracket
    | RECORD_PATTERN LBracket [DestructorPatternPhrase] RBracket
    | LIST_PATTERN LSquareBracket [Pattern] RSquareBracket
    | TUPLE_PATTERN LBracket Pattern [TupleListPattern] RBracket
    | VAR_PATTERN PIdent
    | STR_PATTERN String
    | INT_PATTERN PInteger
    | NULL_PATTERN NullPattern
    | BRACKETED_PATTERN LBracket Pattern RBracket
  deriving (Eq, Ord, Show, Read)

data TupleListPattern = TUPLE_LIST_PATTERN Pattern
  deriving (Eq, Ord, Show, Read)

data DestructorPatternPhrase
    = DESTRUCTOR_PATTERN_PHRASE UIdent Pattern
  deriving (Eq, Ord, Show, Read)

data FunctionDefn
    = INTERNAL_TYPED_FUNCTION_DEFN PIdent MplType [PattExprPhrase]
    | TYPED_FUNCTION_DEFN PIdent [MplType] MplType [PattExprPhrase]
    | FUNCTION_DEFN PIdent [PattExprPhrase]
  deriving (Eq, Ord, Show, Read)

data ProcessDefn
    = TYPED_PROCESS_DEFN PIdent [MplType] [MplType] [MplType] [ProcessPhrase]
    | INTERNAL_TYPED_PROCESS_DEFN PIdent MplType [ProcessPhrase]
    | PROCESS_DEFN PIdent [ProcessPhrase]
  deriving (Eq, Ord, Show, Read)

data ProcessPhrase
    = PROCESS_PHRASE [Pattern] [PIdent] [PIdent] ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ProcessCommandsBlock
    = PROCESS_COMMANDS_DO_BLOCK [ProcessCommand]
    | PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK ProcessCommand
  deriving (Eq, Ord, Show, Read)

data ProcessCommand
    = PROCESS_RUN PIdent LBracket [Expr] [PIdent] [PIdent] RBracket
    | PROCESS_CLOSE PIdent
    | PROCESS_HALT PIdent
    | PROCESS_GET Pattern PIdent
    | PROCESS_PUT Expr PIdent
    | PROCESS_HCASE PIdent [HCasePhrase]
    | PROCESS_HPUT UIdent PIdent
    | PROCESS_SPLIT PIdent [SplitChannel]
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

data SplitChannel = SPLIT_CHANNEL PIdent
  deriving (Eq, Ord, Show, Read)

data ForkPhrase
    = FORK_PHRASE PIdent ProcessCommandsBlock
    | FORK_WITH_PHRASE PIdent [ForkChannel] ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ForkChannel = FORK_CHANNEL PIdent
  deriving (Eq, Ord, Show, Read)

data RacePhrase = RACE_PHRASE PIdent ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data PlugPhrase = PLUG_PHRASE ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ProcessCasePhrase
    = PROCESS_CASE_PHRASE Pattern ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

data ProcessSwitchPhrase
    = PROCESS_SWITCH_PHRASE Expr ProcessCommandsBlock
  deriving (Eq, Ord, Show, Read)

