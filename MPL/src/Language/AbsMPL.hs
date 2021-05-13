-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Language.AbsMPL where

import Prelude (Char, Double, Int, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)

newtype PInteger = PInteger ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PDouble = PDouble ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PChar = PChar ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PString = PString ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Par = Par ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Tensor = Tensor ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype LBracket = LBracket ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype RBracket = RBracket ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype LSquareBracket = LSquareBracket ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype RSquareBracket = RSquareBracket ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype NullPattern = NullPattern ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Colon = Colon ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl1op = Infixl1op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl2op = Infixl2op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl3op = Infixl3op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl4op = Infixl4op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl5op = Infixl5op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl6op = Infixl6op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixr7op = Infixr7op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl8op = Infixl8op ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Close = Close ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Halt = Halt ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Get = Get ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Put = Put ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype HCase = HCase ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype HPut = HPut ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Split = Split ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Fork = Fork ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype ChId = ChId ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Case = Case ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype UIdent = UIdent ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PIdent = PIdent ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype UPIdent = UPIdent ((Int, Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MplProg = MPL_PROG [MplStmt]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MplStmt
    = MPL_DEFN_STMS_WHERE [MplDefn] [MplWhere]
    | MPL_DEFN_STMS [MplDefn]
    | MPL_STMT MplDefn
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MplWhere = MPL_WHERE MplStmt
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MplDefn
    = MPL_SEQUENTIAL_TYPE_DEFN SequentialTypeDefn
    | MPL_CONCURRENT_TYPE_DEFN ConcurrentTypeDefn
    | MPL_FUNCTION_DEFN FunctionDefn
    | MPL_PROCESS_DEFN ProcessDefn
    | MPL_DEFNTEST
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MplType
    = MPL_TYPE MplType
    | PAR_TYPE MplType Par MplType
    | TENSOR_TYPE MplType Tensor MplType
    | MPL_UIDENT_ARGS_TYPE UIdent LBracket [MplType] RBracket
    | MPL_UIDENT_SEQ_CONC_ARGS_TYPE UIdent LBracket [MplType] [MplType] RBracket
    | MPL_UIDENT_NO_ARGS_TYPE UIdent
    | MPL_UNIT_TYPE LBracket RBracket
    | MPL_BRACKETED_TYPE LBracket MplType RBracket
    | MPL_LIST_TYPE LSquareBracket MplType RSquareBracket
    | MPL_TUPLE_TYPE LBracket MplType [TupleListType] RBracket
    | MPL_SEQ_ARROW_TYPE [ForallVarList] [MplType] MplType
    | MPL_CONC_ARROW_TYPE [ForallVarList] [MplType] [MplType] [MplType]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TupleListType = TUPLE_LIST_TYPE MplType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ForallVarList = MPL_SEQ_FUN_TYPE_FORALL_LIST UIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SequentialTypeDefn
    = DATA_DEFN [SeqTypeClauseDefn] | CODATA_DEFN [SeqTypeClauseDefn]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SeqTypeClauseDefn
    = SEQ_TYPE_CLAUSE MplType MplType [SeqTypePhraseDefn]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SeqTypePhraseDefn
    = SEQ_TYPE_PHRASE [TypeHandleName] [MplType] MplType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConcurrentTypeDefn
    = PROTOCOL_DEFN [ConcurrentTypeClauseDefn]
    | COPROTOCOL_DEFN [ConcurrentTypeClauseDefn]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConcurrentTypeClauseDefn
    = CONCURRENT_TYPE_CLAUSE MplType MplType [ConcurrentTypePhraseDefn]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConcurrentTypePhraseDefn
    = CONCURRENT_TYPE_PHRASE [TypeHandleName] MplType MplType
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TypeHandleName = TYPE_HANDLE_NAME UIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

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
    | STRING_EXPR PString
    | CHAR_EXPR PChar
    | DOUBLE_EXPR PDouble
    | UNIT_EXPR LBracket RBracket
    | FOLD_EXPR Expr [FoldExprPhrase]
    | UNFOLD_EXPR Expr [UnfoldExprPhrase]
    | CASE_EXPR Case Expr [PattExprPhrase]
    | SWITCH_EXP [SwitchExprPhrase]
    | DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR UIdent LBracket [Expr] RBracket
    | DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR UIdent
    | TUPLE_EXPR LBracket Expr [TupleExprList] RBracket
    | FUN_EXPR PIdent LBracket [Expr] RBracket
    | RECORD_EXPR LBracket [RecordExprPhrase] RBracket
    | BRACKETED_EXPR LBracket Expr RBracket
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data UnfoldExprPhrase = UNFOLD_EXPR_PHRASE Pattern [FoldExprPhrase]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FoldExprPhrase = FOLD_EXPR_PHRASE UIdent Colon [Pattern] Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data LetExprPhrase = LET_EXPR_PHRASE MplStmt
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TupleExprList = TUPLE_EXPR_LIST Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RecordExprPhrase
    = RECORD_EXPR_PHRASE UIdent Expr
    | RECORD_EXPR_HIGHER_ORDER_PHRASE UIdent PattExprPhrase
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SwitchExprPhrase = SWITCH_EXPR_PHRASE Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PattExprPhrase = PATTERN_TO_EXPR [Pattern] Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

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
    | STR_PATTERN PString
    | CHAR_PATTERN PChar
    | INT_PATTERN PInteger
    | NULL_PATTERN NullPattern
    | BRACKETED_PATTERN LBracket Pattern RBracket
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TupleListPattern = TUPLE_LIST_PATTERN Pattern
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DestructorPatternPhrase
    = DESTRUCTOR_PATTERN_PHRASE UIdent Pattern
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunctionDefn
    = INTERNAL_TYPED_FUNCTION_DEFN PIdent MplType [PattExprPhrase]
    | TYPED_FUNCTION_DEFN PIdent [MplType] MplType [PattExprPhrase]
    | FUNCTION_DEFN PIdent [PattExprPhrase]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcessDefn
    = TYPED_PROCESS_DEFN PIdent [MplType] [MplType] [MplType] [ProcessPhrase]
    | INTERNAL_TYPED_PROCESS_DEFN PIdent MplType [ProcessPhrase]
    | PROCESS_DEFN PIdent [ProcessPhrase]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcessPhrase
    = PROCESS_PHRASE [Pattern] [PIdent] [PIdent] ProcessCommandsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcessCommandsBlock
    = PROCESS_COMMANDS_DO_BLOCK [ProcessCommand]
    | PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK ProcessCommand
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcessCommand
    = PROCESS_RUN PIdent LBracket [Expr] [PIdent] [PIdent] RBracket
    | PROCESS_CLOSE Close PIdent
    | PROCESS_HALT Halt PIdent
    | PROCESS_GET Get Pattern PIdent
    | PROCESS_PUT Put Expr PIdent
    | PROCESS_HCASE HCase PIdent [HCasePhrase]
    | PROCESS_HPUT HPut UIdent PIdent
    | PROCESS_SPLIT Split PIdent [SplitChannel]
    | PROCESS_FORK Fork PIdent [ForkPhrase]
    | PROCESS_ID PIdent ChId PIdent
    | PROCESS_NEG PIdent ChId PIdent
    | PROCESS_RACE [RacePhrase]
    | PROCESS_PLUG [PlugPhrase]
    | PROCESS_CASE Case Expr [ProcessCasePhrase]
    | PROCESS_SWITCH [ProcessSwitchPhrase]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data HCasePhrase = HCASE_PHRASE UIdent ProcessCommandsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SplitChannel = SPLIT_CHANNEL PIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ForkPhrase
    = FORK_PHRASE PIdent ProcessCommandsBlock
    | FORK_WITH_PHRASE PIdent [ForkChannel] ProcessCommandsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ForkChannel = FORK_CHANNEL PIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RacePhrase = RACE_PHRASE PIdent ProcessCommandsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PlugPhrase
    = PLUG_PHRASE ProcessCommandsBlock
    | PLUG_PHRASE_AS [PIdent] [PIdent] ProcessCommandsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcessCasePhrase
    = PROCESS_CASE_PHRASE Pattern ProcessCommandsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ProcessSwitchPhrase
    = PROCESS_SWITCH_PHRASE Expr ProcessCommandsBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

