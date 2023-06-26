-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language MPL.

module MplLanguage.AbsMPL where

import Prelude (String)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Int, Maybe(..)
  )

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
    | PROCESS_ON PIdent [OnPhrase]
    | PROCESS_SPLIT Split PIdent [SplitChannel]
    | PROCESS_FORK Fork PIdent [ForkPhrase]
    | PROCESS_ID PIdent ChId PIdent
    | PROCESS_NEG PIdent ChId PIdent
    | PROCESS_RACE [RacePhrase]
    | PROCESS_PLUG [PlugPhrase]
    | PROCESS_CASE Case Expr [ProcessCasePhrase]
    | PROCESS_IF Expr ProcessCommandsBlock ProcessCommandsBlock
    | PROCESS_SWITCH [ProcessSwitchPhrase]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OnPhrase
    = ON_PUT Put Expr
    | ON_GET Get Pattern
    | ON_HPUT HPut UIdent
    | ON_HCASE HCase [HCasePhrase]
    | ON_FORK Fork [ForkPhrase]
    | ON_SPLIT Split [SplitChannel]
    | ON_CLOSE Close
    | ON_HALT Halt
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

newtype PInteger = PInteger ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PDouble = PDouble ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PChar = PChar ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PString = PString ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Par = Par ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Tensor = Tensor ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype LBracket = LBracket ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype RBracket = RBracket ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype LSquareBracket = LSquareBracket ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype RSquareBracket = RSquareBracket ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype NullPattern = NullPattern ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Colon = Colon ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl1op = Infixl1op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl2op = Infixl2op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl3op = Infixl3op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl4op = Infixl4op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl5op = Infixl5op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl6op = Infixl6op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixr7op = Infixr7op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Infixl8op = Infixl8op ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Close = Close ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Halt = Halt ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Get = Get ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Put = Put ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype HCase = HCase ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype HPut = HPut ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Split = Split ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Fork = Fork ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype ChId = ChId ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Case = Case ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype UIdent = UIdent ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype PIdent = PIdent ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype UPIdent = UPIdent ((C.Int, C.Int), String)
  deriving (C.Eq, C.Ord, C.Show, C.Read)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition PInteger where
  hasPosition (PInteger (p, _)) = C.Just p

instance HasPosition PDouble where
  hasPosition (PDouble (p, _)) = C.Just p

instance HasPosition PChar where
  hasPosition (PChar (p, _)) = C.Just p

instance HasPosition PString where
  hasPosition (PString (p, _)) = C.Just p

instance HasPosition Par where
  hasPosition (Par (p, _)) = C.Just p

instance HasPosition Tensor where
  hasPosition (Tensor (p, _)) = C.Just p

instance HasPosition LBracket where
  hasPosition (LBracket (p, _)) = C.Just p

instance HasPosition RBracket where
  hasPosition (RBracket (p, _)) = C.Just p

instance HasPosition LSquareBracket where
  hasPosition (LSquareBracket (p, _)) = C.Just p

instance HasPosition RSquareBracket where
  hasPosition (RSquareBracket (p, _)) = C.Just p

instance HasPosition NullPattern where
  hasPosition (NullPattern (p, _)) = C.Just p

instance HasPosition Colon where
  hasPosition (Colon (p, _)) = C.Just p

instance HasPosition Infixl1op where
  hasPosition (Infixl1op (p, _)) = C.Just p

instance HasPosition Infixl2op where
  hasPosition (Infixl2op (p, _)) = C.Just p

instance HasPosition Infixl3op where
  hasPosition (Infixl3op (p, _)) = C.Just p

instance HasPosition Infixl4op where
  hasPosition (Infixl4op (p, _)) = C.Just p

instance HasPosition Infixl5op where
  hasPosition (Infixl5op (p, _)) = C.Just p

instance HasPosition Infixl6op where
  hasPosition (Infixl6op (p, _)) = C.Just p

instance HasPosition Infixr7op where
  hasPosition (Infixr7op (p, _)) = C.Just p

instance HasPosition Infixl8op where
  hasPosition (Infixl8op (p, _)) = C.Just p

instance HasPosition Close where
  hasPosition (Close (p, _)) = C.Just p

instance HasPosition Halt where
  hasPosition (Halt (p, _)) = C.Just p

instance HasPosition Get where
  hasPosition (Get (p, _)) = C.Just p

instance HasPosition Put where
  hasPosition (Put (p, _)) = C.Just p

instance HasPosition HCase where
  hasPosition (HCase (p, _)) = C.Just p

instance HasPosition HPut where
  hasPosition (HPut (p, _)) = C.Just p

instance HasPosition Split where
  hasPosition (Split (p, _)) = C.Just p

instance HasPosition Fork where
  hasPosition (Fork (p, _)) = C.Just p

instance HasPosition ChId where
  hasPosition (ChId (p, _)) = C.Just p

instance HasPosition Case where
  hasPosition (Case (p, _)) = C.Just p

instance HasPosition UIdent where
  hasPosition (UIdent (p, _)) = C.Just p

instance HasPosition PIdent where
  hasPosition (PIdent (p, _)) = C.Just p

instance HasPosition UPIdent where
  hasPosition (UPIdent (p, _)) = C.Just p

