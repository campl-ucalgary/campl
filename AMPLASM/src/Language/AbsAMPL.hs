-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Language.AbsAMPL where

newtype Store = Store ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Load = Load ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Ret = Ret ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Call = Call ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype ConstInt = ConstInt ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype ConstChar = ConstChar ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype ConstString = ConstString ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype ToStr = ToStr ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype ToInt = ToInt ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype And = And ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Or = Or ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Append = Append ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Unstring = Unstring ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype LeqI = LeqI ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype EqI = EqI ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Leqc = Leqc ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Eqc = Eqc ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Leqs = Leqs ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Eqs = Eqs ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype ConcatS = ConcatS ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Add = Add ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Subtract = Subtract ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Mul = Mul ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Quot = Quot ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Rem = Rem ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Cons = Cons ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Case = Case ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype If = If ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Rec = Rec ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Get = Get ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Put = Put ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Hput = Hput ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Hcase = Hcase ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Split = Split ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Fork = Fork ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Plug = Plug ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Run = Run ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Race = Race ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Close = Close ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Halt = Halt ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Ch_Id = Ch_Id ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Main_run = Main_run ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype BTrue = BTrue ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype BFalse = BFalse ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype Character = Character ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype UIdent = UIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype PInteger = PInteger ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

newtype IIdent = IIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)

data AMPLCODE = Main [AMPL_CONSTRUCTS] START
  deriving (Eq, Ord, Show, Read)

data AMPL_CONSTRUCTS
    = IMPORT_CONSTRUCT IMPORT
    | HANDLE_CONSTRUCT HANDLES
    | COHANDLE_CONSTRUCT COHANDLES
    | CONSTRUCTOR_CONSTRUCT CONSTRUCTORS
    | DESTRUCTOR_CONSTRUCT DESTRUCTORS
    | PROCESSES_CONSTRUCT PROCESSES
    | FUNCTIONS_CONSTRUCT FUNCTIONS
  deriving (Eq, Ord, Show, Read)

data HANDLE_SPEC = Hand_spec UIdent [Handle]
  deriving (Eq, Ord, Show, Read)

data Handle = HandName UIdent
  deriving (Eq, Ord, Show, Read)

data IMPORT = Import IIdent
  deriving (Eq, Ord, Show, Read)

data CONSTRUCTORS = Constructors [STRUCTOR_SPEC]
  deriving (Eq, Ord, Show, Read)

data DESTRUCTORS = Destructors [STRUCTOR_SPEC]
  deriving (Eq, Ord, Show, Read)

data STRUCTOR_SPEC = Struct_spec UIdent [STRUCT]
  deriving (Eq, Ord, Show, Read)

data STRUCT = Struct UIdent PInteger
  deriving (Eq, Ord, Show, Read)

data HANDLES = Handles [HANDLE_SPEC]
  deriving (Eq, Ord, Show, Read)

data COHANDLES = Cohandles [HANDLE_SPEC]
  deriving (Eq, Ord, Show, Read)

data PROCESSES = Processes [PROCESS_SPEC]
  deriving (Eq, Ord, Show, Read)

data PROCESS_SPEC
    = Process_spec PIdent [Vars] [PIdent] [PIdent] COMS
  deriving (Eq, Ord, Show, Read)

data Vars = VName PIdent
  deriving (Eq, Ord, Show, Read)

data FUNCTIONS = Functions [FUNCTION_SPEC]
  deriving (Eq, Ord, Show, Read)

data FUNCTION_SPEC = Function_spec PIdent [Vars] COMS
  deriving (Eq, Ord, Show, Read)

data START = Start Main_run CHANNEL_SPEC COMS | Start_none
  deriving (Eq, Ord, Show, Read)

data CHANNEL_SPEC = Channel_spec [PIdent] [PIdent]
  deriving (Eq, Ord, Show, Read)

data COMS = Prog [COM]
  deriving (Eq, Ord, Show, Read)

data COM
    = AC_ASSIGN PIdent COM
    | AC_STOREf Store PIdent
    | AC_LOADf Load PIdent
    | AC_RET Ret
    | AC_CALLf Call PIdent [PIdent]
    | AC_INT ConstInt CInteger
    | AC_CHAR ConstChar Character
    | AC_STRING ConstString String
    | AC_TOSTR ToStr
    | AC_TOINT ToInt
    | AC_AND And
    | AC_OR Or
    | AC_APPEND Append
    | AC_TRUE BTrue
    | AC_FALSE BFalse
    | AC_UNSTRING Unstring
    | AC_LEQ LeqI
    | AC_EQ EqI
    | AC_LEQC Leqc
    | AC_EQC Eqc
    | AC_LEQS Leqs
    | AC_EQS Eqs
    | AC_CONCAT ConcatS Integer
    | AC_ADD Add
    | AC_SUB Subtract
    | AC_MUL Mul
    | AC_DIVQ Quot
    | AC_DIVR Rem
    | AC_CONS Cons PInteger PInteger
    | AC_STRUCT UIdent UIdent
    | AC_STRUCTAS UIdent UIdent [PIdent]
    | AC_CASEf Case PIdent [LABELCOMS]
    | AC_IF If PIdent COMS COMS
    | AC_RECORDf Rec [LABELCOMS]
    | AC_DEST UIdent UIdent PIdent
    | AC_DESTAS UIdent UIdent [PIdent] PIdent
    | AC_GETf Get PIdent PIdent
    | AC_HPUTf Hput UIdent UIdent PIdent
    | AC_HCASEf Hcase PIdent [LABELCOMS]
    | AC_PUTf Put PIdent PIdent
    | AC_SPLITf Split PIdent PIdent PIdent
    | AC_FORKf Fork PIdent PIdent [PIdent] COMS PIdent [PIdent] COMS
    | AC_PLUGf Plug [PIdent] [PIdent] COMS [PIdent] COMS
    | AC_RUNf Run PIdent [PIdent] [PIdent] [PIdent]
    | AC_IDF PIdent Ch_Id PIdent
    | AC_RACE Race [RACES]
    | AC_PROD [PIdent]
    | AC_PRODELEM PInteger PIdent
    | AC_EMSG String
    | AC_CLOSEf Close PIdent
    | AC_HALTf Halt PIdent
  deriving (Eq, Ord, Show, Read)

data LABELCOMS
    = Labelcoms1 UIdent UIdent COMS
    | Labelcoms2 UIdent UIdent [PIdent] COMS
  deriving (Eq, Ord, Show, Read)

data RACES = Races PIdent COMS
  deriving (Eq, Ord, Show, Read)

data CInteger = Positive PInteger | Negative PInteger
  deriving (Eq, Ord, Show, Read)

