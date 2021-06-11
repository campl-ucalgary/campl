{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE EmptyDataDeriving #-}
module MplMach.MplMachTypes where

import Optics
import Data.IORef
import Data.Void
import Data.Array
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Sequence

newtype LocalChan = LocalChan Int
  deriving (Show, Ord, Eq, Ix)

{- | A global channel is simpe simply a mapping to the corresponding channel manager.
 - Some notational convetions:
 -      - (q :|> head, head':<| q')  denotes how we will parse through these.
 - -}
newtype GlobalChan = GlobalChan (IORef (Seq QInstr, Seq QInstr))

instance Show GlobalChan where
    show _ = "GlobalChan _"

data LocalChanToLocalChanMapping
  deriving Show

data LocalChanToGlobalChanMapping
  deriving Show

-- function / process

-- * data type for the machine
data Stec = Stec 
    { _stack :: ![Val]
    , _translation :: Array LocalChan GlobalChan
    , _environment :: ![Val]
    , _code :: ![Instr]
    }
  deriving Show

-- *  Index types for the machine
type IxRep = Int
newtype CallIx = CallIx Int
  deriving (Show, Eq, Ord, Ix)
newtype CaseIx = CaseIx IxRep
  deriving (Show, Eq, Ord, Ix)
newtype HCaseIx = HCaseIx IxRep
  deriving (Show, Eq, Ord, Ix)
newtype TupleIx = TupleIx IxRep
  deriving (Show, Eq, Ord, Ix)

-- * Instruction types for the machine
data Instr
    = ConcInstr IConc
    | SeqInstr ISeq
  deriving Show

data IConc
    = IGet LocalChan
    | IPut LocalChan

    | ISplit LocalChan (LocalChan, LocalChan)
    -- split a into (b, c)
    | IFork LocalChan ( (LocalChan, [LocalChan], [Instr]) , (LocalChan, [LocalChan], [Instr]) )

    | IClose LocalChan
    | IHalt LocalChan

    | IId LocalChan LocalChan

    -- We have [a1, .., a_n], and by convention (i.e., Prashant) we have (out channel, in channel)
    | IPlug [LocalChan] (([LocalChan], [Instr]), ([LocalChan], [Instr]))


    | IRun [LocalChanToLocalChanMapping] CallIx Word
    -- Translation mappings, FunctionID and number of arguments to call with this function..

    | IHPut LocalChan HCaseIx
    | IHCase LocalChan (Array HCaseIx [Instr])

    | IRace [(LocalChan, [Instr])]
  deriving Show


data ISeq 
    -- Basic insturctions...
    = IStore
    -- | access the n'th element in the stack
    | IAccess Int
    | IRet
    -- | Call function CallIx 
    | ICall [Instr]

    | IConst Val

    -- built in int instrucitons...
    | IAddInt
    | ISubInt
    | IMulInt
    | IDivInt
    | IModInt

    -- built in bool instructions
    | IOrBool
    | IAndBool
    | INotBool
    -- | if then [Instr] else [Instr]
    | IIf [Instr] [Instr]

    -- Tests if two Vals are equal or less than or equal to
    | IEqInt
    | ILeqInt

    -- data instructions...
    -- | pushes the i'th constructor onto the stack with the top n elements in the stack
    | ICons CaseIx Int 
    | ICase (Array CaseIx [Instr])

    -- Codata instructions
    | IRec (Array CaseIx [Instr]) -- create a record on the stack
    | IDest CaseIx Int
        -- destructs the record by choosing the ith funciton closure with the top n elements of the stack

    -- Unused... (just copied because Prashant had it)
    -- | Number of elements in the tuple
    | ITuple Int
    -- | get the n'th element from a tuple
    | ITupleElem TupleIx

    | IErrorMsg String
    deriving Show


data Val 
    = VClos [Instr] [Val]
    | VInt Int
    | VBool Bool
    | VChar Char
    | VTuple (Array TupleIx Val)

    | VCons CaseIx [Val]
    | VRec (Array CaseIx [Instr]) [Val]
  deriving Show

-- * Instruction types for channel manager
data QInstr 
    = QGet Stec
    | QPut Instr
    | QSplit GlobalChan GlobalChan
    | QFork 
        LocalChanToGlobalChanMapping 
        [Val]
        (LocalChan, LocalChanToGlobalChanMapping, [Instr])
        (LocalChan, LocalChanToGlobalChanMapping, [Instr])
    | QId GlobalChan GlobalChan

    -- this is not needed, since we just build open the new channels immediately in the 
    -- channel manager
    | QPlug !Void

    | QClose 
    | QHalt 

    -- not needed, since a proces can just call run immmediately
    | QRun !Void 

    | QHPut HCaseIx
    | QHCase Stec (Array HCaseIx [Instr])

    -- | other channels to race, stec
    | QRace [LocalChan] Stec

  deriving Show



-- * Template haskell
$(makeClassyPrisms ''Val)
$(makeClassyPrisms ''IConc)
$(makeClassyPrisms ''ISeq)
$(makePrisms ''Instr)
$(makeLenses ''Stec)

instance AsISeq Instr where
    _ISeq = _SeqInstr

instance AsIConc Instr where
    _IConc = _ConcInstr

