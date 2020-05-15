module AMPLTypes where

import Data.Word
import Data.Array

newtype LocalChanID = LocalChanID Word  -- Local channel id
    deriving (Show, Eq)

newtype GlobalChanID = GlobalChanID Word  -- global channel id
    deriving (Show, Eq)

newtype PhysicalChanId = PhysicalChanID Word  -- global channel id
    deriving (Show, Eq)

newtype FunID = FunID Word  -- function id
    deriving (Show, Eq)

data Polarity = Input | Output
    deriving (Show, Eq)

data Service = Internal Word | External Word
    deriving (Show, Eq)
        -- corresponds to  IHPut -- either we put a request for
        -- an internal protocol (one given by the program itself) 
        -- or one for the external world (think IO action)

type Translation = (Polarity, (LocalChanID, GlobalChanID))

-- instruction type.. (splits into parallel, and sequential)
data Instr =
    ConcurrentInstr
    | SequentialInstr
    deriving (Show, Eq)

-- SEQUENTIAL INSTURCTIONS..
data SequentialInstr =
    -- Basic insturctions...
    IStore
    | IAccess Word
    | IRet
    | ICall FunID Word
        -- Call function FunID with Word arguments..

    -- built in instructions..
    | IConstInt Int
    | IAddInt
    | IMulInt
    | ILeqInt

    -- data instructions...
    | ICons Word Word 
        -- pushes the i'th constructor onto the stack with the top n elements in the stack
    | ICase (Array Word [Instr])

    -- Codata instructions
    | IRec (Array Word [Instr]) -- create a record on the stack
    | IDest Word Word
        -- destructs the record by choosing the ith funciton closure with the top n elements of the stack
    deriving (Show, Eq)


-- CONCURRENT INSTRUCTIONS
data ConcurrentInstr =
    IGet LocalChanID
    | IPut LocalChanID

    | ISplit LocalChanID (LocalChanID, LocalChanID)
        -- split a into (b, c)
    | IFork LocalChanID 
        ((LocalChanID, [LocalChanID], [Instr])
        , (LocalChanID, [LocalChanID], [Instr]))

    | IClose LocalChanID
    | IHalt [LocalChanID]   -- ? prashant does this..

    | IId LocalChanID LocalChanID

    | IRun [Translation] FunID Word
        -- Translations, FunctionID and number of arguments to call with this function..

    | IHPut LocalChanID Service
    | IHCase LocalChanID (Array Word [Instr])

    | IRace [(LocalChanID, [Instr])]
    deriving (Show, Eq)

data Val = 
    VClos ([Instr], [Val])
    | VInt Int
