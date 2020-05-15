{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AMPLTypes where

import Data.Word
import Data.Array
import Data.Coerce
import Data.List

-- ASSUMES ALL ARRAYS ARE INDEXED AT 0

newtype LocalChanID = LocalChanID Word  -- Local channel id
    deriving (Show, Eq)

newtype GlobalChanID = GlobalChanID Word  -- global channel id
    deriving (Show, Eq)

newtype PhysicalChanId = PhysicalChanID Word  -- global channel id
    deriving (Show, Eq)

newtype FunID = FunID Word  -- function id
    deriving (Show, Eq, Ord, Ix, Enum)

-- indexing constructors...
newtype ConsIx = ConsIx Word
    deriving (Show, Eq, Ord, Ix)

-- indexing codata destructors...
newtype DesIx = DesIx Word
    deriving (Show, Eq, Ord, Ix)

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
    ConcurrentInstr ConcurrentInstr
    | SequentialInstr SequentialInstr
    deriving Eq

instance Show Instr where
    show (ConcurrentInstr cs) = show cs
    show (SequentialInstr cs) = show cs


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
    | ICons ConsIx Word 
        -- pushes the i'th constructor onto the stack with the top n elements in the stack
    | ICase (Array ConsIx [Instr])

    -- Codata instructions
    | IRec (Array DesIx [Instr]) -- create a record on the stack
    | IDest DesIx Word
        -- destructs the record by choosing the ith funciton closure with the top n elements of the stack
    deriving (Show, Eq)

-- smart consturctors...
iStore :: Instr
iStore = SequentialInstr IStore

iAccess :: Word -> Instr
iAccess = SequentialInstr . IAccess 

iRet :: Instr
iRet = SequentialInstr IRet

iCall :: FunID -> Word -> Instr
iCall id = SequentialInstr . ICall id

iConstInt :: Int -> Instr 
iConstInt = SequentialInstr . IConstInt

iAddInt :: Instr
iAddInt = SequentialInstr IAddInt

iMulInt :: Instr
iMulInt = SequentialInstr IMulInt

iLeqInt :: Instr
iLeqInt = SequentialInstr ILeqInt

iCons :: Word -> Word -> Instr
iCons ix n = SequentialInstr (ICons (coerce ix) n)

iCase :: [[Instr]] -> Instr
iCase [] = SequentialInstr (ICase (listArray (coerce (1 :: Word) :: ConsIx, coerce (0 :: Word) :: ConsIx) []))
iCase cs = SequentialInstr 
    (ICase (listArray (coerce (0 :: Word) :: ConsIx, coerce (genericLength cs - 1 :: Word) :: ConsIx) cs))


iRec :: [[Instr]] -> Instr
iRec [] = SequentialInstr (IRec (listArray (coerce (1 :: Word) :: DesIx, coerce (0 :: Word) :: DesIx) []))
iRec cs = SequentialInstr 
    (IRec (listArray (coerce (0 :: Word) :: DesIx, coerce (genericLength cs - 1 :: Word) :: DesIx) cs))

iDest :: Word -> Word -> Instr
iDest ix n = SequentialInstr (IDest (coerce ix)  n)


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

    -- Primitive data types..
    | VInt Int
    | VBool Bool

    -- User defined data types..
    | VCons (ConsIx, [Val])
    | VRec (Array DesIx [Instr], [Val])
    deriving (Show, Eq)
