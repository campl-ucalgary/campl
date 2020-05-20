-- Used to derive Out from Text.PrettyPrint.GenericPretty
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module AMPLTypes where

import Data.Word
import Data.Array
import Data.Coerce
import Data.List

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Queue (Queue)
import Data.Stream (Stream)
import qualified Data.Stream as Stream

-- ASSUMES ALL ARRAYS ARE INDEXED AT 0

newtype LocalChanID = LocalChanID Word  -- Local channel id
    deriving (Show, Eq, Ord, Ix, Generic, Out)

newtype GlobalChanID = GlobalChanID Word  -- global channel id
    deriving (Show, Eq, Ord, Ix, Generic, Out)

newtype PhysicalChanId = PhysicalChanID Word  -- physical channel id
    deriving (Show, Eq, Generic, Out)
    -- unused...

newtype FunID = FunID Word  -- function id
    deriving (Show, Eq, Ord, Ix, Generic, Out)

-- guaranteed distinct from the rest of the function ids for
-- the special main function
mainFunID :: FunID
mainFunID = FunID 0

-- inifinite stream of function ids..
funIDStream :: [FunID]
funIDStream = unfoldr (\(FunID n) -> Just (FunID n, FunID (n + 1))) (FunID 1) 

-- | inifinite stream of channel ids (reserves 0 for the console)
chanIdStream :: Stream Word
chanIdStream = Stream.iterate succ 0

-- indexing constructors...
newtype ConsIx = ConsIx Word
    deriving (Show, Eq, Ord, Ix, Generic, Out)

-- indexing codata destructors...
newtype DesIx = DesIx Word
    deriving (Show, Eq, Ord, Ix, Generic, Out)

-- By convention (i.e., Prashant), the output queue is the left queue
-- and the input queue is the right queue
data Polarity = Output | Input 
    deriving (Show, Eq, Generic, Out)

newtype HCaseIx = HCaseIx Word
    deriving (Show, Eq, Ord, Ix, Generic, Out)
        -- corresponds to  IHPut / IHCase -- either we put a request for
        -- an internal protocol (one given by the program itself) 
        -- or one for the external world (think IO action)

type Translation = (Polarity, (LocalChanID, GlobalChanID))

-- Stack, translations, environment, code
type Stec = ([Val], [Translation], [Val], [Instr])

-- Code, environment, stack
type Ces = ([Instr], [Val], [Val])

type Chm = Map GlobalChanID (Queue QInstr, Queue QInstr)

-- | Helper function for constructing a channel manager from a list
mkChm :: [(GlobalChanID, (Queue QInstr, Queue QInstr))] -> Chm
mkChm = Map.fromAscList . sortBy (\(a,_) (b,_) -> compare a b)


-- look up the LocalChanID to the corresponding (Polarity, GlobalChanID)
lookupLocalChanIDToGlobalChanID :: 
    LocalChanID -> 
    [Translation] -> 
    Maybe (Polarity, GlobalChanID)
lookupLocalChanIDToGlobalChanID lch ts =
    (\(p, (l, g)) -> (p, g)) <$> find ((== lch) . fst . snd) ts

-- | Deletes all translations given LocalChanID
deleteTranslation :: LocalChanID -> [Translation] -> [Translation]
deleteTranslation lc = 
    foldr 
        (\(p, (lc', gc)) acc -> if lc == lc' then acc else (p, (lc', gc)):acc) 
        []

-- | restrict translations to certain LocalChannel ids
restrictTranslation :: 
    [LocalChanID] -> -- ^ List of local channel ids to restrict the translations to
    [Translation] -> -- ^ translations to be restricted
    [Translation]
restrictTranslation lcls = filter (\(p, (lc, gc)) -> lc `elem` lcls)

-- | Composes translations i.e. if f and g are translations, we get 
-- a translation which is f . g (f after g).
composeTranslation :: [Translation] -> [Translation] -> Maybe [Translation]
composeTranslation as bs = foldr f g bs as
  where
    f :: Translation -> 
        ([Translation] -> Maybe [Translation]) -> 
        [Translation] -> 
        Maybe [Translation]
    f (p, (lc, gc)) h ts = do
        t' <- (\(p', (lc', gc')) -> (p', (lc, gc'))) <$> 
                find (\(p', (lc', gc')) -> p == p' 
                    && (coerce gc :: Word) == (coerce lc' :: Word))
                ts
        (t':) <$> h ts

    g :: [Translation] -> Maybe [Translation]
    g _ = Just []

-- Process instruction type.. (splits into parallel, and sequential)
data Instr =
    ConcurrentInstr ConcurrentInstr
    | SequentialInstr SequentialInstr
    deriving (Eq, Generic, Out)

instance Show Instr where
    show (ConcurrentInstr cs) = show cs
    show (SequentialInstr cs) = show cs
    -- We want to show instructions without
    -- the extra ConcurrentInstr, and SequentialInstr
    -- constructors..

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
    deriving (Show, Eq, Generic, Out)

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


data Val = 
    VClos ([Instr], [Val])

    -- Primitive data types..
    | VInt Int
    | VBool Bool

    -- User defined data types..
    | VCons (ConsIx, [Val])
    | VRec (Array DesIx [Instr], [Val])
    deriving (Show, Eq, Generic, Out)


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

    -- We have [a1, .., a_n], and by convention (i.e., Prashant) we have (out channel, in channel)
    | IPlug [LocalChanID] (([LocalChanID], [Instr]), ([LocalChanID], [Instr]))


    | IRun [Translation] FunID Word
        -- Translations, FunctionID and number of arguments to call with this function..

    | IHPut LocalChanID HCaseIx
    | IHCase LocalChanID (Array HCaseIx [Instr])

    | IRace [(LocalChanID, [Instr])]
    deriving (Show, Eq, Generic, Out)

-- smart constructors for ConcurrentInstr
iGet :: LocalChanID -> Instr
iGet = ConcurrentInstr . IGet

iPut :: LocalChanID -> Instr
iPut = ConcurrentInstr . IPut

iSplit :: LocalChanID -> (LocalChanID, LocalChanID) -> Instr
iSplit a bc = ConcurrentInstr $ ISplit a bc

iFork :: LocalChanID -> ((LocalChanID, [LocalChanID], [Instr]) , (LocalChanID, [LocalChanID], [Instr])) -> Instr
iFork a bc = ConcurrentInstr $ IFork a bc

iClose :: LocalChanID -> Instr
iClose = ConcurrentInstr . IClose

iPlug :: [LocalChanID] -> (([LocalChanID], [Instr]), ([LocalChanID], [Instr])) -> Instr
iPlug lcs =  ConcurrentInstr . IPlug lcs

iHalt :: [LocalChanID] -> Instr
iHalt = ConcurrentInstr . IHalt

iRun :: [Translation] -> FunID -> Word -> Instr
iRun ts f w = ConcurrentInstr $ IRun ts f w

iHPut :: LocalChanID -> HCaseIx -> Instr
iHPut a b = ConcurrentInstr $ IHPut a b

iHCase :: LocalChanID -> [[Instr]] -> Instr
iHCase n [] = ConcurrentInstr $ IHCase n (listArray (coerce (1 :: Word) :: HCaseIx , coerce (0 :: Word) :: HCaseIx) [])
iHCase n is = ConcurrentInstr $ IHCase n (listArray (coerce (0 :: Word) :: HCaseIx, coerce (genericLength is - 1 :: Word) :: HCaseIx) is)

iRace :: [(LocalChanID, [Instr])] -> Instr
iRace = ConcurrentInstr . IRace

data QInstr =
    QGet ([Val], [Translation], [Val], [Instr])
        -- (s, t, e ,c)
    | QPut Val
    | QSplit (GlobalChanID, GlobalChanID)
    | QFork ([Translation], [Val] 
        -- t, e,
                , (LocalChanID, [Translation], [Instr])    
                    -- alpha_1, \Gamma_1, code_1
                , (LocalChanID, [Translation], [Instr])    
                    -- alpha_2, \Gamma_2, code_2
                )
    | QId (GlobalChanID, GlobalChanID)

    | QClose
    | QHalt

    | QHPut HCaseIx
    | QHCase ([Val], [Translation], [Val], Array HCaseIx [Instr])

    | QRace ([LocalChanID], ([Val], [Translation], [Val], [Instr]))
        -- other channels to race, (s,t,e,c)
    deriving (Show, Eq, Generic, Out)

-- | Broadcast channel instructions (used internally to transfer messages from
-- a process to a channel).. This is a layer of indirection for 
-- the Chan in order for the channel to update similar to QInstr
data BInstr =
    BGet 
        -- Polarity and GlobalChanID to place QGet on
        (Polarity, GlobalChanID) 
        ([Val], [Translation], [Val], [Instr])

    | BPut (Polarity, GlobalChanID) Val

    | BSplit (Polarity, GlobalChanID) (GlobalChanID, GlobalChanID)

    | BFork (Polarity, GlobalChanID) 
        ([Translation], [Val] 
                , (LocalChanID, [Translation], [Instr])    
                , (LocalChanID, [Translation], [Instr])    
                )

    | BId (Polarity, GlobalChanID) (GlobalChanID, GlobalChanID)

    | BClose (Polarity, GlobalChanID)
    | BHalt [(Polarity, GlobalChanID)] 

    | BPlug [GlobalChanID]

    | BHPut (Polarity, GlobalChanID) HCaseIx
    | BHCase (Polarity, GlobalChanID) ([Val], [Translation], [Val], Array HCaseIx [Instr])

    | BRace 
        -- polarity, GlobalChanID to map to; and the list
        -- corresponding to the table; and the instructions c1, c2,..,cn
        -- Note that these are the only things that change between
        -- each channel
        [(Polarity, GlobalChanID, [LocalChanID], [Instr])] 
        -- (s, t, e) (these are the same for all races...)
        ([Val], [Translation], [Val])
    deriving (Show, Eq, Generic, Out)

-- smart constructors..
qGet :: ([Val], [Translation], [Val], [Instr]) -> QInstr
qGet = QGet

qPut :: Val -> QInstr
qPut = QPut

qSplit :: (GlobalChanID, GlobalChanID) -> QInstr
qSplit = QSplit

qFork :: ([Translation], [Val], (LocalChanID, [Translation], [Instr]), (LocalChanID, [Translation], [Instr])) -> QInstr
qFork = QFork

qId :: (GlobalChanID, GlobalChanID) -> QInstr
qId = QId 

qClose :: QInstr
qClose = QClose

qHalt :: QInstr
qHalt = QHalt

qHPut :: Word -> QInstr
qHPut = QHPut . HCaseIx

qHCase :: ([Val], [Translation], [Val], [[Instr]]) -> QInstr
qHCase (s, t, e, []) = QHCase (s,t,e,listArray (coerce (1 :: Word) :: HCaseIx, coerce (0 :: Word) :: HCaseIx) [])
qHCase (s,t,e,is) = QHCase (s,t,e, listArray (coerce (0 :: Word) :: HCaseIx, coerce (genericLength is - 1 :: Word) :: HCaseIx) is)

qRace :: ([LocalChanID], ([Val], [Translation], [Val], [Instr])) -> QInstr
qRace = QRace

-- instances for deriving the pretty printer since the array does
-- not have a Generics instance, so we cannot automatically derive
-- the Out instance. So we write it ourselves...
instance (Ix i, Out a) => Out (Array i a) where
    docPrec _ arr = doc (elems arr)
    doc = docPrec 0 

instance Out Word where
    docPrec _ = text . show 
    doc = docPrec 0
