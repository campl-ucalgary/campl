{-# LANGUAGE DeriveGeneric #-}
    -- Used to derive Generics which is used in Text.PrettyPrint
{-# LANGUAGE DerivingStrategies #-}
    -- Used to specify which deriving strategy we wish to use
{-# LANGUAGE DeriveAnyClass #-}
    -- Used to derive Out from Text.PrettyPrint.GenericPretty
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
    --Used to help derive classes like Enum (derives when we have newtype wrappers..)
{-# LANGUAGE DeriveDataTypeable #-}
    -- Used to derive the Data/Typeable class (useful for type generic programming)
{-# LANGUAGE DeriveFunctor #-}
module AMPLTypes where

import Data.Word
import Data.Maybe
import Data.Typeable
import Data.Data
import Data.Array
import Data.Coerce
import Data.List
import Data.Function
import Control.Arrow
import Control.Exception

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Queue (Queue)
import qualified Data.Queue as Queue
import Data.Stream (Stream)
import qualified Data.Stream as Stream

-- ASSUMES ALL ARRAYS ARE INDEXED STARTING AT 0

type ChannelIdRep = Int

newtype LocalChanID = LocalChanID ChannelIdRep  -- Local channel id
    deriving (Show, Enum, Read, Eq, Ord, Ix, Generic, Typeable)
    deriving anyclass Out

newtype GlobalChanID = GlobalChanID ChannelIdRep  -- global channel id
    deriving (Show, Read, Enum, Eq, Ord, Ix, Generic, Typeable)
    deriving anyclass Out

funIdStream :: [FunID]
funIdStream = Stream.toList (coerce wordStream)

newtype FunID = FunID Word  -- function id
    deriving (Show, Eq, Read, Ord, Ix, Generic, Typeable)
    deriving anyclass Out
    deriving newtype Enum

-- | an infinite stream of words. Useful for generating things with ConsIx
wordStream :: Stream Word
wordStream = Stream.iterate succ 0

-- indexing constructors...
newtype ConsIx = ConsIx Word
    deriving (Show, Read, Eq, Ord, Ix, Generic, Typeable)
    deriving anyclass Out
    deriving newtype Enum

-- indexing codata destructors...
newtype DesIx = DesIx Word
    deriving (Show, Read, Eq, Ord, Ix, Generic, Typeable)
    deriving anyclass Out
    deriving newtype Enum

-- indexing codata destructors...
newtype TupleIx = TupleIx Word
    deriving (Show, Read, Eq, Ord, Ix, Generic, Typeable)
    deriving anyclass Out
    deriving newtype Enum

-- By convention (i.e., Prashant), the output queue is the left queue
-- and the input queue is the right queue
data Polarity = Output | Input 
    deriving (Show, Read, Eq, Generic, Out, Typeable)

newtype HCaseIx = HCaseIx Word
    deriving (Show, Read, Eq, Ord, Ix, Generic, Typeable)
    deriving anyclass Out
    deriving newtype Enum
        -- corresponds to IHPut / IHCase -- either we put a request for
        -- an internal protocol (one given by the program itself) 
        -- or one for the external world (think IO action)

        {-
            THIS IS VERY CONFUSING READ THIS PORTION FOR WHAT
            THE INDICES MEAN.
            If we have a IHPut / IHCase scenario, then this 
            is simply indexing the IHCase (as given in the table)
            Otherwise, if it is not an IHPut / IHCase scenario,
            then this a service request (think IO action) that
            is defined by which global channel the IHPut HCaseIx
            is in.

            Then, by Prashant, we know that if HCaseIx is:
                - 0 then it corresponds to get (i.e., enter a number in the terminal)
                - 1 then it corresponds to put (i.e., put a number on the terminal)
                - 2 then it corresponds to close (i.e., close the service)
        -}


-- Polarity, LocalChanID and corresponding GlobalChanID
type Translation = (Polarity, (LocalChanID, GlobalChanID))
type LocalTranslationMapping = (Polarity, (LocalChanID, LocalChanID))
    -- A translation mapping is a mapping from a LocalChanID to 
    -- another LocalChanID. This is useful for the IRun instruction.

    -- Note about LocalTranslationMapping and the IRun instruction...
    -- We have translation mappings because we want to ``pass translations as an argument", 
    -- so that requires mapping local channel ids of the function to local channel ids in the context 
    -- the function is being called in which can map to the required global channel ids.
    -- See AMPLConcurrent.hs in stepConcurrent on the IRun pattern match case...


-- Stack, translations, environment, code
type Stec = ([Val], [Translation], [Val], [Instr])

-- Code, environment, stack
type Ces = ([Instr], [Val], [Val])

type Chm = Map GlobalChanID (Queue QInstr, Queue QInstr)
type ChmTranslation = Map GlobalChanID GlobalChanID

data ChannelManager = ChannelManager {
        channelManager :: Chm
        , channelManagerTranslations :: ChmTranslation
    } deriving (Show, Eq)


emptyQInstrQueues :: (Queue QInstr, Queue QInstr)
emptyQInstrQueues = (Queue.empty, Queue.empty)

-- | Helper function for constructing an empty channel manager from a list
initChannelManager :: [GlobalChanID] -> ChannelManager
initChannelManager gchs = 
    ChannelManager{ 
            channelManager = chm 
            , channelManagerTranslations = 
                Map.fromList (map (id&&&id) gchs)
        }
  where
    chm = Map.fromList 
        $ zip gchs (unfoldr (Just <<< const emptyQInstrQueues &&& id) ()) 

-- | given a channel manager translation with say a maps to b;
-- and given a pair (c, d); this function will change 
-- a maps to b TO a maps to d iff b == c.
composeChannelManagerTranslations :: 
    ChmTranslation -> 
    [(GlobalChanID, GlobalChanID)] ->
    ChmTranslation 
composeChannelManagerTranslations = 
    foldl (\acc (a,b) -> Map.map (\v -> if v == a then b else v) acc) 

deleteChannelManagerTranslations ::
    ChmTranslation -> 
    [GlobalChanID] ->
    ([GlobalChanID], ChmTranslation)
        -- (free channels, new ChmTranslation)
deleteChannelManagerTranslations channelmanagertranslation = 
    first (Set.toList . Set.fromList) . foldr f ([], channelmanagertranslation) 
  where
    f :: GlobalChanID -> ([GlobalChanID], ChmTranslation) -> ([GlobalChanID], ChmTranslation)
    f todelete acc =
        let (newdels, translation') = 
                (concatMap (\(a,b) -> [a,b]) *** Map.fromAscList) 
                (partition (\(a,b) -> b == todelete) $ Map.toAscList $ snd acc)
        in ((newdels++) *** const translation') acc 

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

-- | Composes a translation with its mapping
-- i.e, given a translation f and a translation mapping g,
-- we have the translation f applied after the translation mapping g (f after g).
composeTranslationWithLocalTranslationMapping :: 
    [Translation] -> 
    [LocalTranslationMapping] -> 
    Maybe [Translation]
composeTranslationWithLocalTranslationMapping as bs = foldr f g bs as
  where
    f :: (Polarity, (LocalChanID, LocalChanID)) -> 
        ([Translation] -> Maybe [Translation]) -> 
        [Translation] -> 
        Maybe [Translation]
    f (p, (lc, gc)) h ts = 
        let t' = (\(p', (lc', gc')) -> (p', (lc, gc'))) <$> 
                    find (\(p', (lc', gc')) -> 
                        p == p' && 
                        (coerce gc :: Int) == (coerce lc' :: Int))
                    ts
        in (:) <$> t' <*> h ts

    g :: [Translation] -> Maybe [Translation]
    g _ = Just []

-- | restrict translations to certain LocalChannel ids
restrictTranslation :: 
    [LocalChanID] -> -- ^ List of local channel ids to restrict the translations to
    [Translation] -> -- ^ translations to be restricted
    [Translation]
restrictTranslation lcls = filter (\(p, (lc, gc)) -> lc `elem` lcls)


-- Process instruction type.. (splits into parallel, and sequential)
data Instr =
    ConcurrentInstr ConcurrentInstr
    | SequentialInstr SequentialInstr
    deriving (Show, Read, Eq, Generic, Out, Typeable)

-- SEQUENTIAL INSTURCTIONS..
data SequentialInstr =
    -- Basic insturctions...
    IStore
    | IAccess Word
    | IRet
    | ICall FunID Word
        -- Call function FunID with Word arguments..

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
    | IIf [Instr] [Instr]
        -- if then [Instr] else [Instr]

    -- Tests if two Vals are equal or less than or equal to
    | IEq 
    | ILeq

    -- built in string instrucitons...
    | IUnstring
    | IConcat
    | IConcats Word -- Concat the top n elements
    | IToString
    | IToInt
    | IAppend

    -- data instructions...
    | ICons ConsIx Word 
        -- pushes the i'th constructor onto the stack with the top n elements in the stack
    | ICase (Array ConsIx [Instr])

    -- Codata instructions
    | IRec (Array DesIx [Instr]) -- create a record on the stack
    | IDest DesIx Word
        -- destructs the record by choosing the ith funciton closure with the top n elements of the stack

    | ITuple Word
        -- Number of elements in the tuple
    | ITupleElem TupleIx
        -- get the n'th element from a tuple
    deriving (Show, Read, Eq, Generic, Out, Typeable)

-- smart consturctors...
iStore :: Instr
iStore = SequentialInstr IStore

iOrBool :: Instr
iOrBool = SequentialInstr IOrBool

iAndBool :: Instr
iAndBool = SequentialInstr IAndBool

iEq :: Instr
iEq = SequentialInstr IEq

iLeq :: Instr
iLeq = SequentialInstr ILeq

iIf :: [Instr] -> [Instr] -> Instr
iIf as = SequentialInstr . IIf as

iAccess :: Word -> Instr
iAccess = SequentialInstr . IAccess 

iRet :: Instr
iRet = SequentialInstr IRet

iCall :: FunID -> Word -> Instr
iCall id = SequentialInstr . ICall id

iConst :: Val -> Instr
iConst = SequentialInstr . IConst 

iAddInt :: Instr
iAddInt = SequentialInstr IAddInt

iSubInt :: Instr
iSubInt = SequentialInstr ISubInt

iMulInt :: Instr
iMulInt = SequentialInstr IMulInt

iDivInt :: Instr
iDivInt = SequentialInstr IDivInt

iModInt :: Instr
iModInt = SequentialInstr IModInt

iCons :: ConsIx -> Word -> Instr
iCons ix n = SequentialInstr (ICons ix n)

iCase :: [[Instr]] -> Instr
iCase [] = SequentialInstr (ICase (listArray (coerce (1 :: Word) :: ConsIx, coerce (0 :: Word) :: ConsIx) []))
iCase cs = SequentialInstr 
    (ICase (listArray (coerce (0 :: Word) :: ConsIx, coerce (genericLength cs - 1 :: Word) :: ConsIx) cs))


iRec :: [[Instr]] -> Instr
iRec [] = SequentialInstr (IRec (listArray (coerce (1 :: Word) :: DesIx, coerce (0 :: Word) :: DesIx) []))
iRec cs = SequentialInstr 
    (IRec (listArray (coerce (0 :: Word) :: DesIx, coerce (genericLength cs - 1 :: Word) :: DesIx) cs))

iDest :: DesIx -> Word -> Instr
iDest ix n = SequentialInstr (IDest (coerce ix) n)

iTuple :: Word -> Instr
iTuple = SequentialInstr . ITuple

iTupleElem :: TupleIx -> Instr
iTupleElem = SequentialInstr . ITupleElem

iUnstring :: Instr
iUnstring = SequentialInstr IUnstring

iConcat :: Instr
iConcat = SequentialInstr IConcat

iConcats :: Word -> Instr
iConcats = SequentialInstr . IConcats

iToString :: Instr
iToString = SequentialInstr IToString

iToInt :: Instr
iToInt = SequentialInstr IToInt

iAppend :: Instr
iAppend = SequentialInstr IAppend

data Val = 
    VClos ([Instr], [Val])

    -- Primitive data types..
    | VInt Int
    | VBool Bool
    | VChar Char
    | VString String
    | VTuple (Array TupleIx Val)

    -- User defined data types..
    | VCons (ConsIx, [Val])
    | VRec (Array DesIx [Instr], [Val])
    deriving (Show, Read, Eq, Generic, Out, Typeable)


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


    | IRun [LocalTranslationMapping] FunID Word
        -- Translation mappings, FunctionID and number of arguments to call with this function..

    | IHPut LocalChanID HCaseIx
    | IHCase LocalChanID (Array HCaseIx [Instr])

    | IRace [(LocalChanID, [Instr])]
    deriving (Show, Read, Eq, Generic, Out, Typeable)

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

iId :: LocalChanID ->  LocalChanID  -> Instr
iId a = ConcurrentInstr . IId a

iRun :: [LocalTranslationMapping] -> FunID -> Word -> Instr
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
    deriving (Show, Eq, Generic, Out, Typeable)

-- | Broadcast channel instructions (used internally to transfer messages from
-- a process to a channel).. This is a layer of indirection for 
-- the Chan in order for the channel to update similar to QInstr
-- ch is GlobalChanID (we write this as a type parameter to generate the 
-- functor instance)
data BInstr' ch =
    BGet 
        -- Polarity and GlobalChanID to place QGet on
        (Polarity, ch) 
        ([Val], [Translation], [Val], [Instr])

    | BPut (Polarity, ch) Val

    | BSplit (Polarity, ch) (ch, ch)

    | BFork (Polarity, ch) 
        ([Translation], [Val] 
                , (LocalChanID, [Translation], [Instr])    
                , (LocalChanID, [Translation], [Instr])    
                )

    | BId (Polarity, ch) (ch, ch)

    | BClose (Polarity, ch)
    | BHalt [(Polarity, ch)] 

    | BPlug [ch]

    | BHPut (Polarity, ch) HCaseIx
    | BHCase (Polarity, ch) ([Val], [Translation], [Val], Array HCaseIx [Instr])

    | BRace 
        -- polarity, GlobalChanID to map to; and the list
        -- corresponding to the table; and the instructions c1, c2,..,cn
        -- Note that these are the only things that change between
        -- each channel
        [(Polarity, ch, [LocalChanID], [Instr])] 
        -- (s, t, e) (these are the same for all races...)
        ([Val], [Translation], [Val])


    | BNewGlobalChannels [GlobalChanID]
        -- ^ note that this is not a machine instruction but needed
        -- to generate the new translations
    deriving (Show, Eq, Generic, Out, Typeable, Functor)

type BInstr = BInstr' GlobalChanID 

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

-- | Data type used for terminating the TCP server by throwing an exception
data AmplExit = AmplExit
    deriving (Show, Exception)


-- instances for deriving the pretty printer since the array does
-- not have a Generics instance, so we cannot automatically derive
-- the Out instance. So we write it ourselves...
instance (Ix i, Out a) => Out (Array i a) where
    docPrec _ arr = doc (elems arr)
    doc = docPrec 0 

instance Out Word where
    docPrec _ = text . show 
    doc = docPrec 0
