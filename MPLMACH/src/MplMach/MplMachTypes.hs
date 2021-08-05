{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-| This module all the data types for the abstract machine.
-}
module MplMach.MplMachTypes where

import Optics
import Data.IORef
import Data.Void
import Data.Array
import Data.Coerce
import Data.Foldable
import Data.Traversable

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception

import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map

import Data.Set (Set (..))
import qualified Data.Set as Set

import Control.Arrow

import System.IO.Unsafe

import MplMach.MplMachException

import qualified Text.Show.Pretty as PrettyShow

{- | A channel manager queue is an 'TVar' to a queue (well a 'Seq') of 'QInstr' -}
-- newtype ChMQueue = ChMQueue (TVar (TQueue QInstr))
-- newtype ChMQueue = ChMQueue (TVar (TQueue QInstr))

data ChMQueueChain 
    = CCons (TVar ChMQueueChain)
    | CNil (TQueue QInstr)

{- | 'ChMQueue' 
 
Notes about the debug...
When in debug mode, we include some not totally correct ids 
of each of the elements, which makes it easier to 'show' which 
things are referring to what i.e., this are simply showable 
wrapers around 'TVar's. But in the release build, these are simply not 
necessary..
-}
#if MPL_MACH_DEBUG
data ChMQueue = ChMQueue 
    { _chMId :: Int
    , _chMQueueChainRef :: TVar ChMQueueChain
    }
instance Show ChMQueue where
    -- show _ = "ChMQueue _"
    show chm = "ChMQueue " ++ show (_chMId chm)
#else
newtype ChMQueue = ChMQueue { _chMQueueChainRef :: TVar ChMQueueChain
    }
instance Show ChMQueue where
    show _ = "ChMQueue _"
#endif

{- | helpful little debug function to essentially get a snap shot of the queue 
a given point of time -}
{-
showChMQueue :: 
    ChMQueue ->
    IO _ 
showChMQueue (ChMQueue tvar) = atomically $ do
    q <- readTVar tvar
    res <- flushTQueue q
    for (reverse res) (unGetTQueue q)
    return res
-}
    {-
    q <- readTVar tvar
    res <- tryPeekTQueue q
    -- for (reverse res) (unGetTQueue q)
    return [res]
    -}

    

{- | old convention from Prashant: output queue is left queue, and input queue is right queue -}
data ChMQueues = ChMQueues 
    { _chMOutputQueue :: ChMQueue
    , _chMInputQueue :: ChMQueue
    }
  deriving Show

{- | A local channel is simply an index in the translation. See 'Stec' below. -}
newtype LocalChan = LocalChan Int
  deriving (Show, Ord, Eq, Ix)

{- | A service channel -}
newtype ServiceCh = ServiceCh Int
  deriving (Show, Eq, Ord)

{- | A global channel is simpe simply a mapping to the corresponding channel manager.
 - Some notational convetions:
 -      - (head :<| q, head':<| q')  denotes how we will parse through these.
 -}
newtype GlobalChan = GlobalChan ChMQueues
  deriving Show

{-
instance Show GlobalChan where
    show _ = "GlobalChan _"
-}

{- | This data keeps track of the polarity of the channel. This is necessary for 
 - the channel to know which part of the queue it should put its commands on. -}
data Polarity
    = Output
    | Input
  deriving Show


type LocalChanToLocalChanMapping = Map LocalChan LocalChan

-- | Type alias to map a local channel to its polarity and appropriate
-- polarity and queue (a mapping to the global channel manager)
type Translation = Map LocalChan TranslationLkup

{-| the look up result looking up a 'LocalChan'. -}
data TranslationLkup 
    = InputLkup 
        -- ^ the channel looked up is of __input__ polarity
        { _activeQueue :: ChMQueue
            -- ^ the queue for which this channel should put stuff on (the input queue)
        , _otherQueue :: ChMQueue
            -- ^ the queue for which this channel should NOT put stuff on (the output queue)
        }
    | OutputLkup
        -- ^ the channel looked up is of __output__ polarity
        { _activeQueue :: ChMQueue
            -- ^ the queue for which this channel should put stuff on (the output queue)
        , _otherQueue :: ChMQueue
            -- ^ the queue for which this channel should NOT put stuff on (the input queue)
        }
  deriving Show


{-
showTranslationLkup :: 
    TranslationLkup -> 
    IO _
showTranslationLkup lkup = do
    (actinsrs, othinstrs) <- atomically $ do
        actq <- readTVar (coerce @ChMQueue @(TVar (TQueue QInstr)) act)
        actres <- flushTQueue actq
        for (reverse actres) (unGetTQueue actq)

        othq <- readTVar (coerce @ChMQueue @(TVar (TQueue QInstr)) oth)
        othres <- flushTQueue othq
        for (reverse othres) (unGetTQueue othq)

        return (actres, othres)
    
    return $ case lkup of
        InputLkup _ _ -> (actinsrs, othinstrs)
        OutputLkup _ _ -> (actinsrs, othinstrs)

        -- InputLkup _ _ -> concat [ "InputLkup", " ", show (actinsrs, othinstrs)]
        -- OutputLkup _ _ -> concat [ "OutputLkup", " ", show (actinsrs, othinstrs)]
  where
    act = _activeQueue lkup 
    oth = _otherQueue lkup 
-}


-- function / process

-- * data type for the machine
data Stec = Stec 
    { _stack :: ![Val]
    -- | LocalChan --> (Polarity, ChMQueue). 
    , _translation :: !Translation
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

-- we reserve some special indices for services..
-- N.B. Long deprecated.. we now have specific instrucitons.. 
{-
pattern SGetHCaseIx = HCaseIx 0 
pattern SPutHCaseIx = HCaseIx 1 
pattern SCloseHCaseIx = HCaseIx 2 
-}

-- * Instruction types for the machine
data Instr
    = ConcInstr IConc
    | SeqInstr ISeq
  deriving Show

data IConc
    = IGet LocalChan
    | IPut LocalChan

    -- | split a into (b, c)
    | ISplit LocalChan (LocalChan, LocalChan)
    -- | fork a into (b with bs, c with cs) 
    | IFork LocalChan 
        ( (LocalChan, Set LocalChan, [Instr]) 
        , (LocalChan, Set LocalChan, [Instr]) 
        )

    | IClose LocalChan
    | IHalt LocalChan

    | IId LocalChan LocalChan

    -- We have [a1, .., a_n], and by convention (i.e., Prashant) we have (out channel, in channel)
    | IPlug [LocalChan] (((Set LocalChan, Set LocalChan), [Instr]), ((Set LocalChan, Set LocalChan), [Instr]))


    -- | Translation mappings, FunctionID and number of arguments to call with this function..
    | IRun LocalChanToLocalChanMapping CallIx Int

    | IHPut LocalChan HCaseIx

    -- | HPut for services
    | ISHPut LocalChan SInstr 

    | IHCase LocalChan (Array HCaseIx [Instr])

    | IRace [(LocalChan, [Instr])]
  deriving Show


data ISeq 
    -- Basic insturctions...
    = IStore
    -- | access the n'th element in the stack
    | IAccess Int
    | IRet
    -- | Call function CallIx, and number of things in the enironment to keep
    -- (the arugments)
    | ICall CallIx Int

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
    | IEqBool
    -- | if then [Instr] else [Instr]
    | IIf [Instr] [Instr]

    -- Tests if two Vals are equal or less than or equal to
    | IEqInt
    | ILeqInt
    | ILtInt

    | IEqChar
    | ILeqChar

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
    = VClos ![Instr] ![Val]
    | VInt !Int
    | VBool !Bool
    | VChar !Char
    | VTuple !(Array TupleIx Val)

    | VCons !CaseIx ![Val]
    | VRec !(Array CaseIx [Instr]) [Val]
  deriving Show


{- |
N.B. Note we make some assumptoins as given in the
assembler -- lists have 

    - Empty as ix 0
    - Cons as ix 1

Unfortunately, this is a bit hard coded....
-}
strToVal :: String -> Val
strToVal = foldr (\c acc -> VCons (CaseIx 1) [VChar c, acc]) (VCons (CaseIx 0) [])


newtype IllegalString = IllegalString String 

instance Show IllegalString where
    show (IllegalString str) = "IllegalString: " ++ str
instance Exception IllegalString where

{- |
This makes similar assumptions from 'strToVal'. Morever, this throws an 'IllegalString' error 
if it cannot turn the value into a string (as given in the assumptions of 'strToVal').
-}
valToStr :: Val -> String
valToStr val = go val
  where
    go (VCons _ [VChar c, acc]) =  c : go acc
    go (VCons _ []) =  []
    go bad = throw $ IllegalString $ PrettyShow.ppShow val

{- | Corresponds to the hardcoded unit from the front end
-}
unitVCons :: Val
unitVCons = VCons (CaseIx 0) []
    


newtype MplMachSuperCombinators = MpMachSuperCombinators {
        _supercombinators :: Array CallIx [Instr]
    }
 deriving Show

-- * Instruction types for channel manager
data QInstr 
    = QGet Stec
    | QPut Val
    | QSplit GlobalChan GlobalChan
    -- | Notes:
    -- The 'LocalChan' should be added to the translation of 'Stec' immediately when 
    -- matched with the split, and the @t@ part of 'Stec' is restricted ONLY to the
    -- @withs@ given in the fork phrase (although this is unnecessary).
    | QFork 
        (LocalChan, Stec)
        (LocalChan, Stec)

    | QId GlobalChan

    -- this is not needed, since we just build open the new channels immediately in the 
    -- channel manager
    | QPlug !Void

    | QClose 
    | QHalt 

    -- not needed, since a proces can just call run immmediately
    | QRun !Void 

    | QHPut HCaseIx
    | QHCase Stec (Array HCaseIx [Instr])

    -- special QHPut for services
    | QSHPut SInstr

    -- | other channels to race, stec
    -- old definition was 'QRace [LocalChan] Stec', but now we just make them
    -- all share one election object.
    | QRace Stec
  deriving Show

{- | a service instruction -}
data SInstr 
    = SHGetChar
    | SHPutChar

    | SHGetString
    | SHPutString

    | SHGetInt
    | SHPutInt

    | SHOpenTerm
    | SHOpenThread

    | SHTimeOut

    | SHForkNegStringTerm

    | SHClose
  deriving Show

{- | Negative chs are service channels -}
isServiceCh :: 
    LocalChan ->
    Bool
isServiceCh = (<= -1) . coerce @LocalChan @Int

{- | Negative chs are service channels -}
isInputServiceCh :: 
    LocalChan ->
    Bool
isInputServiceCh n = even n' && n' <= -1
  where
    n' = coerce @LocalChan @Int n 


-- * Template haskell
$(makeClassyPrisms ''Val)
$(makeClassyPrisms ''IConc)
$(makeClassyPrisms ''ISeq)
$(makePrisms ''Instr)
$(makeLenses ''Stec)
$(makeLenses ''ChMQueues)
$(makeLenses ''TranslationLkup)
$(makePrisms ''TranslationLkup)
$(makeClassy ''MplMachSuperCombinators)
$(makeLenses ''ChMQueue)
$(makePrisms ''ChMQueueChain)


instance AsISeq Instr where
    _ISeq = _SeqInstr

instance AsIConc Instr where
    _IConc = _ConcInstr

