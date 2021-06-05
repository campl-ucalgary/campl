module Bytecode where
{-
import Data.Array
import Data.List
import Data.Function

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.Bits

import AMPL
import AMPLSequential 
import AMPLConcurrent
import AMPLTypes
import AMPLServices
import AMPLEnv
import AMPLMach
import AMPLLogger

import Data.Stream (Stream)
import qualified Data.Stream as Stream
import Data.Queue (Queue)
import qualified Data.Queue as Queue


import ServiceConstants
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import System.Environment
import System.IO

import Data.Word
import Data.Int

-- Definition of the byte code commands
-- DO NOT CHANGE THE ORDER, THIS MUST MATCH THE C++ ENUM DECLARATION
data BytecodeCommand = 
      B_AM_DONE 
    | B_AM_STOR 
    | B_AM_STOR_N
    | B_AM_UNSTOR_N
    | B_AM_LOAD 
    | B_AM_CALL 
    | B_AM_RET 
    | B_AM_CONS
    | B_AM_CASE 
    | B_AM_REC  
    | B_AM_DEST
    | B_AM_CONST
    | B_AM_ADD
    | B_AM_SUB
    | B_AM_MUL
    | B_AM_DIV
    | B_AM_LE
    | B_AM_LT
    | B_AM_GE
    | B_AM_GT
    | B_AM_EQ
    | B_AM_LNOT
    | B_AM_LAND
    | B_AM_LOR
    | B_AM_BAND
    | B_AM_BOR
    | B_AM_IF
    | B_AM_JMP
-- Concurrent Commands
    | B_AM_GET
    | B_AM_PUT
    | B_AM_SPLIT
    | B_AM_FORK
    | B_AM_PLUG
    | B_AM_HPUT
    | B_AM_HCASE
    | B_AM_RUN
    | B_AM_CLOSE
    | B_AM_HALT
    | B_AM_HALT_N
    | B_AM_ID
    | B_AM_RACE
    | B_AM_ERROR
    deriving (Enum, Show, Eq)

data BytecodeEntry =
      BI BytecodeCommand
    | BChID LocalChanID
    | BGlobalChID GlobalChanID
    | BServiceDataType ServiceDataType
    | BServiceType ServiceType
    | BPolarity Polarity
    | BU Word64
    | BIdx Word64
    | BCellU Word64
    | BCellS Int64
    | BFunID FunID
    | BJ Int
    | BHeader Word64
    deriving (Show)


bytecodeEntryToByteString :: BytecodeEntry -> Builder
bytecodeEntryToByteString (BI cmd) = word64LE $ fromIntegral $ fromEnum cmd
bytecodeEntryToByteString (BChID (LocalChanID id)) = int64LE $ fromIntegral $ id
bytecodeEntryToByteString (BGlobalChID (GlobalChanID id)) = int64LE $ fromIntegral $ id
bytecodeEntryToByteString (BServiceDataType IntService) = word64LE $ fromIntegral 0
bytecodeEntryToByteString (BServiceDataType CharService) = word64LE $ fromIntegral 1
bytecodeEntryToByteString (BServiceType StdService) = word64LE $ fromIntegral 0
bytecodeEntryToByteString (BServiceType (NetworkedService _)) = word64LE $ fromIntegral 1
bytecodeEntryToByteString (BServiceType (TerminalNetworkedService _ _)) = word64LE $ fromIntegral 2
bytecodeEntryToByteString (BPolarity Output) = word64LE $ fromIntegral 1
bytecodeEntryToByteString (BPolarity Input) = word64LE $ fromIntegral 2
bytecodeEntryToByteString (BU v) =  word64LE v
bytecodeEntryToByteString (BIdx v) =  word64LE $ v
bytecodeEntryToByteString (BCellU cell) = error "Cells cannot be converted to bytestrings (translate to word64)"
bytecodeEntryToByteString (BCellS cell) = error "Cells cannot be converted to bytestrings (translate to word64)"
bytecodeEntryToByteString (BFunID (FunID _)) = error "FunID cannot be converted to bytestrings (translate to jumps)"
bytecodeEntryToByteString (BJ loc) = int64LE $ fromIntegral loc
bytecodeEntryToByteString (BHeader word) = word64BE word

translateBytecodeToByteString :: [BytecodeEntry] -> Builder
translateBytecodeToByteString (head:rest) = mappend (bytecodeEntryToByteString head) (translateBytecodeToByteString rest)
translateBytecodeToByteString [] = mempty

-- emitted code, current position, function locations
type BytecodeState = ([(Int, BytecodeEntry)], Int, [(Int, FunID)])

-- start locations at 1
initBytecodeState :: BytecodeState
initBytecodeState = ([], 0, [])


valToBytecodeEntry :: Val -> BytecodeEntry
valToBytecodeEntry (VInt v)  = BCellS (fromIntegral v)
valToBytecodeEntry (VBool v) = BCellU (if v then 1 else 0)
valToBytecodeEntry (VChar v) = BCellU (fromIntegral $ fromEnum v)
valToBytecodeEntry _ = undefined

-- Location to emit to -> Entry -> Emitted location
emitAt :: Int -> BytecodeEntry -> State BytecodeState Int
emitAt loc entry = do
    modify (\(c, l, f) -> ( (loc, entry):c, l, f) )
    return loc
    
-- Entry -> Emitted Location
emit :: BytecodeEntry -> State BytecodeState Int
emit entry = do
    loc <- location
    emitAt loc entry
    skip
    return loc

-- Current location
location :: State BytecodeState Int
location = do
    (_, loc, _) <- get
    return loc

-- Location that was skipped
skip :: State BytecodeState Int 
skip = do
    l <- location 
    modify (\(c, l, f) -> (c, l + 1, f))
    return l

skipk :: Int -> State BytecodeState Int
skipk k = do
    l <- location
    modify (\(c, l, f) -> (c, l + k, f))
    return l

lookupFunction :: [(Int, FunID)] -> FunID -> Int
lookupFunction fs funID = let 
        res = find (\(_, id) -> id == funID) fs
    in
        case res of Just (loc, _)  -> loc
                    Nothing -> error "funID not found"

insertFunction :: Int -> FunID -> State BytecodeState ()
insertFunction loc funID = 
    modify (\(c, l, fs) -> (c, l, (loc, funID):fs))

getBytecode :: State BytecodeState [BytecodeEntry]
getBytecode = do
    (bytecode, _, _) <- get
    return $ map snd $ sortBy (compare `on` fst) bytecode

getFunctionTable :: State BytecodeState [(Int, FunID)]
getFunctionTable = do
    (_, _, fs) <- get
    return fs

translateBlock :: [Instr] -> State BytecodeState Int
translateBlock [] = location
translateBlock (inst:rest) = do
    ret <- translateInstr inst
    translateBlock rest
    return ret

translateInstr :: Instr -> State BytecodeState Int
translateInstr (SequentialInstr i) = translateSequential i
translateInstr (ConcurrentInstr i) = translateConcurrent i

translateSequential :: SequentialInstr -> State BytecodeState Int
translateSequential (IStore) = emit $ BI B_AM_STOR

translateSequential (IAccess location) = do 
    ret <- emit $ BI B_AM_LOAD
    emit $ BIdx $ (fromIntegral location)
    return ret

translateSequential (IRet) = emit $ BI B_AM_RET

translateSequential (ICall funid num) = do
    ret <- emit $ BI B_AM_STOR_N
    emit $ BU $ fromIntegral num
    emit $ BI B_AM_CALL
    emit $ BFunID funid
    emit $ BI B_AM_UNSTOR_N
    emit $ BU $ fromIntegral num
    return ret

translateSequential (IConst val) = do
    ret <- emit $ BI B_AM_CONST
    emit $ valToBytecodeEntry val
    return ret

translateSequential (IAddInt)  = emit $ BI B_AM_ADD
translateSequential (ISubInt)  = emit $ BI B_AM_SUB
translateSequential (IMulInt)  = emit $ BI B_AM_MUL
translateSequential (IDivInt)  = emit $ BI B_AM_DIV
translateSequential v@(IModInt)  = error $ "Unimplemented instruction: " ++ (show v)
translateSequential (IOrBool)  = emit $ BI B_AM_LOR
translateSequential (IAndBool) = emit $ BI B_AM_LAND

translateSequential (IIf trueCase falseCase) = do
    ret <- emit $ BI B_AM_IF
    trueLoc <- skip
    falseLoc <- skip
    emit $ BI B_AM_JMP
    afterInstructionLoc <- skip
    trueTarget <- translateBlock trueCase
    falseTarget <- translateBlock falseCase
    emitAt trueLoc $ BJ trueTarget
    emitAt falseLoc $ BJ falseTarget

    loc <- location
    emitAt afterInstructionLoc $ BJ loc
    return ret

translateSequential (IEq ) = emit $ BI B_AM_EQ
translateSequential (ILeq) = emit $ BI B_AM_LE


translateSequential (ICons (CaseIx consIdx) nargs) = do
    ret <- emit $ BI B_AM_CONS
    emit $ BIdx $ fromIntegral consIdx
    emit $ BU $ fromIntegral nargs
    return ret


translateSequential (ICase casesArray) = let 
        caseList = elems casesArray
        caseListLen = length caseList
    in do
        ret <- emit $ BI B_AM_CASE
        start <- skipk $ caseListLen 
        emit $ BU $ fromIntegral 0
        emit $ BI B_AM_JMP
        afterInstructionLoc <- skip
        genCaseList start False caseList

        loc <- location
        emitAt afterInstructionLoc $ BJ $ fromIntegral loc
        return ret


translateSequential (IRec entryInstructions) = let 
        caseList = elems entryInstructions
        caseListLen = length caseList
    in do
        ret <- emit $ BI B_AM_REC
        start <- skipk caseListLen
        emit $ BU $ fromIntegral 0
        emit $ BI B_AM_JMP
        afterInstructionLoc <- skip
        genCaseList start False caseList

        loc <- location
        emitAt afterInstructionLoc $ BJ $ fromIntegral loc
        return ret


translateSequential (IDest (CaseIx idx) nargs) = do
        ret <- emit $ BI B_AM_DEST
        emit $ BIdx $ fromIntegral idx
        emit $ BU $ fromIntegral nargs
        return ret

translateSequential (IErrorMsg _) = emit $ BI B_AM_ERROR

translateSequential v@(IUnstring)    = error $ "Unimplemented instruction: " ++ (show v)
translateSequential v@(IConcat)      = error $ "Unimplemented instruction: " ++ (show v)
translateSequential v@(IConcats _)   = error $ "Unimplemented instruction: " ++ (show v)
translateSequential v@(IToString)    = error $ "Unimplemented instruction: " ++ (show v)
translateSequential v@(IToInt)       = error $ "Unimplemented instruction: " ++ (show v)
translateSequential v@(IAppend)      = error $ "Unimplemented instruction: " ++ (show v)
translateSequential v@(ITuple _)     = error $ "Unimplemented instruction: " ++ (show v)
translateSequential v@(ITupleElem _) = error $ "Unimplemented instruction: " ++ (show v)

genCaseList :: Int -> Bool -> [[Instr]] -> State BytecodeState ()
genCaseList idx addDone (instBlk:rest) = do
    target <- translateBlock instBlk
    emitAt idx $ BJ target
    if addDone
        then emit $ BI B_AM_DONE 
        else return 0
    genCaseList (idx + 1) addDone rest
    return ()
genCaseList _ _ [] = return ()

translateConcurrent :: ConcurrentInstr -> State BytecodeState Int
translateConcurrent (IGet id) = do 
    ret <- emit $ BI B_AM_GET
    emit $ BChID id
    return ret

translateConcurrent (IPut id) = do
    ret <- emit $ BI B_AM_PUT
    emit $ BChID id
    return ret

translateConcurrent (ISplit tID (id1, id2)) = do
    ret <- emit $ BI B_AM_SPLIT
    emit $ BChID tID
    emit $ BChID id1
    emit $ BChID id2
    return ret

translateConcurrent (IFork 
    target
    (
        (ch1, ch1Table, ch1Code), 
        (ch2, ch2Table, ch2Code))) = let
    in do
        ret <- emit $ BI B_AM_FORK
        
        emit $ BChID target

        ch1JmpLoc <- skip  -- code 1
        emit $ BChID ch1   -- ch1 name
        translateChannelList ch1Table
        
        ch2JmpLoc <- skip  -- code 2
        emit $ BChID ch2   -- ch2 name
        translateChannelList ch2Table
        
        -- no code comes after fork
        emit $ BI B_AM_DONE 

        ch1Target <- translateBlock ch1Code
        emitAt ch1JmpLoc $ BJ ch1Target
        emit $ BI B_AM_DONE 

        ch2Target <- translateBlock ch2Code
        emitAt ch2JmpLoc $ BJ ch2Target
        emit $ BI B_AM_DONE 

        return ret

translateConcurrent (IClose localChanID) = do
    ret <- emit $ BI B_AM_CLOSE
    emit $ BChID localChanID
    return ret
    
translateConcurrent (IHalt localChanIDs) = do
    ret <- emit $ BI B_AM_HALT_N
    translateChannelList localChanIDs
    emit $ BI B_AM_DONE
    return ret

translateConcurrent (IId lID rID) = do
    ret <- emit $ BI B_AM_ID
    emit $ BChID lID
    emit $ BChID rID
    return ret

translateConcurrent (IPlug newChannels ((p1Chs, p1Code), (p2Chs, p2Code))) = do
    ret <- emit $ BI B_AM_PLUG

    p1CodeLoc <- skip
    p2CodeLoc <- skip

    translateChannelList newChannels
    translateChannelList p1Chs
    translateChannelList p2Chs
    
    -- jump over the cases code blocks

    emit $ BI B_AM_JMP
    afterInstructionLoc <- skip

    p1CodeTarget <- translateBlock p1Code
    emit $ BI B_AM_DONE 

    p2CodeTarget <- translateBlock p2Code
    emit $ BI B_AM_DONE 

    emitAt p1CodeLoc $ BJ $ fromIntegral p1CodeTarget
    emitAt p2CodeLoc $ BJ $ fromIntegral p2CodeTarget

    loc <- location
    emitAt afterInstructionLoc $ BJ $ fromIntegral loc

    return ret

translateConcurrent (IRun localTrans fun nargs) = let
        translateMapping ((pol, (ch1, ch2)):rest) = do
            emit $ BPolarity pol
            emit $ BChID ch1
            emit $ BChID ch2
            translateMapping rest
        translateMapping [] = emit $ BU $ fromIntegral 0
    in do
        ret <- emit $ BI B_AM_STOR_N
        emit $ BU $ fromIntegral nargs

        emit $ BI B_AM_RUN
        emit $ BFunID fun
        translateMapping localTrans

        emit $ BI B_AM_UNSTOR_N
        emit $ BU $ fromIntegral nargs

        return ret

translateConcurrent (IHPut chID (HCaseIx caseIdx)) = do
    ret <- emit $ BI B_AM_HPUT
    emit $ BChID chID
    emit $ BIdx $ fromIntegral caseIdx
    return ret

translateConcurrent (IHCase chID caseArr) = let
        caseList = elems caseArr
        caseListLen = length caseList
    in do
        ret <- emit $ BI B_AM_HCASE
        emit $ BChID chID
        start <- skipk caseListLen
        emit $ BU $ fromIntegral 0
        emit $ BI B_AM_JMP
        afterInstructionLoc <- skip
        genCaseList start True caseList

        loc <- location
        emitAt afterInstructionLoc $ BJ loc
        return ret

translateConcurrent (IRace ((a, ac):(b, bc):[])) = do
    ret <- emit $ BI B_AM_RACE
    emit $ BChID a
    aLoc <- skip
    emit $ BChID b
    bLoc <- skip

    aBlockLoc <- translateBlock ac
    emit $ BI B_AM_DONE

    bBlockLoc <- translateBlock bc
    emit $ BI B_AM_DONE

    emitAt aLoc $ BJ aBlockLoc
    emitAt bLoc $ BJ bBlockLoc
    return ret

translateConcurrent (IRace _) = error "Only 2 way races are supported"


translateChannelList :: [LocalChanID] -> State BytecodeState ()
translateChannelList (head:rest) = do
    emit $ BChID head
    translateChannelList rest
translateChannelList [] = do
    emit $ BU $ fromIntegral 0 
    return ()

translateFun :: [Instr] -> State BytecodeState Int
translateFun instr = do
    ret <- translateBlock instr
    emit $ BI B_AM_DONE
    return ret

translateServiceTable :: ([GlobalChanID], [(GlobalChanID, (ServiceDataType, ServiceType))]) -> State BytecodeState (Int, Int)
translateServiceTable (internalServices, externalServices) = let
        translateInternal :: [GlobalChanID] -> State BytecodeState ()
        translateInternal (head:rest) = do
            emit $ BGlobalChID head
            translateInternal rest
            return ()
        translateInternal [] = return ()

        translateExternal :: [(GlobalChanID, (ServiceDataType, ServiceType))] -> State BytecodeState ()
        translateExternal ((id, (serviceDataType, serviceType)):rest) = do
            emit $ BGlobalChID id
            emit $ BServiceDataType serviceDataType
            emit $ BServiceType serviceType
            translateExternal rest
            return ()
        translateExternal [] = return ()
    in do
        internalRet <- location
        translateInternal internalServices
        externalRet <- location
        translateExternal externalServices
        return (internalRet, externalRet)

translateMainTranslation :: [Translation] -> State BytecodeState Int
translateMainTranslation list = let 
    translateMainTranslation' ((pol, (localID, globalID)):tail) = do
        emit $ BPolarity pol
        emit $ BChID localID
        emit $ BGlobalChID globalID
        translateMainTranslation' tail
        return ()
    translateMainTranslation' [] = return ()
    in do
        ret <- location
        translateMainTranslation' list
        return ret

translateMachineState :: InitAMPLMachState -> ([BytecodeEntry], [(Int, FunID)])
translateMachineState state = let
        patchFunctionLocations :: [(Int, FunID)] -> BytecodeEntry -> BytecodeEntry
        patchFunctionLocations fs (BFunID id) = BJ $ lookupFunction fs id
        patchFunctionLocations _ v = v

        getLowestChannelId :: [BytecodeEntry] -> Int
        getLowestChannelId bytecode = let
                lowestChannelId :: BytecodeEntry -> Int -> Int
                lowestChannelId (BChID (LocalChanID id)) num = min id num
                lowestChannelId (BGlobalChID (GlobalChanID id)) num = min id num
                lowestChannelId _  num = num
            in do
                foldr lowestChannelId 0 bytecode

        incrementBytecodeIndex :: Int ->  BytecodeEntry -> BytecodeEntry
        incrementBytecodeIndex amount (BIdx idx) = BIdx $ idx + (fromIntegral amount)
        incrementBytecodeIndex amount v = v

        incrementChannelId :: Int -> BytecodeEntry -> BytecodeEntry
        incrementChannelId amount (BChID (LocalChanID id)) = BChID $ LocalChanID $ id + amount
        incrementChannelId amount (BGlobalChID (GlobalChanID id)) = BChID $ LocalChanID $ id + amount
        incrementChannelId amount v = v

        convertCellToIntegers :: BytecodeEntry -> BytecodeEntry 
        convertCellToIntegers (BCellS value) = BU ((fromIntegral value) .|. ((fromIntegral 0x8) `shiftL` 60))
        convertCellToIntegers (BCellU value) = BU ((fromIntegral value) .|. ((fromIntegral 0x8) `shiftL` 60))
        convertCellToIntegers v = v

        translate :: InitAMPLMachState -> State BytecodeState ([BytecodeEntry], [(Int, FunID)])
        translate state = let 
                services = initAmplMachStateServices state
                (mainFun, mainTrans) = initAmplMachMainFun state
                funs = initAmplMachFuns state

                translateFunList ((id, (_, instr)):rest) = do
                    blockStart <- translateFun instr
                    insertFunction blockStart id
                    translateFunList rest
                translateFunList [] = return ()
            in do 
                -- ampl encoded in hex, padded to a word64
                emit $ BHeader 0x616d706c00000000 

                -- first for Cells describe the location of each of the parts of the program
                defaultChannelsJumpLoc <- skip 
                serviceChannelsJumpLoc <- skip
                mainFunctionTranslationLoc <- skip
                mainFunctionJumpLoc <- skip

                (defaultTarget, serviceTarget) <- translateServiceTable services
                emitAt defaultChannelsJumpLoc $ BJ defaultTarget
                emitAt serviceChannelsJumpLoc $ BJ serviceTarget
                
                mainFunctionTranslationTarget <- translateMainTranslation mainTrans
                emitAt mainFunctionTranslationLoc $ BJ mainFunctionTranslationTarget

                mainFunctionJumpTarget <- translateFun mainFun
                emitAt mainFunctionJumpLoc $ BJ mainFunctionJumpTarget

                translateFunList funs

                bytecode <- getBytecode
                fs <- getFunctionTable
                return (bytecode, fs)

        (bytecode, fs) = fst $ runState (translate state) initBytecodeState
        channelOffset = -(getLowestChannelId bytecode) + 1
    in (map ((patchFunctionLocations fs).(incrementBytecodeIndex 1).(incrementChannelId channelOffset).convertCellToIntegers) bytecode, fs)

printInitMachineStateToBytecodeFile :: String -> InitAMPLMachState -> IO ()
printInitMachineStateToBytecodeFile filename state = do
        let (translatedMachine, _) = translateMachineState state
        -- putStrLn $ formatBytecodeString "\n" translatedMachine

        handle <- openFile filename (WriteMode)
        let translatedByteString = translateBytecodeToByteString translatedMachine
        B.hPut handle $ BL.toStrict $ toLazyByteString $ translatedByteString
        hClose handle
        return ()


externalCharMach = InitAMPLMachState {
    initAmplMachStateServices = (
        [],
        [      
            (GlobalChanID 0,(CharService,StdService)),
            (GlobalChanID (-1),(CharService,TerminalNetworkedService "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k 15973771760003421084  ; read'" (Key "15973771760003421084")))
        ]), 
    initAmplMachMainFun = (
        [
            ConcurrentInstr (IHPut (LocalChanID 0) (HCaseIx 0)),
            ConcurrentInstr (IGet (LocalChanID 0)),
            SequentialInstr IStore,
            ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 1)),
            SequentialInstr (IAccess 0),
            ConcurrentInstr (IPut (LocalChanID 1)),
            ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 2)),
            ConcurrentInstr (IHPut (LocalChanID 0) (HCaseIx 2)),
            ConcurrentInstr (IClose (LocalChanID 0))
        ],
        [
            (Input,(LocalChanID 0,GlobalChanID 0)),
            (Output,(LocalChanID 1,GlobalChanID (-1)))
        ]), 
    initAmplMachFuns = [] }

formatBytecodeString :: String -> [BytecodeEntry] -> String
formatBytecodeString sep list = let
        formatBytecodeString' :: String -> [BytecodeEntry] -> Int -> String
        formatBytecodeString' _ [] _ = ""
        formatBytecodeString' sep (head:[]) count =  (show count) ++ ":\t" ++ (show head)
        formatBytecodeString' sep (head:rest) count = (show count) ++ ":\t" ++ (show head) ++ sep ++ (formatBytecodeString' sep rest (count + 1))
    in formatBytecodeString' sep list 0


initMachineStateToBytecodeString :: InitAMPLMachState -> String
initMachineStateToBytecodeString mach = let
        (bytecode, fs) = translateMachineState mach
    in ((formatBytecodeString "\n" bytecode) ++ "\n" ++ (show fs))
    -}
