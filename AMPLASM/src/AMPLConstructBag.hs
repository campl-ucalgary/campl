{-# LANGUAGE DeriveDataTypeable #-}
module AMPLConstructBag where

import Language.ParAMPL
import Language.LexAMPL
import Language.AbsAMPL
import Language.ErrM
import Language.LayoutAMPL

import AMPLAST

import Data.Data
import Data.Tuple
import Data.Maybe
import Data.Coerce
import Data.Typeable
import Text.Read ( readMaybe )

import AMPLTypes
import Data.Stream (Stream)
import qualified Data.Stream as Stream


-- | A stream to help generate the index identifiers for anything with consix
consIxStream :: Stream ConsIx
consIxStream = coerce AMPLTypes.wordStream

hcaseIxStream :: Stream HCaseIx
hcaseIxStream = coerce AMPLTypes.wordStream

localChanIDStream :: Stream LocalChanID
localChanIDStream = Stream.iterate succ (LocalChanID 0)

-- | A stream to help generate the index identifiers for anything with consix
desIxStream :: Stream ConsIx
desIxStream = coerce AMPLTypes.wordStream

type ProtocolInfo = (RowColPos, [(Ident, HCaseIx)])
type CoprotocolInfo = (RowColPos, [(Ident, HCaseIx)])
type DataInfo = (RowColPos, [(Ident, (ConsIx, Word))])
type CodataInfo = (RowColPos, [(Ident, (DesIx, Word))])
type ProcessInfo a = (RowColPos, ([Ident], [(Ident, LocalChanID)], [(Ident, LocalChanID)], a))
type FunctionInfo a = (RowColPos, ([Ident], a))
type MainInfo = Maybe (Ident, (([(Ident, LocalChanID)], [(Ident, LocalChanID)]), [ACom]))
        -- ^ Main function (name, ((input channels, output channels), instructions))
        -- Note that name will always be %run when parsed by BNFC

data AmplAsmBag = AmplAsmBag {
    amplImports :: [(FilePath, RowColPos)]
    , amplMainInfo :: MainInfo
    , amplConstructsBag :: AmplConstructsBag
}
        -- ^ (imports info, MainInfo, AmplConstructsBag)


-- | A bag of all the constructs in a program
data AmplConstructsBag = AmplConstructsBag {
    protocolInfo :: [(String, ProtocolInfo)]
        -- ^ (Protocol name, (Handle , Handle index) )
    , coprotocolInfo :: [(String, CoprotocolInfo)]
        -- ^ (Cohandle name, (Cohandle , cohandle index) )
    , dataInfo :: [(String, DataInfo)]
        -- ^ (data name, (data constructor, (sumtype info, number of args) ))
    , codataInfo :: [(String, CodataInfo)]
        -- ^ (codata name, (codatadestructor, (sumtype info, number of args) ))
    , processInfo :: [(String, ProcessInfo [ACom])]
        -- ^ (Vars, input channel, output channel, coms)
    , functionInfo :: [(String, FunctionInfo [ACom])]
        -- ^ (function name, (sequential input vars, instructions))
    } deriving (Show, Eq)

emptyAmplConstructsBag = AmplConstructsBag [] [] [] []

collectSymbols :: 
    -- | AMPLCODE from bnfc
    AMPLCODE ->                         
    AmplAsmBag
collectSymbols (Main constructs start) = 
    AmplAsmBag imports prgmain $ AmplConstructsBag handles cohandles datas codatas processes functions
  where
    -- imports
    imports = mapMaybe importHelper constructs

    importHelper :: AMPL_CONSTRUCTS -> Maybe (FilePath, RowColPos)
    importHelper (IMPORT_CONSTRUCT (Import (IIdent n))) = Just (swap n)
    importHelper _ = Nothing

    -- Handles / cohandles... 
    handles = concat $ mapMaybe handleHelper constructs
    handleHelper :: AMPL_CONSTRUCTS -> Maybe [(String, ProtocolInfo)]
    handleHelper (HANDLE_CONSTRUCT (Handles specs)) = Just $ handleSpecHelper specs
    handleHelper _ = Nothing

    cohandles = concat $ mapMaybe cohandleHelper constructs
    cohandleHelper :: AMPL_CONSTRUCTS -> Maybe [(String, CoprotocolInfo)]
    cohandleHelper (COHANDLE_CONSTRUCT (Cohandles specs)) = Just $ handleSpecHelper specs
    cohandleHelper _ = Nothing
    
    handleSpecHelper :: [HANDLE_SPEC] -> [(String, ProtocolInfo)]
        -- Recall that ProtocolInfo == CoprotocolInfo
    handleSpecHelper = map f 
      where
        f (Hand_spec (UIdent (rowcol, name)) handles) = 
            (name
            , (rowcol, zip (map (\(HandName id) -> uIdentToIdent id) handles) (Stream.toList hcaseIxStream)))

    -- data / codata...
    datas = concat $ mapMaybe dataHelper constructs

    dataHelper :: AMPL_CONSTRUCTS -> Maybe [(String, DataInfo)]
    dataHelper (CONSTRUCTOR_CONSTRUCT (Constructors specs)) = Just $ coerce $ structorSpecHelper specs
    dataHelper _ = Nothing

    codatas = concat $ mapMaybe codataHelper constructs

    codataHelper :: AMPL_CONSTRUCTS -> Maybe [(String, CodataInfo)]
    codataHelper (DESTRUCTOR_CONSTRUCT (Destructors specs)) = Just $ coerce $ structorSpecHelper specs
    codataHelper _ = Nothing

    structorSpecHelper :: [STRUCTOR_SPEC] -> [(String, (RowColPos, [(Ident, (Word, Word))]))]
    structorSpecHelper = map f
      where
        f (Struct_spec (UIdent (rowcol, name)) structs) = 
            ( name
            , (rowcol, zipWith (\(id,args) ix -> (id, (ix, args)))
                (map (\(Struct id args) -> (uIdentToIdent id, pIntegerToWord args)) structs) 
                (Stream.toList AMPLTypes.wordStream)
                ))

    -- Processes
    processes = concat $ mapMaybe processHelper constructs

    processHelper :: AMPL_CONSTRUCTS -> Maybe [(String, ProcessInfo [ACom])]
    processHelper (PROCESSES_CONSTRUCT (Processes specs)) = Just $ map f specs
      where 
        f :: PROCESS_SPEC -> (String, ProcessInfo [ACom])
        f (Process_spec (PIdent (rowcol,name)) sequential inchs outchs (Prog coms)) =
            ( name
            ,   (rowcol, 
                    ( map (pIdentToIdent . (\(VName n)-> n)) sequential
                    , zip (map pIdentToIdent inchs) (Stream.toList localChanIDStream)
                    , zip (map pIdentToIdent outchs) (drop (length inchs) (Stream.toList localChanIDStream))
                    , map translateCOMToACom coms)
                )
            )
    processHelper _ = Nothing

    -- Functions.
    functions = concat $ mapMaybe functionsHelper constructs

    functionsHelper :: AMPL_CONSTRUCTS -> Maybe [(String, FunctionInfo [ACom])]
    functionsHelper (FUNCTIONS_CONSTRUCT (Functions specs)) = Just $ map f specs
      where
        f :: FUNCTION_SPEC -> (String, FunctionInfo [ACom])
        f (Function_spec (PIdent (rowcol, name)) vars (Prog coms)) = 
            ( name
            , (rowcol, (map (pIdentToIdent . \(VName n) -> n) vars, map translateCOMToACom coms)))

    functionsHelper _ = Nothing

    prgmain = case start of
        Start (Main_run (rowcol, name)) (Channel_spec inschs outchs) (Prog coms) -> Just 
                ( (name, rowcol)
                , (
                    ( zip (map pIdentToIdent inschs) (Stream.toList localChanIDStream)
                    , zip (map pIdentToIdent outchs) (drop (length inschs) (Stream.toList localChanIDStream)) )
                  , map translateCOMToACom coms 
                  ) 
                )
        Start_none -> Nothing
