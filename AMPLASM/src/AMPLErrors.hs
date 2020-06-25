{-# LANGUAGE MultiParamTypeClasses #-}
module AMPLErrors where

import AMPLCompileErrors
import AMPLSymbolTable

class HasParseAndLexError e where
    parseAndLexError :: String -> e

newtype ParseAndLexError = ParseAndLexError String 
  deriving Show

instance HasParseAndLexError ParseAndLexError where
    parseAndLexError = ParseAndLexError

data AssemblerErrors =
    AsmNoMainFunction
    | AsmBnfcError ParseAndLexError
    | AsmSymAmbiguousLookup SymAmbiguousLookup
    | AsmIllegalInstrCall (IllegalInstrCall AssemblerErrors)
    | AsmFunctionArityMismatch FunctionArityMismatch
    | AsmCasingOverMultipleDatasError CasingOverMultipleDatasError
    | AsmRecordOverMultipleCodatasError RecordOverMultipleCodatasError
    | AsmDataArityMismatch DataArityMismatch
    | AsmCodataArityMismatch CodataArityMismatch
    | AsmProcessArityMismatch ProcessArityMismatch
    | AsmHCasingOverMultipleDatasError HCasingOverMultipleDatasError
    | AsmPolarityMismatch PolarityMismatch
    | AsmFreeVarError FreeVarError
    | AsmFreeChannelError FreeChannelError
    | AsmCodataLookupError CodataLookupError
    | AsmCoprotocolLookupError CoprotocolLookupError
    | AsmDataLookupError DataLookupError
    | AsmProtocolLookupError ProtocolLookupError
    | AsmProcessLookupError ProcessLookupError
    | AsmFunctionLookupError FunctionLookupError
    | AsmHCaseArityMismatch HCaseArityMismatch
  deriving Show


instance HasParseAndLexError AssemblerErrors where
    parseAndLexError = AsmBnfcError . parseAndLexError

instance HasAmbiguousLookupError AssemblerErrors where
    symAmbiguousLookup = AMPLErrors.AsmSymAmbiguousLookup . symAmbiguousLookup

instance HasIllegalInstrCallError AssemblerErrors AssemblerErrors where
    illegalInstrCall a = AMPLErrors.AsmIllegalInstrCall . illegalInstrCall a

instance HasFunctionArityMismatchError AssemblerErrors where
    functionArityMismatch a = AMPLErrors.AsmFunctionArityMismatch . functionArityMismatch a

instance HasCasingOverMultipleDatas AssemblerErrors where
    casingOverMultipleDatas = AMPLErrors.AsmCasingOverMultipleDatasError . casingOverMultipleDatas

instance HasHCasingOverMultipleTypes AssemblerErrors where
    hcasingOverMultipleTypes = AMPLErrors.AsmHCasingOverMultipleDatasError . hcasingOverMultipleTypes

instance HasRecordOverMultipleCodatas AssemblerErrors where
    recordOverMultipleCodatas = AMPLErrors.AsmRecordOverMultipleCodatasError . recordOverMultipleCodatas
    
instance HasDataArityMismatchError AssemblerErrors where
    dataArityMismatch a = AMPLErrors.AsmDataArityMismatch . dataArityMismatch a
    
instance HasCodataArityMismatchError AssemblerErrors where
    codataArityMismatch a = AMPLErrors.AsmCodataArityMismatch . codataArityMismatch a
    
instance HasProcessArityMismatch AssemblerErrors where
    processArityMismatch a = AMPLErrors.AsmProcessArityMismatch . processArityMismatch a
    
instance HasPolarityMismatch AssemblerErrors where
    polarityMismatch a = AMPLErrors.AsmPolarityMismatch . polarityMismatch a
    
instance HasFreeVarError AssemblerErrors where
    freeVar = AMPLErrors.AsmFreeVarError . freeVar
    
instance HasFreeChannelError AssemblerErrors where
    freeChannel = AMPLErrors.AsmFreeChannelError . freeChannel
    
instance HasCodataLookupError AssemblerErrors where
    codataDoesNotExist = AMPLErrors.AsmCodataLookupError . codataDoesNotExist
    destructorDoesNotExist a = AMPLErrors.AsmCodataLookupError . destructorDoesNotExist a
    notCodata a = AMPLErrors.AsmCodataLookupError . notCodata a
    
instance HasCoprotocolLookupError AssemblerErrors where
     coprotocolDoesNotExist = AMPLErrors.AsmCoprotocolLookupError . coprotocolDoesNotExist 
     cohandleDoesNotExist a = AMPLErrors.AsmCoprotocolLookupError . cohandleDoesNotExist a
     notCoprotocol a = AMPLErrors.AsmCoprotocolLookupError . notCoprotocol a
    
instance HasDataLookupError AssemblerErrors where
     dataDoesNotExist = AMPLErrors.AsmDataLookupError . dataDoesNotExist
     constructorDoesNotExist a = AMPLErrors.AsmDataLookupError . constructorDoesNotExist a
     notData a = AMPLErrors.AsmDataLookupError . notData a
    
instance HasProtocolLookupError  AssemblerErrors where
     protocolDoesNotExist = AMPLErrors.AsmProtocolLookupError . protocolDoesNotExist 
     handleDoesNotExist a = AMPLErrors.AsmProtocolLookupError . handleDoesNotExist a
     notProtocol a = AMPLErrors.AsmProtocolLookupError . notProtocol a
    
instance HasLookupProcessError AssemblerErrors where
     processDoesNotExist = AMPLErrors.AsmProcessLookupError . processDoesNotExist
     notProcess a = AMPLErrors.AsmProcessLookupError . notProcess a
    
instance HasLookupFunctionError AssemblerErrors where
    functionDoesNotExist = AMPLErrors.AsmFunctionLookupError . functionDoesNotExist
    notFunction a = AMPLErrors.AsmFunctionLookupError . notFunction a

instance HasHCaseArityMismatch AssemblerErrors where
    hcaseArityMismatch a = AMPLErrors.AsmHCaseArityMismatch . hcaseArityMismatch a

