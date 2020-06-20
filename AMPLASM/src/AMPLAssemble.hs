{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module AMPLAssemble where 
import AMPL
import AMPLServices
import AMPLTypes
import AMPLCompile
import AMPLCompileErrors
import AMPLSymbolTable
import AMPLConstructBag

import Language.ParAMPLGrammar
import Language.LexAMPLGrammar
import Language.AbsAMPLGrammar
import Language.ErrM
import Language.LayoutAMPLGrammar

import Data.Stream (Stream)
import qualified Data.Stream as Stream

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

import Data.List
import Data.Foldable
import Data.Coerce
import Data.Tuple
import Data.List
import Data.Either
import Data.Function
import Control.Arrow
import qualified Data.Bifunctor as Bifunctor

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans

import System.IO
import System.Random
import System.Exit
import Debug.Trace

data AmplAssembleConfig = AmplAssembleConfig {
        filetocompile :: String
        , outputfile :: Maybe String
    }

amplAssembleMain :: 
    AmplAssembleConfig ->
    IO ()
amplAssembleMain AmplAssembleConfig{ filetocompile = filename, outputfile = outputfile} = do
    prg <- readFile filename
    servicestate <- defaultServiceState
    let eithercode = amplAssemble servicestate prg
    case eithercode of
        Right mach -> do
            handle <- maybe (pure stdout) (flip openFile ReadWriteMode) outputfile
            -- hPutStr handle $ render $ doc $ mach
            hPutStr handle $ show mach
            hClose handle
        Left errs -> hPutStrLn stderr (show errs) >> exitFailure


defaultServiceState :: IO ServiceState
defaultServiceState = do    
    keystream <- defaultKeyStream
    return $ ServiceState {
            keyStream = keystream
            , serviceGlobalChanIdGen = Stream.iterate pred (GlobalChanID 0)
            , internalGlobalChanIdGen = Stream.iterate succ (GlobalChanID 1)
            , terminalNetworkedCommand = (\(Key str) -> "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k " ++ str ++ "  ; read'")
        }

defaultKeyStream :: IO (Stream Key)
defaultKeyStream = do
    stdgen <- newStdGen
    let keys = map (Key . show) $ nub $ (randoms stdgen :: [Word])
    return $ Stream.fromList keys
    -- TODO -- this might generate the same key twicE!!

    

amplAssemble :: 
    ServiceState -> 
    String -> 
    Either [(Ident, [AssemblerErrors])] InitAMPLMachState
amplAssemble svstate str = case parseAndLex str of
    Ok code -> amplCodeToInstr svstate code 
    Bad str -> Left [(("", (-1,-1)), [parseAndLexError str])]

parseAndLex :: String -> Err AMPLCODE
parseAndLex = pAMPLCODE . resolveLayout True . myLexer

data ServiceState = ServiceState {
    keyStream :: Stream Key 
    , serviceGlobalChanIdGen :: Stream GlobalChanID
    , internalGlobalChanIdGen :: Stream GlobalChanID
    , terminalNetworkedCommand :: Key -> String
}

amplCodeToInstr :: 
    ServiceState ->
    AMPLCODE -> 
    Either 
        [(Ident, [AssemblerErrors])] 
        InitAMPLMachState
amplCodeToInstr servicegeneratorstate amplcode = 
    let (funerrs, funs) = (lefts functions, rights functions)
        (proerrs, pros) = (lefts protocols, rights protocols)
        errs = funerrs ++ proerrs
    in case cmainfun of
        Right ( (nonservicechs, servicechs) , (maininstrs, maintranslations) ) ->
            if null errs 
                then return $ InitAMPLMachState { 
                        initAmplMachStateServices = (nonservicechs, servicechs)
                        , initAmplMachMainFun = (maininstrs, maintranslations)
                        , initAmplMachFuns = funs ++ pros 
                    }
                else Left errs
        Left mainerrs -> Left ( mainerrs : errs)
  where
    (imports, mainfun, bag) = AMPLConstructBag.collectSymbols amplcode
    symboltable = AMPLSymbolTable.makeSymbolTable bag :: Map String (Either AssemblerErrors SymEntry)
    functions = map f (functionInfo bag)
      where
        f :: (String, FunctionInfo COMS) -> Either (Ident, [AssemblerErrors]) (FunID,(String, [Instr]))
        f (fname, (fpos, (args, Prog coms))) = do
            (_, (_, funid)) <- Bifunctor.first (((fname,fpos),) . pure) $ lookupFunction (fname, fpos) symboltable
            Bifunctor.bimap (\errs -> ((fname, fpos), errs)) (\(instrs, _) -> (funid, (fname,instrs)))
                $ compileRunner coms 
                    (CompileEnv { symbolTable = symboltable }) 
                    (CompileState { localVarStack = map fst args , channelTranslations = [] } )
    protocols = map f (processInfo bag)
      where
        f :: (String, ProcessInfo COMS) -> Either (Ident, [AssemblerErrors]) (FunID,(String, [Instr]))
        f (pname, (ppos, (args, inchs, outchs, Prog coms))) = do
            let intranslations = map (fst *** (Input,)) inchs
                outtranslations = map (fst *** (Output,)) outchs
            (_, (_, _, _, funid)) <- Bifunctor.first (((pname,ppos),) . pure) 
                                        $ lookupProcess (pname, ppos) symboltable
            Bifunctor.bimap (\errs -> ((pname, ppos), errs)) (\(instrs, _) -> (funid, (pname,instrs)))
                $ compileRunner coms 
                    (CompileEnv { symbolTable = symboltable }) 
                    (CompileState { localVarStack = map fst args , channelTranslations = intranslations ++ outtranslations } )
    cmainfun = case mainfun of
        Just (runident, ((inchs, outchs), Prog coms)) -> Bifunctor.first (runident,) $ do
            (maintranslations, nonservicechs, servicechs) <- Bifunctor.first pure $ runExcept $ flip evalStateT servicegeneratorstate $ do 
                        (intrans, ingchs, ingchssvs) <- getTranslationsInternalServiceChannelsAndExternalServiceChannels Input inchs
                        (outtrans, outgchs, outgchsvs) <- getTranslationsInternalServiceChannelsAndExternalServiceChannels Output outchs
                        return (intrans ++ outtrans, ingchs ++ outgchs, ingchssvs ++ outgchsvs)

            (maininstrs, _) <- compileRunner coms 
                            (CompileEnv { symbolTable = symboltable }) 
                            (CompileState { localVarStack = [] 
                                          , channelTranslations = map (fst *** (Input,)) inchs ++ map (fst *** (Output,)) outchs
                                          } )
            return ( (nonservicechs, servicechs) , (maininstrs, maintranslations) )
          where
        Nothing -> Left (("", (-1,-1)),[NoMainFunction])
                        -- unfortunately, we need to include a calling context Ident
                        -- so we just give this an ``empty" sorta context..

getTranslationsInternalServiceChannelsAndExternalServiceChannels :: 
    HasPolarityMismatch e => 
    Polarity ->
    [(Ident, LocalChanID)] ->
    StateT
        ServiceState 
        (Except e)
        ( [Translation]
        , [GlobalChanID]
        , [(GlobalChanID, (ServiceDataType, ServiceType))])
        -- ^ (machine translations, non service channels, service channels (GlobalChanID, (ServiceDataType, ServiceType)) )
getTranslationsInternalServiceChannelsAndExternalServiceChannels pol = foldrM f ([], [], [])
  where 
    f :: HasPolarityMismatch e => 
        (Ident, LocalChanID) -> 
        ([Translation], [GlobalChanID], [(GlobalChanID, (ServiceDataType, ServiceType))]) ->
        StateT ServiceState (Except e) ([Translation], [GlobalChanID], [(GlobalChanID, (ServiceDataType, ServiceType))])
    f (name, lch) (translations, nonservices, services) = do
        svstype <- getServiceChannelType pol name
        case svstype of
            Just (svdtype, svtype) -> do
                gch <- Stream.head <$> gets serviceGlobalChanIdGen
                modify (\s -> s { serviceGlobalChanIdGen = Stream.tail $ serviceGlobalChanIdGen s })
                return ( (pol, (lch, gch)) : translations, nonservices, (gch, (svdtype, svtype)) : services)

            Nothing -> do 
                gch <- Stream.head <$> gets internalGlobalChanIdGen
                modify (\s -> s { internalGlobalChanIdGen = Stream.tail $ internalGlobalChanIdGen s })
                return ((pol, (lch, gch)) : translations, gch : nonservices, services)
        

getServiceChannelType :: 
    HasPolarityMismatch e => 
    Polarity -> 
    Ident -> 
    StateT ServiceState (Except e) (Maybe (ServiceDataType, ServiceType))
        -- Nothing implies that this is not a service channel
getServiceChannelType pol name = do
    let isint = "int" `isPrefixOf` fst name
        ischar = "char" `isPrefixOf` fst name
        isintconsole = fst name == "console"
        ischarconsole = fst name == "cconsole"
    if | (isintconsole || ischarconsole) && pol == Output -> throwError $ polarityMismatch Input (name, Output)
       | (isint || ischar) && pol == Input -> throwError $ polarityMismatch Output (name, Input)
            -- Errors.. recall that console must be input polarity, and all other things must be output
       | isintconsole && pol == Input -> return $ Just (IntService, StdService)
       | ischarconsole && pol == Input -> return $ Just (CharService, StdService)
       | isint && pol == Output -> getTerminalNetworkedService IntService
       | ischar && pol == Output -> getTerminalNetworkedService CharService
       | otherwise -> return Nothing
  where
    getTerminalNetworkedService svs = do
        stream <- gets keyStream
        ftermcmd <- gets terminalNetworkedCommand
        let key = Stream.head stream
            rst = Stream.tail stream
        modify (\s -> s { keyStream = rst })
        return $ Just (svs, TerminalNetworkedService (ftermcmd key) key)


globalChanIDStream :: Stream GlobalChanID
globalChanIDStream = Stream.iterate pred (GlobalChanID 0)

class HasParseAndLexError e where
    parseAndLexError :: String -> e

newtype ParseAndLexError = ParseAndLexError String 
  deriving Show

instance HasParseAndLexError ParseAndLexError where
    parseAndLexError = ParseAndLexError

data AssemblerErrors =
    NoMainFunction
    | BnfcError ParseAndLexError
    | SymAmbiguousLookup SymAmbiguousLookup
    | IllegalInstrCall (IllegalInstrCall AssemblerErrors)
    | FunctionArityMismatch FunctionArityMismatch
    | CasingOverMultipleDatasError CasingOverMultipleDatasError
    | RecordOverMultipleCodatasError RecordOverMultipleCodatasError
    | DataArityMismatch DataArityMismatch
    | CodataArityMismatch CodataArityMismatch
    | ProcessArityMismatch ProcessArityMismatch
    | HCasingOverMultipleDatasError HCasingOverMultipleDatasError
    | PolarityMismatch PolarityMismatch
    | FreeVarError FreeVarError
    | FreeChannelError FreeChannelError
    | CodataLookupError CodataLookupError
    | CoprotocolLookupError CoprotocolLookupError
    | DataLookupError DataLookupError
    | ProtocolLookupError ProtocolLookupError
    | ProcessLookupError ProcessLookupError
    | FunctionLookupError FunctionLookupError
    | HCaseArityMismatch HCaseArityMismatch
  deriving Show

instance HasParseAndLexError AssemblerErrors where
    parseAndLexError = BnfcError . parseAndLexError

instance HasAmbiguousLookupError AssemblerErrors where
    symAmbiguousLookup = AMPLAssemble.SymAmbiguousLookup . symAmbiguousLookup

instance HasIllegalInstrCallError AssemblerErrors AssemblerErrors where
    illegalInstrCall a = AMPLAssemble.IllegalInstrCall . illegalInstrCall a

instance HasFunctionArityMismatchError AssemblerErrors where
    functionArityMismatch a = AMPLAssemble.FunctionArityMismatch . functionArityMismatch a

instance HasCasingOverMultipleDatas AssemblerErrors where
    casingOverMultipleDatas = AMPLAssemble.CasingOverMultipleDatasError . casingOverMultipleDatas

instance HasHCasingOverMultipleTypes AssemblerErrors where
    hcasingOverMultipleTypes = AMPLAssemble.HCasingOverMultipleDatasError . hcasingOverMultipleTypes

instance HasRecordOverMultipleCodatas AssemblerErrors where
    recordOverMultipleCodatas = AMPLAssemble.RecordOverMultipleCodatasError . recordOverMultipleCodatas
    
instance HasDataArityMismatchError AssemblerErrors where
    dataArityMismatch a = AMPLAssemble.DataArityMismatch . dataArityMismatch a
    
instance HasCodataArityMismatchError AssemblerErrors where
    codataArityMismatch a = AMPLAssemble.CodataArityMismatch . codataArityMismatch a
    
instance HasProcessArityMismatch AssemblerErrors where
    processArityMismatch a = AMPLAssemble.ProcessArityMismatch . processArityMismatch a
    
instance HasPolarityMismatch AssemblerErrors where
    polarityMismatch a = AMPLAssemble.PolarityMismatch . polarityMismatch a
    
instance HasFreeVarError AssemblerErrors where
    freeVar = AMPLAssemble.FreeVarError . freeVar
    
instance HasFreeChannelError AssemblerErrors where
    freeChannel = AMPLAssemble.FreeChannelError . freeChannel
    
instance HasCodataLookupError AssemblerErrors where
    codataDoesNotExist = AMPLAssemble.CodataLookupError . codataDoesNotExist
    destructorDoesNotExist a = AMPLAssemble.CodataLookupError . destructorDoesNotExist a
    notCodata a = AMPLAssemble.CodataLookupError . notCodata a
    
instance HasCoprotocolLookupError AssemblerErrors where
     coprotocolDoesNotExist = AMPLAssemble.CoprotocolLookupError . coprotocolDoesNotExist 
     cohandleDoesNotExist a = AMPLAssemble.CoprotocolLookupError . cohandleDoesNotExist a
     notCoprotocol a = AMPLAssemble.CoprotocolLookupError . notCoprotocol a
    
instance HasDataLookupError AssemblerErrors where
     dataDoesNotExist = AMPLAssemble.DataLookupError . dataDoesNotExist
     constructorDoesNotExist a = AMPLAssemble.DataLookupError . constructorDoesNotExist a
     notData a = AMPLAssemble.DataLookupError . notData a
    
instance HasProtocolLookupError  AssemblerErrors where
     protocolDoesNotExist = AMPLAssemble.ProtocolLookupError . protocolDoesNotExist 
     handleDoesNotExist a = AMPLAssemble.ProtocolLookupError . handleDoesNotExist a
     notProtocol a = AMPLAssemble.ProtocolLookupError . notProtocol a
    
instance HasLookupProcessError AssemblerErrors where
     processDoesNotExist = AMPLAssemble.ProcessLookupError . processDoesNotExist
     notProcess a = AMPLAssemble.ProcessLookupError . notProcess a
    
instance HasLookupFunctionError AssemblerErrors where
    functionDoesNotExist = AMPLAssemble.FunctionLookupError . functionDoesNotExist
    notFunction a = AMPLAssemble.FunctionLookupError . notFunction a

instance HasHCaseArityMismatch AssemblerErrors where
    hcaseArityMismatch a = AMPLAssemble.HCaseArityMismatch . hcaseArityMismatch a

