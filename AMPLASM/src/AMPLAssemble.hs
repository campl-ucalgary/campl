{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module AMPLAssemble where 
import AMPL
import AMPLServices
import AMPLTypes
import AMPLErrors
import AMPLCompileErrors
import AMPLCompile
import AMPLSymbolTable
import AMPLConstructBag
import AMPLAST

import Language.ParAMPL
import Language.LexAMPL
import Language.AbsAMPL
import Language.ErrM
import Language.LayoutAMPL

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

import Debug.Trace


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
    AmplAsmBag{ amplImports = imports, amplMainInfo = mainfun, amplConstructsBag = bag} = 
        AMPLConstructBag.collectSymbols amplcode
    symboltable = AMPLSymbolTable.makeSymbolTable bag :: Map String (Either AssemblerErrors SymEntry)
    functions = map f (functionInfo bag)
      where
        f :: (String, FunctionInfo [ACom]) -> Either (Ident, [AssemblerErrors]) (FunID,(String, [Instr]))
        f (fname, (fpos, (args, coms))) = do
            (_, (_, funid)) <- Bifunctor.first (((fname,fpos),) . pure) $ lookupFunction (fname, fpos) symboltable
            Bifunctor.bimap 
                (\errs -> ((fname, fpos), errs)) 
                (\(instrs, _) -> (funid, (fname,instrs)))
                $ compileRunner coms 
                    (CompileEnv { symbolTable = symboltable }) 
                    (CompileState { localVarStack = map fst args , channelTranslations = [] } )
    protocols = map f (processInfo bag)
      where
        f :: (String, ProcessInfo [ACom]) -> Either (Ident, [AssemblerErrors]) (FunID,(String, [Instr]))
        f (pname, (ppos, (args, inchs, outchs, coms))) = do
            let intranslations = map (fst *** (Input,)) inchs
                outtranslations = map (fst *** (Output,)) outchs
            (_, (_, _, _, funid)) <- Bifunctor.first (((pname,ppos),) . pure) 
                                        $ lookupProcess (pname, ppos) symboltable
            Bifunctor.bimap (\errs -> ((pname, ppos), errs)) (\(instrs, _) -> (funid, (pname,instrs)))
                $ compileRunner coms 
                    (CompileEnv { symbolTable = symboltable }) 
                    (CompileState { localVarStack = map fst args , channelTranslations = intranslations ++ outtranslations } )
    cmainfun = case mainfun of
        Just (runident, ((inchs, outchs), coms)) -> Bifunctor.first (runident,) $ do
            (maintranslations, nonservicechs, servicechs) <- Bifunctor.first pure 
                $ runExcept 
                $ flip evalStateT servicegeneratorstate $ do 
                        (intrans, ingchs, ingchssvs) <- 
                            getTranslationsInternalServiceChannelsAndExternalServiceChannels Input inchs
                        (outtrans, outgchs, outgchsvs) <- 
                            getTranslationsInternalServiceChannelsAndExternalServiceChannels Output outchs
                        return (intrans ++ outtrans, ingchs ++ outgchs, ingchssvs ++ outgchsvs)

            (maininstrs, _) <- compileRunner coms 
                            (CompileEnv { symbolTable = symboltable }) 
                            (CompileState { localVarStack = [] 
                                          , channelTranslations = 
                                            map (fst *** (Input,)) inchs 
                                            ++ map (fst *** (Output,)) outchs
                                          } )
            return ( (nonservicechs, servicechs) , (maininstrs, maintranslations) )
        Nothing -> Left (("", (-1,-1)),[AsmNoMainFunction])
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
        StateT ServiceState (Except e) 
            ([Translation], [GlobalChanID], [(GlobalChanID, (ServiceDataType, ServiceType))])
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

pprintAmplAssemblerErrors :: [(Ident, [AssemblerErrors])] -> String
pprintAmplAssemblerErrors = intercalate "\n" . map (render . pprintAmplAssemblerError)

pprintAmplAssemblerError :: (Ident, [AssemblerErrors]) -> Doc
pprintAmplAssemblerError (_, [AsmNoMainFunction]) = hcat 
    [text "Error: ", text "Failed to compile -- no main function exists."] 
pprintAmplAssemblerError (_, [AsmBnfcError (ParseAndLexError str)]) = hcat
    [text "Bnfc error: ", text str]

pprintAmplAssemblerError (ident, err) = 
    hsep [text "Error(s) in function/process",  identToDoc ident, text "as follows:"]
        $$ nest 2 (vcat $ intersperse (text "\n") $ map f err)
  where
    f AsmNoMainFunction = text ("No main function exists")
    f (AsmBnfcError (ParseAndLexError str)) = hcat [text ("Bnfc error: "), text str]
    f (AsmSymAmbiguousLookup (SymAmbiguousLookup (name, occurences))) = 
        hsep [text "Ambiguous lookup with", text name, text "could be referring to declarations at (line, column):"] 
            $$ nest 2 (vcat (map (\(row,col) -> hcat [lparen,int row,int col,rparen]) occurences))
    f n = text (show n)

identToDoc :: Ident -> Doc
identToDoc (name, (row, col)) = hsep [text name, text "at line", int row, text "and column", int col]

-- render $ doc 
