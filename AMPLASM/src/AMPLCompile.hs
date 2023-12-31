{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module AMPLCompile where

import Language.ParAMPL
import Language.LexAMPL
import Language.AbsAMPL
import Language.ErrM
import Language.LayoutAMPL

import AMPLSymbolTable
import AMPLAST
import AMPLConstructBag
import AMPLTypes
import AMPLCompileErrors

import Control.Monad.RWS
import Control.Monad.Except
import Data.Map (Map)

import Control.Arrow
import qualified Data.Bifunctor as Bifunctor
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.List 
import Data.Function
import Data.Coerce
import Data.Functor.Const

import Debug.Trace

import Data.Stream (Stream)
import qualified Data.Stream as Stream

type LocalVarStack = [String]
type ChannelTranslations = [(String,(Polarity, LocalChanID))]

newtype CompileEnv e = CompileEnv {
    symbolTable :: Map String (Either e SymEntry)
        -- ^ Symbol table
}

data CompileState = CompileState {
    localVarStack :: LocalVarStack
    , channelTranslations :: ChannelTranslations
}

compileRunner ::
    forall e.
    CompilerErrors e =>
    [ACom] ->
    CompileEnv e ->
    CompileState ->
    Either [e] ([Instr], CompileState)
compileRunner coms env state = 
    case runRWS (compile coms) env state  of 
        (res, st, []) -> Right (res, st)
        (res, st, errs) -> Left errs

compile :: 
    forall e.
    CompilerErrors e =>
    [ACom] ->
    RWS 
        (CompileEnv e)
            --  compiling environment
        [e]
            --  accumlated errors
        CompileState
            --  State of local variables and channel translations
        [Instr]
compile [] = return []
compile (c:cs) = do
    ec <- runExceptT (f c)
    case ec of
        Left errs -> tell errs >> compile cs
        Right instrs -> (instrs ++) <$> compile cs
  where
    f :: ACom -> ExceptT [e] (RWS (CompileEnv e) [e] CompileState) [Instr] 
    f c = case c of 
        AAssign var com -> do
            env <- ask
            state <- get
            (com', _) <- liftEither $ compileRunner [com] env state
            modify (\s -> s { localVarStack = fst var : localVarStack s} )
            return (com' ++ [iStore])
        AStore store var -> do
             modify (\s -> s { localVarStack = fst var : localVarStack s} )
             return [iStore]
        ALoad load var -> do
            lclstack <- gets localVarStack
            access <- liftEither $ Bifunctor.first (pure . illegalInstrCall load) $ lookupLocalVarStack var lclstack
            return [iAccess access]
                 
        ARet _ -> return [iRet]

        ACall call callfun args -> do
            symtable <- asks symbolTable
            (funpos, (funargs, funid)) <- liftEither $ Bifunctor.first (pure . illegalInstrCall call) $ lookupFunction callfun symtable
            if length funargs == length args  
                then do
                    lclstack <- gets localVarStack
                    loads <- liftEither 
                        $ Bifunctor.first (map (illegalInstrCall call)) 
                        $ getLocalVarLookupInstrs args lclstack
                    modify (\s -> s { localVarStack = map fst args})
                    return $ loads ++ [iCall funid (genericLength funargs)]
                else throwError 
                    [functionArityMismatch ((fst callfun, funpos ), funargs) (snd callfun, args)]
        AInt _ v -> return [iConst (VInt v)]
        AChar _ v -> return [iConst (VChar v)]
        AAnd _ -> return [iAndBool]
        AOr _ -> return [iOrBool]
        ATrue _ -> return [iConst (VBool True)]
        AFalse _ -> return [iConst (VBool False)]

        ALeqInt _ -> return [iLeq]
        AEqInt _ -> return [iEq]
        ALeqChar _ -> return [iLeq]
        AEqChar _ -> return [iEq]

        AAddInt _ -> return [iAddInt]
        ASubInt _ -> return [iSubInt]
        ADivInt _ -> return [iDivInt]
        AModInt _ -> return [iModInt]

        AIf ifident (var, (thenprog, elseprog)) -> do
            env <- ask
            state <- get
            var' <- liftEither $ getLocalVarLookupInstrs [var] (localVarStack state)
            (thenprog', _) <- liftEither $ compileRunner thenprog env state
            (elseprog', _) <- liftEither $ compileRunner elseprog env state
            return (var' ++ [iIf thenprog' elseprog'])


        AConstructor (datatype, constructor) -> 
            f (AConstructorArgs ((datatype, constructor), []))
        AConstructorArgs ((datatype, constructor), args) -> do
            symtable <- asks symbolTable
            (datapos, (consident, (consix, consargs))) <- liftEither $ Bifunctor.first pure $ lookupDataAndConstructor datatype constructor symtable
            if consargs == genericLength args
                then do
                    lclstack <- gets localVarStack
                    either 
                        (\errs -> tell errs >> return []) 
                            -- this error message is not very informative...
                        (\loads -> return (loads ++ [iCons consix consargs]))
                        (getLocalVarLookupInstrs args lclstack)
                else tell [ 
                    dataArityMismatch 
                        ((fst datatype, datapos), consident, consargs) 
                        (snd datatype, snd constructor, []) 
                        ] >> return []
                
        ACase caseident (caseon, labelcoms) -> do 
            env <- ask
            state <- get
            caseon' <- liftEither $ getLocalVarLookupInstrs [caseon] (localVarStack state)
            case getConst (labelComsGenerateCode env state labelcoms :: Const (Either [e] [[Instr]]) LabelComsCodeGenDataConstructors) of
                        Right instrs -> return (caseon' ++ [iCase instrs])
                        Left errs -> throwError errs
        ARecord _ labelcoms -> do 
            env <- ask
            state <- get
            case getConst (labelComsGenerateCode env state labelcoms :: Const (Either [e] [[Instr]]) LabelComsCodeGenCodataDestructors) of
                        Right instrs -> return [iRec instrs]
                        Left errs -> throwError errs

        ADest (codataname, destructor) var -> f (ADestArgs ((codataname, destructor), []) var)
        ADestArgs ((codataname, destructor), args) var -> do
            symtable <- asks symbolTable
            state <- get
            (codatapos, (desident,(desix, desargs))) <- liftEither 
                        $ Bifunctor.first pure 
                        $ lookupCodataAndDestructor codataname destructor symtable
            if desargs == genericLength args
                then do
                    loads <- liftEither $ getLocalVarLookupInstrs args (localVarStack state)
                    var' <-  liftEither $ Bifunctor.first pure $ lookupLocalVarStack var (localVarStack state)
                    return (loads ++ [iAccess var', iDest desix desargs])
                else throwError [codataArityMismatch ((fst codataname, codatapos), desident, desargs) (snd codataname, snd destructor, args)]

        AGet getident var ch -> do
            translations <- gets channelTranslations
            (_, lch) <- liftEither $ Bifunctor.first (pure . illegalInstrCall getident) $ lookupTranslation ch translations
            modify (\s -> s { localVarStack = fst var : localVarStack s })
            return [iGet lch, iStore]
        APut putident var ch -> do
            lclstack <- gets localVarStack
            translations <- gets channelTranslations
            (_, lch) <- liftEither 
                        $ Bifunctor.first (pure . illegalInstrCall putident) 
                        $ lookupTranslation ch translations
            access <- liftEither 
                        $ Bifunctor.first (map (illegalInstrCall putident))
                        $ getLocalVarLookupInstrs [var] lclstack
            lclstack <- gets localVarStack
            return (access ++ [iPut lch])

        --  RECALL:
        --  Handles: these are handle names associated with a type.  
        --    They are hput on output polarity channels and hcased on input polarity channels
        --  Cohandles:  these are handle names associated with a type.
        --    They are hput on input polarity channels and hcased on output polarity channels
        AHPut hputident (name, subname) ch  -> do
            translations <- gets channelTranslations
            symtable <- asks symbolTable
            (pol, lch) <- liftEither 
                        $ Bifunctor.first (pure . illegalInstrCall hputident) 
                        $ lookupTranslation ch translations
            case pol of 
                Output -> do
                    (_, (_, ix)) <- liftEither 
                            $ Bifunctor.first (pure . illegalInstrCall hputident) 
                            $ lookupProtocolAndHandle name subname symtable
                    return [iHPut lch ix]
                Input -> do
                    (_ ,(_, ix)) <- liftEither 
                            $ Bifunctor.first (pure . illegalInstrCall hputident) 
                            $ lookupCoprotocolAndCohandle name subname symtable
                    return [iHPut lch ix]

        --   AC_HCASEf  .COM ::= Hcase PIdent "of"  "{" [LABELCOMS] "}"  ; 
        AHCase hcaseident (ch, labelcoms) -> do
            env <- ask
            state <- get
            (pol, lch) <- liftEither $ Bifunctor.first pure $ lookupTranslation ch (channelTranslations state)
            case pol of
                Input -> case getConst (labelComsGenerateCode env state labelcoms :: Const (Either [e] [[Instr]]) LabelComsCodeGenProtocolsHandles) of
                            Right instrs -> return [iHCase lch instrs]
                            Left errs -> throwError errs
                Output -> case getConst (labelComsGenerateCode env state labelcoms :: Const (Either [e] [[Instr]]) LabelComsCodeGenCoprotocolsCohandles) of
                            Right instrs -> return [iHCase lch instrs]
                            Left errs -> throwError errs

        -- AC_SPLITf  .COM ::= Split PIdent "into" PIdent PIdent    ;
        ASplit hsplitident ch ch1 ch2 -> do
            translations <- gets channelTranslations
            (pol, lch) <- liftEither $ Bifunctor.first (pure . illegalInstrCall hsplitident) $ lookupTranslation ch translations
            let lch1 = computeFreshLocalChanIDByMaximum translations
                lch2 = succ lch1
            modify (\s -> s 
                { channelTranslations = 
                    (fst ch2, (pol, lch2)) 
                    : (fst ch1, (pol, lch1))
                    : channelTranslations s
                })
            return [iSplit lch (lch1, lch2)]

        -- AC_FORKf   .COM ::= Fork PIdent "as" "{" PIdent "with" [PIdent] ":" COMS ";" PIdent "with" [PIdent] ":" COMS "}" ;
          --  AC_FORKf Fork PIdent PIdent [PIdent] COMS PIdent [PIdent] COMS
        AFork forkident ch
            ((ch1, chs1), coms1)
            ((ch2, chs2), coms2) -> do
                env <- ask
                state <- get
                translations <- gets channelTranslations
                (pol, lch) <- liftEither 
                            $ Bifunctor.first (pure . illegalInstrCall forkident) 
                            $ lookupTranslation ch translations
                lchs1 <- liftEither $ restrictChannelTranslation chs1 translations
                lchs2 <- liftEither $ restrictChannelTranslation chs2 translations

                let lch1 = computeFreshLocalChanIDByMaximum translations
                    lch2 = succ lch1

                (instrs1, _) <- liftEither 
                            $ compileRunner coms1 env 
                                (state { channelTranslations = (fst ch1, (pol,lch1)) : lchs1 })
                (instrs2, _) <- liftEither 
                            $ compileRunner coms2 env 
                                (state { channelTranslations = (fst ch2, (pol,lch2)) : lchs2 })

                return [iFork lch ((lch1, map (snd . snd) lchs1, instrs1), (lch2, map (snd . snd) lchs2, instrs2))]

        -- AC_PLUGf :: Plug -> [PIdent] -> [PIdent] -> COMS -> [PIdent] -> COMS -> COM
        -- AC_PLUGf   .COM ::= Plug [PIdent]  "as" "{" "with" "[" [PIdent] "]" ":" COMS ";" "with" "[" [PIdent] "]" ":" COMS "}" ;
        APlug plugident pluggedchs
            (chs1, coms1)
            (chs2, coms2) -> do
                env <- ask
                state <- get
                translations <- gets channelTranslations

                let pluggedlchs = zipWith 
                                const 
                                (iterate succ (computeFreshLocalChanIDByMaximum translations)) 
                                pluggedchs

                lchs1 <- liftEither $ restrictChannelTranslation chs1 translations
                lchs2 <- liftEither $ restrictChannelTranslation chs2 translations

                (instrs1, _) <- liftEither   
                                $ compileRunner coms1 env 
                                    (state { channelTranslations = 
                                            zip 
                                                (map fst pluggedchs) 
                                                (zip (iterate (const Output) Output) pluggedlchs) ++ lchs1 
                                            })
                (instrs2, _) <- liftEither   
                                $ compileRunner coms2 env 
                                    (state { channelTranslations = 
                                            zip    
                                                (map fst pluggedchs) 
                                                (zip (iterate (const Input) Input) pluggedlchs) ++ lchs2 
                                            })

                return [ iPlug pluggedlchs ((map (snd . snd) lchs1, instrs1), (map (snd . snd) lchs2, instrs2)) ]

        -- AC_RUNf :: Run -> PIdent -> [PIdent] -> [PIdent] -> [PIdent] -> COM
        ARun runident
            process (vars, (inchs, outchs)) -> do
                env <- ask
                state <- get
                (pos, (pvars, pinchs, poutchs, funid)) <- liftEither $ Bifunctor.first pure $ lookupProcess process (symbolTable env)
                if length vars == length pvars && length inchs == length pinchs && length outchs == length poutchs
                    then do
                        loads <- liftEither $ getLocalVarLookupInstrs vars (localVarStack state)
                        inlchs <- liftEither $ lookupTranslationsByPolarity inchs Input (channelTranslations state)
                        outlchs <- liftEither $ lookupTranslationsByPolarity outchs Output (channelTranslations state)
                        let inputmappings = zipWith (\(_, lch) lch' -> (Input, (lch,lch'))) pinchs inlchs 
                            outputmappings = zipWith (\(_, lch) lch' -> (Output, (lch,lch'))) poutchs outlchs 
                            translationmappings =  inputmappings ++ outputmappings
                        return (loads ++ [ iRun translationmappings funid (genericLength vars) ])
                    else throwError [processArityMismatch ((fst process, pos), pvars, map fst pinchs, map fst poutchs) (snd process, vars, inchs, outchs)]

        AId idident (ch1, ch2) -> do
            translations <- gets channelTranslations
            lch1 <- liftEither $ Bifunctor.first pure $ lookupTranslation ch1 translations
            lch2 <- liftEither $ Bifunctor.first pure $ lookupTranslation ch2 translations
            return [iId (snd lch1) (snd lch2)]

        -- AC_RACE :: Race -> [RACES] -> COM
        ARace raceident races -> do
            env <- ask
            state <- get
            let res = map (f env state) races
                errs = concat $ lefts res
                sucs = rights res
            if null errs
                then return [ iRace sucs ]
                else throwError errs
          where
            f :: CompileEnv e -> CompileState -> (Ident, [ACom]) -> Either [e] (LocalChanID, [Instr])
            f env state (ch, coms) = do
                (pol, lch) <- Bifunctor.first pure $ lookupTranslation ch (channelTranslations state)
                (instr, _) <- compileRunner coms env (state { channelTranslations = (fst ch, (pol,lch)) : channelTranslations state })
                return (lch, instr)

        AClose closeident ch -> do
            translations <- gets channelTranslations
            (pol, lch) <- liftEither $ Bifunctor.first pure $ lookupTranslation ch translations
            return [ iClose lch ]

        AHalt haltident chs {- (map pIdentToIdent -> chs) -} -> do
            translations <- gets channelTranslations
            pollchs <- liftEither $ lookupTranslations [chs] translations
            return [ iHalt (map snd pollchs) ]

        n -> error $ "have not implemented instruction: " ++ show n


                
{-
--      Handles: these are handle names associated with a type.  
--        They are hput on output polarity channels and hcased on input polarity channels
--      Cohandles:  these are handle names associated with a type.
--        They are hput on input polarity channels and hcased on output polarity channels


AC_PROD    .COM ::= "(" [PIdent] ")" ;
AC_PRODELEM.COM ::= "#" CInteger "(" PIdent ")" ;
AC_EMSG    .COM ::= String ;

separator PIdent "," ;

AC_CLOSEf  .COM ::= Close PIdent ;
AC_HALTf   .COM ::= Halt [PIdent] ;
-}

-- Help! This did not generalize well at all!
data LabelComsCodeGenDataConstructors 
data LabelComsCodeGenCodataDestructors 
data LabelComsCodeGenProtocolsHandles 
data LabelComsCodeGenCoprotocolsCohandles

class LabelComsCodeGenerator a where
    labelComsGenerateCode :: forall e. CompilerErrors e => 
        CompileEnv e -> 
        CompileState -> 
        [ALabelComs] -> 
        Const (Either [e] [[Instr]]) a
    labelComsGenerateCode env state labelcoms = 
        let dataNames = map getDataName labelcoms
            labelcoms' = map (labelComsGenerateCode' env state) labelcoms
            labelcomserrs = concat (lefts labelcoms')
            casesinstrs = foldr 
                (\(ix, instrs) h (ix':ixs') -> 
                    if ix == ix' 
                        then instrs : h ixs' 
                        else [iErrorMsg "Non exhaustive case on."] : h ixs') 
                (const [])
                (sortBy (compare `on` fst) (rights labelcoms')) 
                (coerce $ Stream.toList AMPLTypes.wordStream)
        in Const $ if null dataNames || all ((fst (head dataNames)==) . fst) dataNames
                    then if null labelcomserrs then Right casesinstrs else Left labelcomserrs
                    else Left [getConst (labelComsMultipleTypesError dataNames :: Const e a)]

      where
        getDataName :: ALabelComs -> Ident
        getDataName (ALabelComs (cts, _) _)  = cts
        getDataName (ALabelComsArgs ((cts, _), _) _)  = cts
    
        labelComsGenerateCode' :: CompileEnv e -> CompileState -> ALabelComs -> Either [e] (Word, [Instr])
        labelComsGenerateCode' env state (ALabelComs (dataname, cts) coms) = labelComsGenerateCode' env state (ALabelComsArgs ((dataname, cts), []) coms )
        labelComsGenerateCode' env state (ALabelComsArgs ((dataname, cts), args) coms) = do
            (datapos, (ctsident, (ix, numargs))) <- Bifunctor.first pure $ getConst 
                                            $ (labelComsLookup dataname cts (symbolTable env) :: Const (Either e (RowColPos, (Ident, (Word, Word)))) a)
            if numargs == genericLength args
                then do 
                    let state' = (state{ localVarStack = map fst args ++ localVarStack state })
                    (instr, _) <- compileRunner coms env state'
                    return $ (ix, instr)
                else Left [getConst (labelComsArityError ((fst dataname, datapos), ctsident, numargs) (snd dataname, snd cts, []) :: Const e a)]


    labelComsArityError :: CompilerErrors e => (Ident, Ident, Word) -> (RowColPos, RowColPos, [Ident]) -> Const e a
    labelComsMultipleTypesError :: CompilerErrors e => [Ident] -> Const e a
    labelComsLookup :: CompilerErrors e => 
        Ident -> 
        Ident -> 
        Map String (Either e SymEntry) -> 
        Const (Either e (RowColPos, (Ident, (Word, Word)))) a
                                    -- ix, num args
instance LabelComsCodeGenerator LabelComsCodeGenDataConstructors where
    labelComsArityError a = Const . dataArityMismatch a
    labelComsMultipleTypesError = Const . casingOverMultipleDatas 
    labelComsLookup dta cts map = Const 
        $ (coerce :: (RowColPos, (Ident, (ConsIx, Word))) -> (RowColPos, (Ident, (Word, Word)))) 
        <$> lookupDataAndConstructor dta cts map

instance LabelComsCodeGenerator LabelComsCodeGenCodataDestructors where
    labelComsArityError a = Const . codataArityMismatch a
    labelComsMultipleTypesError = Const . recordOverMultipleCodatas
    labelComsLookup dta cts map = Const 
        $ (coerce :: (RowColPos, (Ident, (DesIx, Word))) -> (RowColPos, (Ident, (Word, Word)))) 
        <$> lookupCodataAndDestructor dta cts map

instance LabelComsCodeGenerator LabelComsCodeGenCoprotocolsCohandles where
    labelComsArityError a = Const . hcaseArityMismatch a
    labelComsMultipleTypesError = Const . hcasingOverMultipleTypes
    labelComsLookup dta cts map = Const 
        $ (coerce :: (RowColPos, (Ident, (HCaseIx, Word))) -> (RowColPos, (Ident, (Word, Word))))
            . (second (second (id &&& const 0) ))
        <$> lookupCoprotocolAndCohandle dta cts map

instance LabelComsCodeGenerator LabelComsCodeGenProtocolsHandles where
    labelComsArityError a = Const . hcaseArityMismatch a
    labelComsMultipleTypesError = Const . hcasingOverMultipleTypes
    labelComsLookup dta cts map = Const 
        $ (coerce :: (RowColPos, (Ident, (HCaseIx, Word))) -> (RowColPos, (Ident, (Word, Word))))
            . (second (second (id &&& const 0) ))
        <$> lookupProtocolAndHandle dta cts map

restrictChannelTranslation :: 
    HasFreeChannelError e => 
    [Ident] -> 
    ChannelTranslations -> 
    Either [e] ChannelTranslations
restrictChannelTranslation restriction translations =
    let restriction' = map fst restriction
        translations' = map fst translations
        errs = mapMaybe (\(name, pos) -> if name `elem` translations' then Nothing else Just (freeChannel (name, pos))) restriction
    in if null errs then Right $ filter ( (`elem` map fst restriction) . fst ) translations else Left errs

computeFreshLocalChanIDByMaximum :: ChannelTranslations -> LocalChanID
computeFreshLocalChanIDByMaximum [] = LocalChanID 0
computeFreshLocalChanIDByMaximum translations = succ $ maximum (map (snd . snd) translations)

localVarPositionsStream :: Stream Word
localVarPositionsStream = Stream.iterate succ 0


lookupLocalVarStack :: ( HasFreeVarError e ) =>
    Ident ->
    LocalVarStack ->
    Either e Word
lookupLocalVarStack ident stack = maybe (Left $ freeVar ident) Right $
        lookup (fst ident) (zip stack (Stream.toList localVarPositionsStream)) 

getLocalVarLookupInstrs :: 
    forall e.
    HasFreeVarError e =>
    [Ident] -> 
    LocalVarStack ->
    Either [e] [Instr]
getLocalVarLookupInstrs localvars stack = do
    let lookuploads = map (`lookupLocalVarStack`stack ) localvars :: [Either e Word]
        loads = rights lookuploads
        loaderrs = lefts lookuploads
    if null loaderrs
        then Right (reverse $ map iAccess loads)
        else Left loaderrs

lookupTranslation :: 
    HasFreeChannelError e =>
    Ident ->
    ChannelTranslations ->
    Either e (Polarity, LocalChanID)
lookupTranslation ident translations = maybe (Left $ freeChannel ident) Right $
        lookup (fst ident) translations

lookupTranslations ::
    HasFreeChannelError e =>
    [Ident] ->
    ChannelTranslations ->
    Either [e] [(Polarity, LocalChanID)]
lookupTranslations idents translations = 
    let lookedup = map (`lookupTranslation` translations) idents
        errs = lefts lookedup
        chanids = rights lookedup
    in if null errs then Right chanids else Left errs

lookupTranslationsByPolarity :: 
    ( HasFreeChannelError e 
    , HasPolarityMismatch e ) =>
    [Ident] ->
    Polarity ->
    ChannelTranslations ->
    Either [e] [LocalChanID]
lookupTranslationsByPolarity idents pol translations = do
    polschnids <- lookupTranslations idents translations
    let filtered = zipWith (\ident (lpol, lch) -> if pol == lpol then Right lch else Left $ polarityMismatch pol (ident, lpol)) idents polschnids
        errs = lefts filtered
        sucs = rights filtered
    if null errs
        then Right sucs
        else Left errs


