{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module MplAsmPasses.Compile.Compile where

import MplMach.MplMachTypes

import MplAsmAST.MplAsmCore
import MplAsmPasses.Compile.CompileErrors
import MplAsmPasses.Compile.Stack

import MplAsmPasses.Parse.ParseAST

import Optics 
import Optics.State.Operators

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Traversable

import Debug.Trace
import Data.Coerce
import Data.List
import Data.Maybe
import Data.Function
import Data.Bool

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set  as Set

import qualified Data.Array as Arr

import Control.Exception

mplAsmProgToInitMachState ::
    ( Ord (IdP x) 
    , AsCompileError err x 
    , HasName (IdP x)) =>
    -- | an assembly program
    MplAsmProg x -> 
    -- | Either: errors; or an array of the supercombinators and the main function instruction
    Either [err] (MplMachSuperCombinators, [Instr])
mplAsmProgToInitMachState prog = case runWriter res of
    (res', []) -> Right res'
    (_, errs) -> Left errs
  where
    res = flip evalStateT initMplAsmCompileSt $ do
        {- compilation of statements is straightforward -}

        -- first, put evreything in symbol table. see 'mplAsmCollectStmtsInSymTab' for why
        -- this is necessasry[
        mplAsmCollectStmtsInSymTab prog

        funs <- fmap concat $ traverse mplAsmCompileStmt (prog ^. mplAsmStmts)

        {- compilation of the main function is a bit more complicated.  
         - We have some weirdness. Recall that:
                 - @console@ and @cconsole@ are of input polarity, and these are int and char terminals on std
                 - all other are of output polarity.
         - Here are the steps:
                - reset the local channel id counter (as normal when compiling proceses)
                - tag all the channels (also normal when compiling processes)
                - add the main function to the symbol table. (as normal when compiling processes)
                - don't add the sequential arugments (indeed, there should be none) -- THIS IS TODO
                - update the channel translations
                - compile the commands
                - resolve the channels according to the above "weirdness"
         -}
        {-
        ~(Just (services, mainf)) <- case prog ^. mplAsmMain of
            Just (mainident, (seqs, ins, outs), coms) -> assert (null seqs) $ do
                let overlapping = filter ((>1) . length) $ group $ sort $ seqs ++ ins ++ outs
                tell $ bool [_OverlappingDeclarations # overlapping] [] $ null overlapping 

                ~(Just (funid, (_, insids, outsids))) <- guse (symTab % symTabProcs % at mainident)

                {- Main function should already be put in (if it exists from 'mplAsmCollectStmtsInSymTab')
                -- reset local channel id
                uniqCounters % uniqLocalChan .= (coerce (0 :: Int) :: LocalChan)

                -- give fresh local channel ids to the channels
                insids <- traverse (const freshLocalChan) ins
                outsids <- traverse (const freshLocalChan) outs

                -- main function should have a fresh funciton id
                funid <- freshCallIx 
                -- update the symbol table accordingly (in case we want to recursively call the main function)
                symTab % symTabProcs % at mainident 
                    ?= (funid, (genericLength seqs, insids, outsids))
                -}

                -- no need to add the sequential arugments to the stack
                -- varStack .= reverse seqs
                -- indeed, we need the channel translations to compile this.
                channelTranslations .= Map.fromList (zip ins (map (Input,) insids) ++ zip outs (map (Output,) outsids))

                -- compile the instructions
                maininstrs <- mplAsmComsToInstr coms

                {-
                 - resolve according to above "weirdness" 
                 - We want to output something of type: 
                    - ([Translation], [(GlobalChanID, (ServiceDataType, ServiceType))])
                 - where we have the necessary translation from local channels to global channels,
                 - and all the service channels
                 -}
                intransandservices <- for (zip ins insids) $ \(ch, chid) -> do
                    let chstr = ch ^. nameStr 
                    if | chstr == "console" -> do
                            gch <- freshGlobalChanId
                            return $ ([(Input, (chid, gch))], [(gch, (IntService, StdService))])
                       | chstr == "cconsole" -> do
                            gch <- freshGlobalChanId
                            return $ ([(Input, (chid, gch))], [(gch, (CharService, StdService))])
                       | otherwise -> tell [_UnknownInputService # ch] >> return mempty
                -- TODO: This is really bad; we need to rethink how the machine 
                -- handles services in the future.. probably do some WAI / warp webserver
                -- is the best way to do this
                outtransandservices <- for (zip outs outsids) $ \(ch, chid) -> do
                    let chstr = ch ^. nameStr 
                        nkey = show (coerce chid :: Int)
                    if | "int" `isPrefixOf` chstr -> do
                            gch <- freshGlobalChanId
                            return $ 
                                ( [(Output, (chid, gch))]
                                , [
                                    ( gch
                                    , ( IntService
                                      , TerminalNetworkedService
                                        (concat 
                                            [ "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k "
                                            , nkey  
                                            ,"  ; read'"
                                            ])
                                        (Key nkey)
                                      )
                                    )
                                   ]
                                )
                       | "char" `isPrefixOf` chstr -> do
                            gch <- freshGlobalChanId
                            return $ 
                                ( [(Output, (chid, gch))]
                                , [
                                    ( gch
                                    , ( CharService
                                      , TerminalNetworkedService
                                        (concat 
                                            [ "xterm -e 'amplc -hn 127.0.0.1 -p 5000 -k "
                                            , nkey  
                                            ,"  ; read'"
                                            ])
                                        (Key nkey)
                                      )
                                    )
                                   ]
                                )

                       | otherwise -> tell [_UnknownOutputService # ch] >> return mempty

                return $ Just (intransandservices  <> outtransandservices, (funid, maininstrs))
            Nothing -> tell [_NoMainFunction # ()] >> return Nothing
        -}

        undefined
        {-
        return $ InitAMPLMachState 
                { initAmplMachStateServices = foldOf (folded % _2) services
                , initAmplMachMainFun = (snd mainf, foldOf (folded % _1) services )
                , initAmplMachFuns = mainf:funs 
                }
        -}


{- | collects all the elements into the symbol table. Note: this is required to permit mutually
 - recursive declarations between data / codta and functions, etc. -}
mplAsmCollectStmtsInSymTab ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m) =>
    MplAsmProg x ->
    m ()
mplAsmCollectStmtsInSymTab prog = do
    -- First, check for overlapping declarations
    overlappingCheck typeAndConcSpecsType protocols
    overlappingCheck typeAndConcSpecsType coprotocols
    overlappingCheck typeAndSeqSpecsType constructors
    overlappingCheck typeAndSeqSpecsType destructors


    overlappingCheck _1 processes
    overlappingCheck _1 functions

    -- add things in the symbol table
    concObjSymTab (symTab % symTabProtocol) protocols
    concObjSymTab (symTab % symTabCoprotocol) coprotocols

    seqObjSymTab (symTab % symTabData) constructors
    seqObjSymTab (symTab % symTabCodata) destructors

    funSymTab functions
    procSymTab processes

  where
    stmts = prog ^. mplAsmStmts

    protocols = concat $ mapMaybe (preview _Protocols) stmts  
    coprotocols = concat $ mapMaybe (preview _Coprotocols) stmts 

    constructors = concat $ mapMaybe (preview _Constructors) stmts 
    destructors = concat $ mapMaybe (preview _Destructors) stmts 

    functions = concat $ mapMaybe (preview _Functions) stmts
    processes = mainf ++ concat (mapMaybe (preview _Processes) stmts)

    mainf = maybeToList $ prog ^. mplAsmMain 

    -- checks for overlapping declaraings based on what is focused by the @viewlens@
    overlappingCheck viewlens decs = do
        let overlapping = filter ((>1) . length) $ group $ sort $ map (view viewlens) decs
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ null overlapping 

    -- insert protocl / coprotocls to the part of the smbol table that is focused by @viewlens@
    concObjSymTab viewlens decs = 
        for_ decs $ \(TypeAndConcSpecs tp handles) -> do
            let handles' = zip handles (coerce [0 :: Int ..] :: [HCaseIx])

            viewlens % at tp ?= Map.fromList handles'

    -- insert data / codatas to the part of the smbol table that is focused by @viewlens@
    seqObjSymTab viewlens decs =
        for_ decs $ \(TypeAndSeqSpecs tp handles) -> do
            let handles' = map (\((idp, numargs),caseix) -> (idp, (caseix,numargs))) 
                    $ zip handles (coerce [0 :: Int ..] :: [CaseIx])
            viewlens % at tp ?= Map.fromList handles'

    funSymTab decs = 
        for_ decs $ \(fname, args, _) -> do
            funid <- freshCallIx 

            symTab % symTabFuns % at fname ?= (funid, genericLength args)

    procSymTab decs =
        for_ decs $ \(pname, (seqs, ins, outs), _) -> do
            uniqCounters % uniqLocalChan .= (coerce (0 :: Int) :: LocalChan)

            insids <- traverse (const freshLocalChan) ins
            outsids <- traverse (const freshLocalChan) outs

            funid <- freshCallIx 
            symTab % symTabProcs % at pname ?= (funid, (length seqs, insids, outsids))

{-
    case stmt of
    Protocols tpconcspecs -> do
        let overlapping = group $ sort $ map (view typeAndConcSpecsType) tpconcspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] 
            $ length overlapping <= 1

        for tpconcspecs $ \(TypeAndConcSpecs tp handles) -> do
            let handles' = zip handles (coerce [0 :: Word ..] :: [HCaseIx])

            symTab % symTabProtocol % at tp ?= Map.fromList handles'

        return ()
    Coprotocols tpconcspecs -> do
        let overlapping = group $ sort $ map (view typeAndConcSpecsType) tpconcspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping <= 1

        for tpconcspecs $ \(TypeAndConcSpecs tp handles) -> do
            let handles' = zip handles (coerce [0 :: Word ..] :: [HCaseIx])

            symTab % symTabCoprotocol % at tp ?= Map.fromList handles'
        return ()
    Constructors tpseqspecs -> do
        let overlapping = group $ sort $ map (view typeAndSeqSpecsType) tpseqspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping <= 1

        for tpseqspecs $ \(TypeAndSeqSpecs tp handles) -> do
            let handles' = map (\((idp, numargs),caseix) -> (idp, (caseix,numargs))) 
                    $ zip handles (coerce [0 :: Word ..] :: [CaseIx])
            symTab % symTabData % at tp ?= Map.fromList handles'
        return ()

    Destructors tpseqspecs -> do
        let overlapping = group $ sort $ map (view typeAndSeqSpecsType) tpseqspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping <= 1

        for tpseqspecs $ \(TypeAndSeqSpecs tp handles) -> do
            let handles' = map (\((idp, numargs),caseix) -> (idp, (caseix,numargs))) 
                    $ zip handles (coerce [0 :: Word ..] :: [CaseIx])

            symTab % symTabCodata % at tp ?= Map.fromList handles'
        return ()

    Functions funs -> do
        let overlapping = group $ sort $ map (view _1) funs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping <= 1

        -- put everything in symbol table first.
        for funs $ \(fname, args, _) -> do
            funid <- freshCallIx 

            symTab % symTabFuns % at fname ?= (funid, genericLength args)

        return ()

    Processes procs -> do
        let overlapping = group $ sort $ map (view _1) procs
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping <= 1

        -- put everything in symbol table first.
        for procs $ \(pname, (seqs, ins, outs), _) -> do
            uniqCounters % uniqLocalChan .= (coerce (0 :: Int) :: LocalChan)

            insids <- traverse (const freshLocalChan) ins
            outsids <- traverse (const freshLocalChan) outs

            funid <- freshCallIx 
            symTab % symTabProcs % at pname 
                ?= (funid, (genericLength seqs, insids, outsids))

        return ()
-}

{- | Converts a statment into function ids to instructions. Note that this will
appropriately modify the monadic context.  -}
mplAsmCompileStmt ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m) =>
    MplAsmStmt x ->
    m [(CallIx, [Instr])]
mplAsmCompileStmt stmt = case stmt of
    Protocols tpconcspecs -> return []
    Coprotocols tpconcspecs -> return []
    Constructors tpseqspecs -> return []
    Destructors tpseqspecs -> return []

    Functions funs -> 
        for funs $ \(fname, args, coms) -> localMplAsmCompileSt $ do
            let overlapping = filter ((>1) . length) $ group $ sort $ args
            tell $ bool [_OverlappingDeclarations # overlapping] [] $ null overlapping 

            ~(Just (funid,_)) <- guse (symTab % symTabFuns % at fname)

            varStack .= reverse args
            instrs <- mplAsmComsToInstr coms
            return (funid, instrs)

    Processes procs -> 
        for procs $ \(pname, (seqs, ins, outs), coms) -> localMplAsmCompileSt $ do
            let overlapping = filter ((>1) . length) $ group $ sort $ seqs ++ ins ++ outs
            tell $ bool [_OverlappingDeclarations # overlapping] [] $ null overlapping 

            ~(Just (funid, (_, insids, outsids))) <- guse (symTab % symTabProcs % at pname)

            varStack .= reverse seqs
            channelTranslations .= Map.fromList (zip ins (map (Input,) insids) ++ zip outs (map (Output,) outsids))

            instrs <- mplAsmComsToInstr coms

            return $ (funid, instrs)

mplAsmComsToInstr ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m
    ) =>
    [MplAsmCom x] -> 
    m [Instr]
mplAsmComsToInstr = fmap concat . traverse mplAsmComToInstr 

{- | compiles a single asm instruction to an instruction for the AMPL machine -}
mplAsmComToInstr ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m
    ) =>
    MplAsmCom x -> 
    m [Instr]
mplAsmComToInstr = \case 
    -- assign the variable @v@ a value and store it
    CAssign _ v com -> do
        com' <- mplAsmComToInstr com
        varStack %= (v:)
        return $ com' ++ [_IStore # ()]
    CStore _ v -> do
        varStack %= (v:)
        return [_IStore # ()]
    -- load the variable @v@ so it is at the top of the stack
    CLoad _ v -> do
        ~(Just ix) <- lookupVarStack v
        return $ [_IAccess # ix]
    CRet _ -> return [_IRet # ()]
    CCall _ fname args -> do
        ~(Just (funid, numargs)) <- lookupFun fname
        ~(Just ixs) <- fmap sequenceA $ traverse lookupVarStack args
        tell $ bool [_IllegalFunCall # (fname, numargs, genericLength args)] [] $ genericLength args == numargs
        -- let ixs' = reverse ixs
        let ixs' = ixs
        -- return $ map (review _IAccess) ixs' ++ [_ICall # (funid, numargs)]
        return $ concatMap (\ix -> [_IAccess # ix, _IStore # ()]) ixs' ++ [_ICall # funid]
        -- TODO: Calling is broken

    CInt _ n  -> return [_IConst # (VInt n)]
    CChar _ n -> return [_IConst # (VChar n)]
    CBool _ n -> return [_IConst # (VBool n)]

    CEqInt _   -> return [_IEqInt # ()]
    CEqChar _  -> return [_IEqChar # ()]

    CLeqInt _  -> return [_ILeqInt # ()]
    CLeqChar _ -> return [_ILeqChar # ()]

    CAdd _ -> return [_IAddInt # ()]
    CSub _ -> return [_ISubInt # ()]
    CMul _ -> return [_IMulInt # ()]
    CConstructor _ typeandspec args -> do
        ~(Just (caseix, numargs)) <- lookupData typeandspec
        tell $ bool [ _IllegalConstructorCall # (typeandspec, numargs, genericLength args)] [] $ genericLength args == numargs
        ~(Just loadixs) <- fmap sequenceA $ traverse lookupVarStack args
        let loadixs' = reverse loadixs

        tell $ bool [_IllegalConstructorCall # (typeandspec, numargs, genericLength args)] [] $ genericLength args == numargs
        return $ map (review _IAccess) loadixs' ++ [_ICons # (caseix, numargs)]
        
    CDestructor _ typeandspec args v -> do
        ~(Just (caseix, numargs)) <- lookupCodata typeandspec
        ~(Just loadv) <- lookupVarStack v
        ~(Just loadixs) <- fmap sequenceA $ traverse lookupVarStack args
        let loadixs' = reverse loadixs
        tell $ bool [_IllegalDestructorCall # (typeandspec, numargs, genericLength args)] [] $ genericLength args == numargs
        return $ map (review _IAccess) loadixs ++ [_IAccess # loadv, _IDest # (caseix, numargs)]

    CCase _ caseon labelledcoms -> do
        ~(Just caseonix) <- lookupVarStack caseon

        let allsame = 
                groupBy ((==) `on` view typeAndSpecTy) 
                $ sortOn (view typeAndSpecTy) 
                $ map (view _1) labelledcoms
        tell $ bool [_NotAllSameCase # allsame] [] $ length allsame == 1

        instrs <- for labelledcoms $ \(tpandspec, args, coms) -> localMplAsmCompileSt $ do
            ~(Just (caseix, numargs)) <- lookupData tpandspec
            tell $ bool [_IllegalConstructorCall # (tpandspec, numargs, genericLength args)] [] $ genericLength args == numargs
            varStack %= (reverse args<>)
            coms' <-  mplAsmComsToInstr coms
            return coms'

        return $ [_IAccess # caseonix] ++ [_ICase # Arr.listArray (coerce @Int @CaseIx 0, coerce @Int @CaseIx $ length instrs - 1) instrs]

    CRecord _ labelledcoms -> do
        let allsame = 
                groupBy ((==) `on` view typeAndSpecTy) 
                $ sortOn (view typeAndSpecTy) 
                $ map (view _1) labelledcoms

        tell $ bool [_NotAllSameRecord # allsame] [] $ length allsame == 1

        -- duplicated above
        instrs <- for labelledcoms $ \(tpandspec, args, coms) -> localMplAsmCompileSt $ do
            ~(Just (caseix, numargs)) <- lookupCodata tpandspec
            tell $ bool [_IllegalDestructorCall # (tpandspec, numargs, genericLength args)] [] $ genericLength args == numargs
            varStack %= (reverse args<>)
            coms' <- localMplAsmCompileSt $ mplAsmComsToInstr coms
            return coms'
        return $ [_IRec # Arr.listArray (coerce @Int @CaseIx 0, coerce @Int @CaseIx $ length instrs - 1) instrs]
    CIf _ caseon thenc elsec -> do
        ~(Just caseonix) <- lookupVarStack caseon
        thecinstrs <- localMplAsmCompileSt $ mplAsmComsToInstr thenc
        elscinstrs <- localMplAsmCompileSt $ mplAsmComsToInstr elsec
        return $ [_IAccess # caseonix] ++ [_IIf # (thecinstrs, elscinstrs)]

    CTuple _ tuplelems -> do
        ~(Just tupleelemsix) <- fmap sequenceA $ traverse lookupVarStack tuplelems
        let tupleelemsix' = reverse tupleelemsix

        return $ map (review _IAccess) tupleelemsix' ++ [_ITuple # length tuplelems]

    CProj _ proj tuple -> do
        ~(Just tupleix) <- lookupVarStack tuple
        return $ [_IAccess # tupleix] ++ [_ITupleElem # coerce @Int @TupleIx proj]

    CGet _ v ch -> do
        -- since we are getting a new value, we need to immedaitely put it on the
        -- stack so that it corresponds to actually being put on the stack
        ~(Just (_pol, chid)) <- lookupCh ch
        varStack %= (v:)
        return $ [_IGet # chid, _IStore # ()]

    CPut _ v ch -> do
        ~(Just vix) <- lookupVarStack v
        ~(Just (_pol, chid)) <- lookupCh ch
        return $ [_IAccess # vix, _IPut # chid]

    CHPut _ tpspec ch -> do
        ~(Just (pol, chid)) <- lookupCh ch
        -- recall input polarity means you hput protocol...
        -- recall output polarity means you hput coprotocol...
        case pol of
            Input -> do
                ~(Just hcaseix) <- lookupProtocol tpspec
                return $ [_IHPut # (chid, hcaseix)]
            Output -> do
                ~(Just hcaseix) <- lookupCoprotocol tpspec
                return $ [_IHPut # (chid, hcaseix)]

        -- recall input polarity means you hcase protocol...
        -- recall output polarity means you hcase coprotocol...
    CHCase _ chcaseon labelledconccoms -> do
        let allsame = 
                groupBy ((==) `on` view typeAndSpecTy) 
                $ sortOn (view typeAndSpecTy)
                $ map (view _1) labelledconccoms
        tell $ bool [_NotAllSameHCase # allsame] [] $ length allsame == 1

        ~(Just (pol, chid)) <- lookupCh chcaseon 
        instrs <- for labelledconccoms $ \(tpandspec, coms) -> case pol of
            Input -> do
                ~(Just _hcaseix) <- lookupProtocol tpandspec
                comsinstrs <- mplAsmComsToInstr coms
                return comsinstrs
            Output -> do
                ~(Just _hcaseix) <- lookupCoprotocol tpandspec
                comsinstrs <- mplAsmComsToInstr coms
                return comsinstrs
        return 
            [_IHCase # 
                ( chid
                , Arr.listArray (coerce @Int @HCaseIx 0, coerce @Int @HCaseIx $ length instrs - 1) instrs)
            ]

    CSplit _ ch (lch, rch) -> do
        ~(Just (pol, chid)) <- lookupCh ch 
        lchid <- freshLocalChan
        rchid <- freshLocalChan
        channelTranslations % at lch ?= (pol, lchid)
        channelTranslations % at rch ?= (pol, rchid)

        return [_ISplit # (chid, (lchid, rchid)) ]

    -- TODO: probably should do an exhaustive fork check
    CFork _ ch ((ch0, with0, coms0), (ch1, with1, coms1)) -> do
        ~(Just (pol, chid)) <- lookupCh ch 
        ~(Just with0ids) <- fmap sequenceA $ traverse lookupCh with0 
        ~(Just with1ids) <- fmap sequenceA $ traverse lookupCh with1 

        ch0id <- freshLocalChan
        coms0instr <- localMplAsmCompileSt $ do
            channelTranslations %= flip Map.restrictKeys (Set.fromList with0)
            channelTranslations % at ch0 ?= (pol, ch0id)
            mplAsmComsToInstr coms0

        ch1id <- freshLocalChan
        coms1instr <- localMplAsmCompileSt $ do
            channelTranslations %= flip Map.restrictKeys (Set.fromList with1)
            channelTranslations % at ch1 ?= (pol, ch1id)
            mplAsmComsToInstr coms1

        return [_IFork # (chid, ((ch0id, Set.fromList (map snd with0ids), coms0instr), (ch1id, Set.fromList (map snd with1ids), coms1instr))) ]

    -- TODO: There is definately a bug here... recall we need the USER to 
    -- supply what polarity the plugged channels are, so we can't just ASSUME that
    -- the first one plug commands are output channels, then the next are input..
    -- this is WRONG and needs to be fixed in the future.
    CPlug _ plugs ((with0, coms0), (with1, coms1)) -> do
        plugsids <- traverse (const freshLocalChan) plugs
        ~(Just with0ids) <- fmap sequenceA $ traverse lookupCh with0 
        ~(Just with1ids) <- fmap sequenceA $ traverse lookupCh with1 
        let plugsandids = zip plugs plugsids

        coms0instrs <- localMplAsmCompileSt $ do
            for plugsandids $ \(ch, chid) ->  do
                channelTranslations %= flip Map.restrictKeys (Set.fromList with0)
                channelTranslations % at ch ?= (Output, chid)
            mplAsmComsToInstr coms0

        coms1instrs <- localMplAsmCompileSt $ do
            for plugsandids $ \(ch, chid) ->  do
                channelTranslations %= flip Map.restrictKeys (Set.fromList with1)
                channelTranslations % at ch ?= (Input, chid)
            mplAsmComsToInstr coms1

        return 
            [ _IPlug # 
                ( plugsids, 
                    ( (Set.fromList $ map snd with0ids, coms0instrs)
                    , (Set.fromList $ map snd with1ids, coms1instrs)
                    )
                )
            ]

    -- TODO: technically should do some polarity checks here
    CRun _ callp (seqs, ins, outs) -> do
        ~(Just ixseqs) <- fmap sequenceA $ traverse lookupVarStack seqs

        ~(Just insids) <- fmap sequenceA $ traverse lookupCh ins 
        ~(Just outssids) <- fmap sequenceA $ traverse lookupCh outs 

        ~(Just (callid, (numargs, callins, callouts))) <- lookupProc callp
        -- let ixseqs' = reverse ixseqs
        let ixseqs' = ixseqs
            -- instranslations = map (Input,) $ zip callins (map snd insids)
            -- outstranslations = map (Output,) $ zip callouts (map snd outssids)
            instranslations = zip callins (map snd insids)
            outstranslations = zip callouts (map snd outssids)
            translationmapping = instranslations ++ outstranslations 

        -- return $ map (review _IAccess) ixseqs' ++ [_IRun # (translationmapping, callid, numargs)]
        return $ concatMap (\ix -> [_IAccess # ix, _IStore # ()]) ixseqs' 
            ++ [_IRun # (coerce $ Map.fromList translationmapping, callid)]

    -- TODO: Technically, should do some polarity checking here
    CId _ (lch, rch) -> do
        ~(Just (_lpol, lchid)) <- lookupCh lch
        ~(Just (_rpol, rchid)) <- lookupCh rch
        return [_IId # (lchid, rchid)]

    -- TODO: should do some polarity checking and exhaustiveness checking here
    CRace _ phrases -> do
        phrasesinstrs <- for phrases $ \(ch, coms) -> localMplAsmCompileSt $ do
            ~(Just (_pol, chid)) <- lookupCh ch
            instrs <- mplAsmComsToInstr coms
            return (chid, instrs)
        return [_IRace # phrasesinstrs]

    CClose _ ch -> do
        ~(Just (_,chid)) <- lookupCh ch
        return [_IClose # chid]

    CHalt _ ch -> do
        ~(Just (_,chid)) <- lookupCh ch
        return [_IHalt # chid]

lookupVarStack ::
    ( MonadState (MplAsmCompileSt x) m
    , Eq (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe Int)
lookupVarStack v = do
    res <- guses varStack (elemIndex v)
    tell $ bool [_OutOfScopeVariable # v] [] $ isJust res
    return res

lookupFun ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe (CallIx, Int))
lookupFun v = do
    res <- guse (symTab % symTabFuns % at v ) 
    tell $ bool [_OutOfScopeFun # v] [] $ isJust res
    return res

lookupProc ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe (CallIx, (Int, [LocalChan], [LocalChan])))
lookupProc v = do
    res <- guse (symTab % symTabProcs % at v ) 
    tell $ bool [_OutOfScopeProc # v] [] $ isJust res
    return res

lookupData ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    TypeAndSpec x ->
    m (Maybe (CaseIx, Int))
lookupData tpspec@(TypeAndSpec tp spec) = do   
    res <- guse (symTab % symTabData % at tp % _Just % at spec % _Just)
    tell $ bool [_OutOfScopeData # tpspec] [] $ isJust res
    return res

lookupCodata ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    TypeAndSpec x ->
    m (Maybe (CaseIx, Int))
lookupCodata tpspec@(TypeAndSpec tp spec) = do   
    res <- guse (symTab % symTabCodata % at tp % _Just % at spec % _Just)
    tell $ bool [_OutOfScopeCodata # tpspec] [] $ isJust res
    return res

lookupProtocol ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    TypeAndSpec x ->
    m (Maybe HCaseIx)
lookupProtocol tpspec@(TypeAndSpec tp spec) = do
    res <- guse (symTab % symTabProtocol % at tp % _Just % at spec % _Just)
    tell $ bool [_OutOfScopeProtocol # tpspec] [] $ isJust res
    return res

lookupCoprotocol ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    TypeAndSpec x ->
    m (Maybe HCaseIx)
lookupCoprotocol tpspec@(TypeAndSpec tp spec) = do
    res <- guse (symTab % symTabCoprotocol % at tp % _Just % at spec % _Just)
    tell $ bool [_OutOfScopeCoprotocol # tpspec] [] $ isJust res
    return res

lookupCh ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe (Polarity, LocalChan))
lookupCh idp = do
    res <- guse (channelTranslations % at idp)
    tell $ bool [_OutOfScopeChannel # idp] [] $ isJust res
    return res

freshLocalChan ::
    MonadState (MplAsmCompileSt x) m =>
    m LocalChan
freshLocalChan = 
    uniqCounters % uniqLocalChan <<%= (coerce :: Int -> LocalChan) 
        . succ 
        . (coerce :: LocalChan -> Int)

{-
freshGlobalChanId ::
    MonadState (MplAsmCompileSt x) m =>
    m GlobalChanID
freshGlobalChanId = 
    uniqCounters % uniqGlobalChanId <<%= (coerce :: Int -> GlobalChanID) 
        . succ 
        . coerce @GlobalChanID @Int
-}

freshCallIx ::
    MonadState (MplAsmCompileSt x) m =>
    m CallIx
freshCallIx = 
    uniqCounters % uniqCallIx <<%= (coerce :: Int -> CallIx) 
        . succ 
        . coerce @CallIx @Int

