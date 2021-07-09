{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
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
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Function
import Data.Bool

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set  as Set

import Data.Array (Array)
import qualified Data.Array as Arr

import Control.Exception
import qualified Text.Show.Pretty as PrettyShow

mplAsmProgToInitMachState ::
    ( Ord (IdP x) 
    , Show (IdP x)
    , Show (IdP x)
    , AsCompileError err x 
    , HasName (IdP x)) =>
    -- | an assembly program
    MplAsmProg x -> 
    -- | Either: errors; or an array of the supercombinators and the ((ins, outs), main function instruction)
    Either [err] (MplMachSuperCombinators, (([LocalChan], [LocalChan]), [Instr]))
mplAsmProgToInitMachState prog = case runWriter res of
    (res', []) -> Right res'
    (_, errs) -> Left errs
  where
    res = flip evalStateT initMplAsmCompileSt $ do
        {- compilation of statements is straightforward -}

        -- first, put evreything in symbol table. see 'mplAsmCollectStmtsInSymTab' for why
        -- this is necessasry[
        mplAsmCollectStmtsInSymTab prog
        pp <- guse symTab
        -- traceM $ PrettyShow.ppShow pp 

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

        ~(Just mainf) <- case prog ^. mplAsmMain of
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
                return $ Just (funid, ((insids, outsids), maininstrs))
            Nothing -> tell [_NoMainFunction # ()] >> return Nothing

        let allfuns = second snd mainf : funs
            
        return 
            ( coerce @(Array CallIx [Instr]) @MplMachSuperCombinators  (Arr.array (coerce @Int @CallIx 0, coerce @Int @CallIx $ length allfuns - 1) allfuns)
            , snd mainf)

{- | collects all the elements into the symbol table. Note: this is required to permit mutually
 - recursive declarations between data / codta and functions, etc. -}
mplAsmCollectStmtsInSymTab ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , Show (IdP x)
    , HasName (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m
    ) =>
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

            symTab % symTabFuns % at fname ?= (funid, args)

    {-
    procSymTab decs =
        for_ decs $ \(pname, (seqs, ins, outs), _) -> do
            uniqCounters % uniqLocalChan .= (coerce (0 :: Int) :: LocalChan)

            insids <- traverse (const freshLocalChan) ins
            outsids <- traverse (const freshLocalChan) outs

            funid <- freshCallIx 
            symTab % symTabProcs % at pname ?= (funid, (length seqs, insids, outsids))
    -}
    procSymTab decs =
        for_ decs $ \(pname, (seqs, ins, outs), _) -> do
            uniqCounters % uniqLocalChan .= (coerce (0 :: Int) :: LocalChan)

            insids <- traverse freshLocalChanOrServiceChanInput ins
            outsids <- traverse freshLocalChanOrServiceChanOutput outs

            funid <- freshCallIx 
            symTab % symTabProcs % at pname ?= (funid, (seqs, insids, outsids))

{- | Converts a statment into function ids to instructions. Note that this will
appropriately modify the monadic context.  -}
mplAsmCompileStmt ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , Show (IdP x)
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
        for funs $ \(fname, args, coms) -> localMplAsmCompileSt id $ do
            let overlapping = filter ((>1) . length) $ group $ sort $ args
            tell $ bool [_OverlappingDeclarations # overlapping] [] $ null overlapping 

            ~(Just (funid,_)) <- guse (symTab % symTabFuns % at fname)

            varStack .= reverse args
            instrs <- mplAsmComsToInstr coms
            return (funid, instrs)

    Processes procs -> 
        for procs $ \(pname, (seqs, ins, outs), coms) -> localMplAsmCompileSt id $ do
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
    , Show (IdP x)
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
    , Show (IdP x)
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
        ~(Just (funid, fargs)) <- lookupFun fname
        tell $ bool [_IllegalFunCall # (fname, length fargs, length args)] [] $ length args == length fargs
        accesses <- localMplAsmCompileSt id $ fmap concat $ for (zip fargs args) $ \(fv, v) -> do  
            ~(Just ix) <- lookupVarStack v
            varStack %= (fv:)
            return [_IAccess # ix, _IStore # ()]

        -- return $ concatMap (\ix -> [_IAccess # ix, _IStore # ()]) ixs' ++ [_ICall # funid]
        return $ accesses ++ [_ICall # (funid, length fargs)]
        -- TODO: Calling is broken

    CInt _ n  -> return [_IConst # (VInt n)]
    CChar _ n -> return [_IConst # (VChar n)]
    CBool _ n -> return [_IConst # (VBool n)]

    CEqInt _   -> return [_IEqInt # ()]
    CEqChar _  -> return [_IEqChar # ()]
    CEqBool _  -> return [_IEqBool # ()]

    CLeqInt _  -> return [_ILeqInt # ()]
    CLtInt _  -> return [_ILtInt # ()]
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

        instrs <- for labelledcoms $ \(tpandspec, args, coms) -> localMplAsmCompileSt id $ do
            ~(Just (caseix, numargs)) <- lookupData tpandspec
            tell $ bool [_IllegalConstructorCall # (tpandspec, numargs, genericLength args)] [] $ genericLength args == numargs
            varStack %= (args<>)
            coms' <-  mplAsmComsToInstr coms
            return (caseix, coms')

        return $ [_IAccess # caseonix] ++ [_ICase # Arr.array (coerce @Int @CaseIx 0, coerce @Int @CaseIx $ length instrs - 1) instrs]

    CRecord _ labelledcoms -> do
        let allsame = 
                groupBy ((==) `on` view typeAndSpecTy) 
                $ sortOn (view typeAndSpecTy) 
                $ map (view _1) labelledcoms

        tell $ bool [_NotAllSameRecord # allsame] [] $ length allsame == 1

        -- duplicated above
        instrs <- for labelledcoms $ \(tpandspec, args, coms) -> localMplAsmCompileSt id $ do
            ~(Just (caseix, numargs)) <- lookupCodata tpandspec
            tell $ bool [_IllegalDestructorCall # (tpandspec, numargs, genericLength args)] [] $ genericLength args == numargs
            varStack %= (args<>)
            coms' <- localMplAsmCompileSt id $ mplAsmComsToInstr coms
            return (caseix, coms')
        return $ [_IRec # Arr.array (coerce @Int @CaseIx 0, coerce @Int @CaseIx $ length instrs - 1) instrs]

    CIf _ caseon thenc elsec -> do
        ~(Just caseonix) <- lookupVarStack caseon
        thecinstrs <- localMplAsmCompileSt id $ mplAsmComsToInstr thenc
        elscinstrs <- localMplAsmCompileSt id $ mplAsmComsToInstr elsec
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
        lkup <- lookupCh ch
        let Just (pol,chid) = lkup
        -- recall input polarity means you hput coprotocol...
        -- recall output polarity means you hput protocol...
        if isJust lkup 
            then case pol of
                Output -> do
                    ~(Just hcaseix) <- lookupProtocol tpspec
                    return $ [_IHPut # (chid, hcaseix)]
                Input -> do
                    ~(Just hcaseix) <- lookupCoprotocol tpspec
                    return $ [_IHPut # (chid, hcaseix)]
            else return []

    CSHPut _ sv ch -> do
        lkup <- lookupCh ch
        let Just (pol, chid) = lkup
        -- recall input polarity means you hput coprotocol...
        -- recall output polarity means you hput protocol...
        if isJust lkup
            then case pol of
                Input | Just sinstr <- maybeTermService sv -> return [ _ISHPut # (chid, sinstr) ]
                Output | Just sinstr <- maybeTermService sv -> return [ _ISHPut # (chid, sinstr) ]
                -- Output | Just sinstr <- maybeTermService sv -> return [ _ISHPut # (chid, sinstr) ]

                _ -> tell [_IllegalServiceCall # (sv, ch)] >> return []
            else return []


        -- recall input polarity means you hcase protocol...
        -- recall output polarity means you hcase coprotocol...
    CHCase _ chcaseon labelledconccoms -> do
        let allsame = 
                groupBy ((==) `on` view typeAndSpecTy) 
                $ sortOn (view typeAndSpecTy)
                $ map (view _1) labelledconccoms
        tell $ bool [_NotAllSameHCase # allsame] [] $ length allsame == 1

        lkup <- lookupCh chcaseon 
        let Just (pol, chid) = lkup
        if isJust lkup
            then do
                instrs <- for labelledconccoms $ \(tpandspec, coms) -> localMplAsmCompileSt id $ case pol of
                    Input -> do
                        ~(Just hcaseix) <- lookupProtocol tpandspec
                        comsinstrs <- mplAsmComsToInstr coms
                        return (hcaseix, comsinstrs)
                    Output -> do
                        ~(Just hcaseix) <- lookupCoprotocol tpandspec
                        comsinstrs <- mplAsmComsToInstr coms
                        return (hcaseix, comsinstrs)
                return 
                    [_IHCase # 
                        ( chid
                        , Arr.array (coerce @Int @HCaseIx 0, coerce @Int @HCaseIx $ length instrs - 1) instrs)
                    ]
            else return []

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
        coms0instr <- localMplAsmCompileSt id $ do
            channelTranslations %= flip Map.restrictKeys (Set.fromList with0)
            channelTranslations % at ch0 ?= (pol, ch0id)
            mplAsmComsToInstr coms0

        ch1id <- freshLocalChan
        coms1instr <- localMplAsmCompileSt id $ do
            channelTranslations %= flip Map.restrictKeys (Set.fromList with1)
            channelTranslations % at ch1 ?= (pol, ch1id)
            mplAsmComsToInstr coms1

        return [_IFork # (chid, ((ch0id, Set.fromList (map snd with0ids), coms0instr), (ch1id, Set.fromList (map snd with1ids), coms1instr))) ]

    -- TODO: There is definately a bug here... recall we need the USER to 
    -- supply what polarity the plugged channels are, so we can't just ASSUME that
    -- the first one plug commands are output channels, then the next are input..
    -- this is WRONG and needs to be fixed in the future.
    CPlug _ plugs (((withsins0, withsouts0), coms0), ((withsins1, withsouts1), coms1)) -> do
        plugsids <- traverse (const freshLocalChan) plugs
        let plugsandids = zip plugs plugsids
            plugsandpolsids = Map.fromList $ map (second (Output,)) plugsandids
            localPlugWiths = localMplAsmCompileSt (over channelTranslations (`Map.union` plugsandpolsids))

        ~(Just withsins0ids) <- localPlugWiths $ fmap sequenceA $ traverse lookupCh $ withsins0 
        ~(Just withsouts0ids) <- localPlugWiths $ fmap sequenceA $ traverse lookupCh $ withsouts0 
        ~(Just withsins1ids) <- localPlugWiths $ fmap sequenceA $ traverse lookupCh $ withsins1 
        ~(Just withsouts1ids) <- localPlugWiths $ fmap sequenceA $ traverse lookupCh $ withsouts1 


        coms0instrs <- localMplAsmCompileSt id $ do
            for plugsandids $ \(ch, chid) ->  do
                -- channelTranslations %= flip Map.restrictKeys (Set.fromList with0)
                channelTranslations % at ch ?= (Output, chid)
            mplAsmComsToInstr coms0

        coms1instrs <- localMplAsmCompileSt id $ do
            for plugsandids $ \(ch, chid) ->  do
                -- channelTranslations %= flip Map.restrictKeys (Set.fromList with1)
                channelTranslations % at ch ?= (Input, chid)
            mplAsmComsToInstr coms1

        return 
            [ _IPlug # 
                ( plugsids, 
                    ( ((Set.fromList $ map snd withsins0ids, Set.fromList $ map snd withsouts0ids), coms0instrs)
                    , ((Set.fromList $ map snd withsins1ids, Set.fromList $ map snd withsouts1ids), coms1instrs)
                    )
                )
            ]

    {-
    CPlug _ plugs ((with0, coms0), (with1, coms1)) -> do
        plugsids <- traverse (const freshLocalChan) plugs
        ~(Just with0ids) <- fmap sequenceA $ traverse lookupCh with0 
        ~(Just with1ids) <- fmap sequenceA $ traverse lookupCh with1 
        let plugsandids = zip plugs plugsids

        coms0instrs <- localMplAsmCompileSt id $ do
            for plugsandids $ \(ch, chid) ->  do
                -- channelTranslations %= flip Map.restrictKeys (Set.fromList with0)
                channelTranslations % at ch ?= (Output, chid)
            mplAsmComsToInstr coms0

        coms1instrs <- localMplAsmCompileSt id $ do
            for plugsandids $ \(ch, chid) ->  do
                -- channelTranslations %= flip Map.restrictKeys (Set.fromList with1)
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
    -}

    -- TODO: technically should do some polarity checks here, and need to check arity
    CRun _ callp (seqs, ins, outs) -> do
        ~(Just ixseqs) <- fmap sequenceA $ traverse lookupVarStack seqs


        ~(Just insids) <- fmap sequenceA $ traverse lookupCh ins 
        ~(Just outssids) <- fmap sequenceA $ traverse lookupCh outs 

        -- ~(Just (callid, (pargs, callins, callouts))) <- lookupProc callp
        proclkup <- lookupProc callp
        let ~(Just (callid, (pargs, callins, callouts))) = proclkup

        -- let ixseqs' = reverse ixseqs
        let ixseqs' = ixseqs
            -- instranslations = map (Input,) $ zip callins (map snd insids)
            -- outstranslations = map (Output,) $ zip callouts (map snd outssids)
            instranslations = zip callins (map snd insids)
            outstranslations = zip callouts (map snd outssids)
            translationmapping = instranslations ++ outstranslations 

        if isJust proclkup
            then do
                accesses <- localMplAsmCompileSt id $ fmap concat $ for (zip pargs seqs) $ \(fv, v) -> do  
                    ~(Just ix) <- lookupVarStack v
                    varStack %= (fv:)
                    return [_IAccess # ix, _IStore # ()]

                -- return $ map (review _IAccess) ixseqs' ++ [_IRun # (translationmapping, callid, numargs)]
                return $ accesses ++ [_IRun # (coerce $ Map.fromList translationmapping, callid, length seqs)]
            else return []

    -- TODO: Technically, should do some polarity checking here
    CId _ (lch, rch) -> do
        ~(Just (_lpol, lchid)) <- lookupCh lch
        ~(Just (_rpol, rchid)) <- lookupCh rch
        return [_IId # (lchid, rchid)]

    -- TODO: should do some polarity checking and exhaustiveness checking here
    CRace _ phrases -> do
        phrasesinstrs <- for phrases $ \(ch, coms) -> localMplAsmCompileSt id $ do
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
    m (Maybe (CallIx, [IdP x]))
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
    m (Maybe (CallIx, ([IdP x], [LocalChan], [LocalChan])))
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

{- | returns a local chan or service chan based on the name. (note that these built ins depend on the polarity) -}
freshLocalChanOrServiceChanOutput ::
    ( MonadState (MplAsmCompileSt x) m 
    , HasName (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m) =>
    IdP x ->
    m LocalChan
freshLocalChanOrServiceChanOutput ident 
    | '_':rst <- ident ^. name % coerced @Name @String = go rst
    | otherwise = freshLocalChan
  where
    go sv 
        {-
        -- int terminal?
        | "INTTERMINAL" `isPrefixOf` sv = freshServiceChan
        -- char terminal
        | "CHARTERMINAL" `isPrefixOf` sv = freshServiceChan
        | otherwise = tell [_UnknownInputService # ident] >> freshLocalChan
        -}
        | otherwise = freshOutputServiceChan

freshLocalChanOrServiceChanInput ::
    ( MonadState (MplAsmCompileSt x) m 
    , HasName (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m) =>
    IdP x ->
    m LocalChan
freshLocalChanOrServiceChanInput ident 
    | '_':rst <- ident ^. name % coerced @Name @String = go rst
    | otherwise = freshLocalChan
  where
    go sv 
        {-
        -- int console? 
        | "CONSOLE" `isPrefixOf` sv = return $ LocalChan (-1)
        -- char console? 
        | "CCONSOLE" `isPrefixOf` sv = return $ LocalChan (-2)
        -}
        | otherwise = freshInputServiceChan

freshInputServiceChan ::
    MonadState (MplAsmCompileSt x) m =>
    m LocalChan
freshInputServiceChan = 
    uniqCounters % uniqInputServiceChan <<%= (coerce :: Int -> LocalChan) 
        . pred 
        . pred 
        . (coerce :: LocalChan -> Int)

freshOutputServiceChan ::
    MonadState (MplAsmCompileSt x) m =>
    m LocalChan
freshOutputServiceChan = 
    uniqCounters % uniqOutputServiceChan <<%= (coerce :: Int -> LocalChan) 
        . pred 
        . pred 
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

