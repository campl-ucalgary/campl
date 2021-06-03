{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module MplAsmPasses.Compile.Compile where

import AMPL
import AMPLTypes

import MplAsmAST.MplAsmCore
import MplAsmPasses.Compile.CompileErrors
import MplAsmPasses.Compile.Stack

import Optics 
import Optics.State.Operators

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Traversable

import Data.Coerce
import Data.Word
import Data.List
import Data.Maybe
import Data.Function
import Data.Bool

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set  as Set

mplAsmProgToInitMachState ::
    ( Ord (IdP x) 
    , AsCompileError err x ) =>
    MplAsmProg x -> 
    Either [err] InitAMPLMachState 
mplAsmProgToInitMachState = undefined

{- | Converts a statment into function ids to instructions. Note that this will
appropriately modify the monadic context.  -}
mplAsmCompileStmt ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m) =>
    MplAsmStmt x ->
    m [(FunID, [Instr])]
mplAsmCompileStmt stmt = case stmt of
    Protocols tpconcspecs -> do
        let overlapping = group $ map (view typeAndConcSpecsType) tpconcspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping == 1

        for tpconcspecs $ \(TypeAndConcSpecs tp handles) -> do
            let handles' = zip handles (coerce [0 :: Word ..] :: [HCaseIx])
            symTabProtocol % at tp ?= Map.fromList handles'
        return []
    Coprotocols tpconcspecs -> do
        let overlapping = group $ map (view typeAndConcSpecsType) tpconcspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping == 1

        for tpconcspecs $ \(TypeAndConcSpecs tp handles) -> do
            let handles' = zip handles (coerce [0 :: Word ..] :: [HCaseIx])
            symTabCoprotocol % at tp ?= Map.fromList handles'
        return []
    Constructors tpseqspecs -> do
        let overlapping = group $ map (view typeAndSeqSpecsType) tpseqspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping == 1

        for tpseqspecs $ \(TypeAndSeqSpecs tp handles) -> do
            let handles' = map (\((idp, numargs),caseix) -> (idp, (caseix,numargs))) 
                    $ zip handles (coerce [0 :: Word ..] :: [CaseIx])
            symTabData % at tp ?= Map.fromList handles'
        return []

    Destructors tpseqspecs -> do
        let overlapping = group $ map (view typeAndSeqSpecsType) tpseqspecs 
        tell $ bool [_OverlappingDeclarations # overlapping] [] $ length overlapping == 1

        for tpseqspecs $ \(TypeAndSeqSpecs tp handles) -> do
            let handles' = map (\((idp, numargs),caseix) -> (idp, (caseix,numargs))) 
                    $ zip handles (coerce [0 :: Word ..] :: [CaseIx])
            symTabCodata % at tp ?= Map.fromList handles'
        return []
    Functions funs -> for funs $ \(fname, args, coms) -> localMplAsmCompileSt $ do
        funid <- freshFunId
        varStack .= reverse args
        instrs <- mplAsmComsToInstr coms
        return (funid, instrs)

    Processes procs -> for procs $ \(pname, (seqs, ins, outs), coms) -> localMplAsmCompileSt $ do
        uniqLocalChanId .= (coerce (0 :: ChannelIdRep) :: LocalChanID)

        funid <- freshFunId
        varStack .= reverse seqs

        insids <- traverse (const freshLocalChanId) ins
        outsids <- traverse (const freshLocalChanId) outs
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
    CAssign v com -> do
        com' <- mplAsmComToInstr com
        varStack %= (v:)
        return $ com' ++ [iStore]
    -- load the variable @v@ so it is at the top of the stack
    CLoad v -> do
        ~(Just ix)<- lookupVarStack v
        return $ [iAccess ix]
    CRet -> return [iRet]
    CCall fname args -> do
        ~(Just (funid, numargs)) <- lookupFun fname
        ~(Just ixs) <- fmap sequenceA $ traverse lookupVarStack args
        tell $ bool [_IllegalFunCall # (fname, numargs, genericLength args)] [] $ genericLength args == numargs
        let ixs' = reverse ixs
        return $ map iAccess ixs' ++ [iCall funid numargs]
    CInt n -> return [iConst (VInt n)]
    CChar n -> return [iConst (VChar n)]
    CEqInt -> return [iEq]
    CEqChar -> return [iEq]
    CLeqInt -> return [iLeq]
    CLeqChar -> return [iLeq]
    CAdd -> return [iAddInt]
    CSub -> return [iSubInt]
    CMul -> return [iMulInt]
    CConstructor typeandspec args -> do
        ~(Just (caseix, numargs)) <- lookupData typeandspec
        tell $ bool [ _IllegalConstructorCall # (typeandspec, numargs, genericLength args)] [] $ genericLength args == numargs
        ~(Just loadixs) <- fmap sequenceA $ traverse lookupVarStack args
        let loadixs' = reverse loadixs

        tell $ bool [_IllegalConstructorCall # (typeandspec, numargs, genericLength args)] [] $ genericLength args == numargs
        return $ map iAccess loadixs' ++ [iCons caseix numargs]
        
    CDestructor typeandspec args v -> do
        ~(Just (caseix, numargs)) <- lookupCodata typeandspec
        ~(Just loadv) <- lookupVarStack v
        ~(Just loadixs) <- fmap sequenceA $ traverse lookupVarStack args
        let loadixs' = reverse loadixs
        tell $ bool [_IllegalDestructorCall # (typeandspec, numargs, genericLength args)] [] $ genericLength args == numargs
        return $ map iAccess loadixs ++ [iAccess loadv, iDest caseix numargs]

    CCase caseon labelledcoms -> do
        ~(Just caseonix) <- lookupVarStack caseon

        let allsame = groupBy ((==) `on` view typeAndSpecTy) $ map (view _1) labelledcoms
        tell $ bool [_NotAllSameCase # allsame] [] $ length allsame == 1

        instrs <- for labelledcoms $ \(tpandspec, args, coms) -> localMplAsmCompileSt $ do
            ~(Just (caseix, numargs)) <- lookupData tpandspec
            tell $ bool [_IllegalConstructorCall # (tpandspec, numargs, genericLength args)] [] $ genericLength args == numargs
            varStack %= (reverse args<>)
            coms' <-  mplAsmComsToInstr coms
            return coms'

        return $ [iAccess caseonix] ++ [iCase instrs]

    CRecord labelledcoms -> do
        let allsame = groupBy ((==) `on` view typeAndSpecTy) $ map (view _1) labelledcoms
        tell $ bool [_NotAllSameRecord # allsame] [] $ length allsame == 1

        -- duplicated above
        instrs <- for labelledcoms $ \(tpandspec, args, coms) -> localMplAsmCompileSt $ do
            ~(Just (caseix, numargs)) <- lookupCodata tpandspec
            tell $ bool [_IllegalDestructorCall # (tpandspec, numargs, genericLength args)] [] $ genericLength args == numargs
            varStack %= (reverse args<>)
            coms' <- localMplAsmCompileSt $ mplAsmComsToInstr coms
            return coms'
        return $ [iRec instrs]
    CIf caseon thenc elsec -> do
        ~(Just caseonix) <- lookupVarStack caseon
        thecinstrs <- localMplAsmCompileSt $ mplAsmComsToInstr thenc
        elscinstrs <- localMplAsmCompileSt $ mplAsmComsToInstr elsec
        return $ [iAccess caseonix] ++ [iIf thecinstrs elscinstrs]

    CTuple tuplelems -> do
        ~(Just tupleelemsix) <- fmap sequenceA $ traverse lookupVarStack tuplelems
        let tupleelemsix' = reverse tupleelemsix

        return $ map iAccess tupleelemsix' ++ [iTuple $ genericLength tuplelems]

    CProj proj tuple -> do
        ~(Just tupleix) <- lookupVarStack tuple
        return $ [iAccess tupleix] ++ [iTupleElem $ coerce proj]

    CGet v ch -> do
        -- since we are getting a new value, we need to immedaitely put it on the
        -- stack so that it corresponds to actually being put on the stack
        ~(Just (_pol, chid)) <- lookupCh ch
        varStack %= (v:)
        return $ [iGet chid, iStore]

    CPut v ch -> do
        ~(Just vix) <- lookupVarStack v
        ~(Just (_pol, chid)) <- lookupCh ch
        return $ [iAccess vix, iPut chid]

    CHPut tpspec ch -> do
        ~(Just (pol, chid)) <- lookupCh ch
        -- recall input polarity means you hput protocol...
        -- recall output polarity means you hput coprotocol...
        case pol of
            Input -> do
                ~(Just hcaseix) <- lookupProtocol tpspec
                return $ [iHPut chid hcaseix]
            Output -> do
                ~(Just hcaseix) <- lookupCoprotocol tpspec
                return $ [iHPut chid hcaseix]

        -- recall input polarity means you hcase protocol...
        -- recall output polarity means you hcase coprotocol...
    CHCase chcaseon labelledconccoms -> do
        let allsame = groupBy ((==) `on` view typeAndSpecTy) $ map (view _1) labelledconccoms
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
        return [iHCase chid instrs]

    CSplit ch (lch, rch) -> do
        ~(Just (pol, chid)) <- lookupCh ch 
        lchid <- freshLocalChanId
        rchid <- freshLocalChanId
        channelTranslations % at lch ?= (pol, lchid)
        channelTranslations % at rch ?= (pol, rchid)

        return [iSplit chid (lchid, rchid)]

    -- TODO: probably should do an exhaustive fork check
    CFork ch ((ch0, with0, coms0), (ch1, with1, coms1)) -> do
        ~(Just (pol, chid)) <- lookupCh ch 
        ~(Just with0ids) <- fmap sequenceA $ traverse lookupCh with0 
        ~(Just with1ids) <- fmap sequenceA $ traverse lookupCh with1 

        ch0id <- freshLocalChanId
        coms0instr <- localMplAsmCompileSt $ do
            channelTranslations %= flip Map.restrictKeys (Set.fromList with0)
            channelTranslations % at ch0 ?= (pol, ch0id)
            mplAsmComsToInstr coms0

        ch1id <- freshLocalChanId
        coms1instr <- localMplAsmCompileSt $ do
            channelTranslations %= flip Map.restrictKeys (Set.fromList with1)
            channelTranslations % at ch1 ?= (pol, ch1id)
            mplAsmComsToInstr coms1

        return [iFork chid ((ch0id, map snd with0ids, coms0instr), (ch1id, map snd with1ids, coms1instr)) ]

    -- TODO: There is definately a bug here... recall we need the USER to 
    -- supply what polarity the plugged channels are, so we can't just ASSUME that
    -- the first one plug commands are output channels, then the next are input..
    -- this is WRONG and needs to be fixed in the future.
    CPlug plugs ((with0, coms0), (with1, coms1)) -> do
        plugsids <- traverse (const freshLocalChanId) plugs
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

        return [iPlug plugsids ((map snd with0ids, coms0instrs), (map snd with1ids, coms1instrs))]

    -- TODO: technically should do some polarity checks here
    CRun callp (seqs, ins, outs) -> do
        ~(Just ixseqs) <- fmap sequenceA $ traverse lookupVarStack seqs

        ~(Just insids) <- fmap sequenceA $ traverse lookupCh ins 
        ~(Just outssids) <- fmap sequenceA $ traverse lookupCh outs 

        ~(Just (callid, (numargs, callins, callouts))) <- lookupProc callp
        let ixseqs' = reverse ixseqs
            instranslations = map (Input,) $ zip callins (map snd insids)
            outstranslations = map (Output,) $ zip callouts (map snd outssids)
            translationmapping = instranslations ++ outstranslations 

        return $ map iAccess ixseqs' ++ [iRun translationmapping callid numargs]

    -- TODO: Technically, should do some polarity checking here
    CId (lch, rch) -> do
        ~(Just (_lpol, lchid)) <- lookupCh lch
        ~(Just (_rpol, rchid)) <- lookupCh rch
        return [iId lchid rchid]
            

    -- TODO: should do some polarity checking and exhaustiveness checking here
    CRace phrases -> do
        phrasesinstrs <- for phrases $ \(ch, coms) -> localMplAsmCompileSt $ do
            ~(Just (_pol, chid)) <- lookupCh ch
            instrs <- mplAsmComsToInstr coms
            return (chid, instrs)
        return [iRace phrasesinstrs]
        

lookupVarStack ::
    ( MonadState (MplAsmCompileSt x) m
    , Eq (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe Word)
lookupVarStack v = do
    res <- fmap (fmap fromIntegral) $ guses varStack (elemIndex v)
    tell $ bool [_OutOfScopeVariable # v] [] $ isJust res
    return res

lookupFun ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe (FunID, Word))
lookupFun v = do
    res <- guse (symTabFuns % at v ) 
    tell $ bool [_OutOfScopeFun # v] [] $ isJust res
    return res

lookupProc ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe (FunID, (Word, [LocalChanID], [LocalChanID])))
lookupProc v = do
    res <- guse (symTabProcs % at v ) 
    tell $ bool [_OutOfScopeProc # v] [] $ isJust res
    return res

lookupData ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    TypeAndSpec x ->
    m (Maybe (CaseIx, Word))
lookupData tpspec@(TypeAndSpec tp spec) = do   
    res <- guse (symTabData % at tp % _Just % at spec % _Just)
    tell $ bool [_OutOfScopeData # tpspec] [] $ isJust res
    return res

lookupCodata ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    TypeAndSpec x ->
    m (Maybe (CaseIx, Word))
lookupCodata tpspec@(TypeAndSpec tp spec) = do   
    res <- guse (symTabCodata % at tp % _Just % at spec % _Just)
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
    res <- guse (symTabProtocol % at tp % _Just % at spec % _Just)
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
    res <- guse (symTabCoprotocol % at tp % _Just % at spec % _Just)
    tell $ bool [_OutOfScopeCoprotocol # tpspec] [] $ isJust res
    return res

lookupCh ::
    ( MonadState (MplAsmCompileSt x) m
    , Ord (IdP x)
    , AsCompileError err x
    , MonadWriter [err] m ) =>
    IdP x ->
    m (Maybe (Polarity, LocalChanID))
lookupCh idp = do
    res <- guse (channelTranslations % at idp)
    tell $ bool [_OutOfScopeChannel # idp] [] $ isJust res
    return res


freshLocalChanId ::
    MonadState (MplAsmCompileSt x) m =>
    m LocalChanID
freshLocalChanId = 
    uniqLocalChanId <<%= (coerce :: ChannelIdRep -> LocalChanID) 
        . succ 
        . (coerce :: LocalChanID -> ChannelIdRep)

freshFunId ::
    MonadState (MplAsmCompileSt x) m =>
    m FunID
freshFunId = 
    uniqFunId <<%= (coerce :: Word -> FunID) 
        . succ 
        . (coerce :: FunID -> Word)
