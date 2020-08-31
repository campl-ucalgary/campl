{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Renamer.Rename where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.Renamer.RenameUtils
import MplPasses.Renamer.RenameSym
import MplPasses.Renamer.RenameErrors
import MplPasses.Renamer.RenameObj 
import MplPasses.Renamer.RenameType 
import MplPasses.Renamer.RenamePatt 
import MplPasses.Renamer.RenameCmdFreeVars 
import MplPasses.Env

import Control.Monad.State
import Control.Monad.Writer

import MplUtil.UniqueSupply

import Data.Bool
import Data.Maybe
import Data.List
import Data.Functor.Foldable (Base, cata)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable
import Control.Arrow

import Debug.Trace

runRename' ::
    AsRenameErrors err =>
    (TopLevel, UniqueSupply) ->
    MplProg MplParsed ->
    Either [err] (MplProg MplRenamed)
runRename' (top, sup) = 
    \case 
        (res, []) -> Right res
        (_, errs) -> Left errs
    . runWriter 
    . (`evalStateT` (_Env # (top, sup, mempty, mempty)))
    . runRename

runRename ::
    Rename (MplProg MplParsed) (MplProg MplRenamed)
runRename (MplProg stmts) = 
    MplProg <$> traverse renameStmt stmts

renameStmt ::   
    Rename (MplStmt MplParsed) (MplStmt MplRenamed)
renameStmt (MplStmt defns wheres) = do
    tell $ overlappingDeclarations 
        $ foldMap (NE.toList .  mplStmtTopLevelIdents) wheres 
    gbl <- guse envGbl 

    wheres' <- traverse renameStmt wheres

    envGbl .= collectSymTab wheres'<> gbl

    defns' <- NE.fromList <$> renameDefns (NE.toList defns)

    envGbl .= collectSymTab defns' <> gbl

    return $ MplStmt defns' wheres'

renameDefns ::
    Rename 
        [MplDefn MplParsed]
        [MplDefn MplRenamed]
renameDefns (defn : defns) = do
    uniqsup <- freshUniqueSupply

    --rec defn' <- (`evalStateT` (_RenameEnv # (uniqsup, symtab)))
    rec defn' <- envLcl .= symtab >> renameDefn defn
        envGbl %= (collectSymTab defn' <>)
        defns' <- renameDefns defns
        symtab <- guse envGbl

    return (defn' : defns')
renameDefns [] = return []

renameDefn ::
    Rename (MplDefn MplParsed) (MplDefn MplRenamed)
renameDefn (ObjectDefn obj) = ObjectDefn <$> case obj of
    SeqObjDefn obj -> SeqObjDefn <$> case obj of
        DataDefn n -> DataDefn <$> renameTypeClauseSpine n
        CodataDefn n -> CodataDefn <$> renameTypeClauseSpine n
    ConcObjDefn obj -> ConcObjDefn <$> case obj of
        ProtocolDefn n -> ProtocolDefn <$> renameTypeClauseSpine n
        CoprotocolDefn n -> CoprotocolDefn <$> renameTypeClauseSpine n
renameDefn (FunctionDefn (MplFunction name funtype defn)) = do
    name' <- tagIdentP name

    funtype' <- flip (maybe (return Nothing)) funtype $ \(froms, to) -> do
        lcl <- guse envLcl
        (bds, froms') <- unzip <$> traverse renameType froms
        (bd, to') <- renameType to 
        envLcl .= lcl
        return $ Just (bd ++ fold bds, froms', to')
    
    defn' <- traverse renamePattsExpr defn

    return $ FunctionDefn $ _MplFunction # (name', funtype', defn')

renameDefn (ProcessDefn (MplProcess name proctype defn)) = do
    name' <- tagIdentP name
    proctype' <- flip (maybe (return Nothing)) proctype $ \(seqs, ins, outs) -> do
        lcl <- guse envLcl
        (bds0, seqs') <- unzip <$> traverse renameType seqs
        (bds1, ins') <- unzip <$> traverse renameType ins
        (bds2, outs') <- unzip <$> traverse renameType outs
        envLcl .= lcl
        return $ Just (fold (bds0 <> bds1 <> bds2), seqs', ins', outs')

    defn' <- traverse renameProcBodyPhrase defn

    return $ ProcessDefn $ _MplProcess # (name', proctype', defn')

renamePattsExpr ::   
    Rename 
        ([MplPattern MplParsed], MplExpr MplParsed) 
        ([MplPattern MplRenamed], MplExpr MplRenamed)
renamePattsExpr (patts, expr) = do
        symtab <- guse envLcl
        tell $ overlappingDeclarations 
            $ concatMap collectPVarIdPs patts
        patts' <- traverse (splitUniqueSupply . renamePattern) patts
        expr' <- splitUniqueSupply (renameExpr expr)
        envLcl .= symtab
        return (patts', expr')

renameProcBodyPhrase :: 
    Rename
        ( ([MplPattern MplParsed], [ChP MplParsed], [ChP MplParsed])
        , NonEmpty (MplCmd MplParsed)) 
        ( ([MplPattern MplRenamed], [ChP MplRenamed], [ChP MplRenamed])
        , NonEmpty (MplCmd MplRenamed)) 
renameProcBodyPhrase ((patts, ins, outs), cmds) = do
    symtab <- guse envLcl

    patts' <- traverse (splitUniqueSupply . renamePattern) patts
    ins' <- traverse (g Input) ins
    outs' <- traverse (g Output) outs

    cmds' <- splitUniqueSupply $ renameCmds . ((`evalState`[])  . cmdsBindFreeVars) $ cmds

    envLcl .= symtab

    return ((patts', ins', outs'), cmds' )
  where
    g pol ident = do
        ident' <- tagIdentP ident
        let ch = _ChIdentR # (ident', pol)
        envLcl %= ((collectSymTab ch)<>)
        return ch 


-- Renaming an expression...
-- TODO: change this to use the reader monad for the symbol
-- table... no need for state here!
renameExpr ::   
    Rename 
        (MplExpr MplParsed) (MplExpr MplRenamed)
renameExpr = cata f
  where
    f :: Base (MplExpr MplParsed) (_ (MplExpr MplRenamed)) ->
        (_ (MplExpr MplRenamed))
    f (EPOpsF cxt op a b) = do
        a' <- a
        b' <- b
        return $ _EPOps # (cxt, op, a', b')
        
    f (EVarF cxt ident) = do
        symtab <- guse envLcl
        let ident' = fromJust $ lookupSymAny ident $  symtab
        -- tell $ outOfScopeWith lookupSymAny (trace ("SYMYAB" ++ show symtab) symtab) $ trace ("vAR"++ show ident ) ident
        tell $ outOfScopeWith lookupSymAny symtab $ ident
        return $ _EVar # (cxt, _IdentR # (ident, ident' ^. uniqueTag))

    f (EIntF cxt n) = return $ _EInt # (cxt, n)
    f (EDoubleF cxt n) = return $ _EDouble # (cxt, n)
    f (ECharF cxt n) = return $ _EChar # (cxt, n)
    f (ECaseF cxt caseon cases) = do
        caseon' <- caseon
        cases' <- traverse g cases
        return $ _ECase # (cxt, caseon', cases')
      where
        g :: (MplPattern MplParsed, _ (MplExpr MplRenamed))
            -> _ (MplPattern MplRenamed, MplExpr MplRenamed)
        g (patt, mexpr) = do
            symtab <- guse envLcl
            patt' <- renamePattern patt
            expr' <- mexpr
            envLcl .= symtab
            return (patt', expr')
    f (EObjCallF cxt ident args) = do
        symtab <- guse envLcl
        let ident' = fromJust $ lookupSymSeqPhrase ident  symtab
        tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 

        args' <- sequenceA args

        return $ _EObjCall # 
            (cxt, _IdentR # (ident, ident' ^. uniqueTag), args')
    -- same thing for call...
    f (ECallF cxt ident args) = do
        symtab <- guse envLcl 
        let ident' = fromJust $ lookupSymAny ident symtab
        tell $ outOfScopeWith lookupSymAny symtab $ ident 

        args' <- sequenceA args

        return $ _ECall # 
            (cxt, _IdentR # (ident, ident' ^. uniqueTag), args')

    f (ERecordF cxt args) = do
        args' <- traverse g args
        return $ _ERecord # (cxt, args')
      where
        g (cxt, ident, (patts, expr)) = do
            symtab <- guse envLcl
            let ident' = fromJust $ lookupSymSeqPhrase ident symtab
            tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 

            patts' <- traverse (splitUniqueSupply . renamePattern) patts
            expr' <- expr

            envLcl .= symtab

            return 
                ( cxt
                , _IdentR # (ident, ident' ^. uniqueTag)
                , (patts', expr')
                )
    f (ELetF cxt stmts expr) = do
        stmts' <- traverse f stmts
        envLcl %= (collectSymTab stmts' <>)
        expr' <- expr 
        return $ ELet cxt stmts' expr'
      where
        f :: MplStmt MplParsed -> _ (MplStmt MplRenamed)
        f stmt = do
            st <- guse equality
            lcl <- guse envLcl
            sup <- freshUniqueSupply
            evalStateT (renameStmt stmt) (st & uniqueSupply .~ sup & envGbl .~ lcl)
            

-- Renaming commands...
renameCmds ::
    Rename (NonEmpty (MplCmd MplCmdFreeVars)) (NonEmpty (MplCmd MplRenamed))
renameCmds (cmd :| []) = do
    {-
    case cmd of
        (CClose cxt _) -> tell [_IllegalLastCommand # cxt]
        (CGet cxt _ _) -> tell [_IllegalLastCommand # cxt]
        (CPut cxt _ _) -> tell [_IllegalLastCommand # cxt]
        (CHPut cxt _ _) -> tell [_IllegalLastCommand # cxt]
        (CSplit cxt _ _) -> tell [_IllegalLastCommand # cxt]
        _ -> tell []
    -}
    (:|[]) <$> renameCmd cmd
renameCmds (cmd :| rst) = do
    {-
    case cmd of
        CFork cxt _ _ -> tell [_IllegalNonLastCommand # cxt]
        CId cxt _ -> tell [_IllegalNonLastCommand # cxt]
        CIdNeg cxt _ -> tell [_IllegalNonLastCommand # cxt]
        CRun _ cxt _ _ _ -> tell [review _IllegalNonLastCommand $ cxt ^. identPNameOcc % to KeyWordNameOcc]
            -- techincally not a keyword..
        CHCase cxt _ _ -> tell [_IllegalNonLastCommand # cxt]
        CHalt cxt _ -> tell [_IllegalNonLastCommand # cxt]
        CRace cxt _ -> tell [_IllegalNonLastCommand # cxt]
        _ -> tell []
    -}
    cmd' <- renameCmd cmd
    rst' <- NE.toList <$> renameCmds (NE.fromList rst)
    return (cmd' :| rst')

renameCmd ::
    Rename (MplCmd MplCmdFreeVars) (MplCmd MplRenamed)
renameCmd = f 
  where
    f :: MplCmd MplCmdFreeVars -> _ (MplCmd MplRenamed)
    f (CRun cxt ident seqs ins outs) = do
        symtab <- guse envLcl
        let identlkup = lookupSym ident (_Just % _SymProcInfo) symtab
            ident' = _IdentR # (ident, identlkup ^. to fromJust % uniqueTag )
            inslkup = traverse (flip lookupCh symtab) ins
            outslkup = traverse (flip lookupCh symtab) outs
            inslkup' = fromJust inslkup
            outslkup' = fromJust outslkup

            ins' = zipWith 
                (\ch -> review _IdentR 
                    . (ch,) 
                    . view uniqueTag) ins inslkup'
            outs' = zipWith 
                (\ch -> review _IdentR 
                    . (ch,) 
                    . view uniqueTag) outs outslkup'

        tell $ concat 
            [ maybe [_OutOfScope # ident] (const []) identlkup
            , outOfScopesWith lookupCh symtab ins 
            , outOfScopesWith lookupCh symtab outs ]
        -- tell $ maybe (concatMap expectedInputPolarity $ zip ins inslkup') (const []) $ inslkup
        -- tell $ maybe (concatMap expectedOutputPolarity $ zip ins outslkup') (const []) $ outslkup

        seqs' <- traverse (\expr -> do
                expr' <- splitUniqueSupply >=> renameExpr $ pure expr
                -- let bindings change the symbol table, so we make sure
                -- to reset this..
                envLcl .= symtab
                return expr' ) seqs
        return $ _CRun # 
            ( cxt
            , ident'
            , seqs'
            , map (review _ChIdentR . (,Input)  ) ins'
            , map (review _ChIdentR . (,Output) ) outs')

    f (CClose cxt ch) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch 
        let chlkup = lookupCh ch symtab
            chlkup' = fromJust chlkup
            ch' = _ChIdentR # (_IdentR # (ch, chlkup' ^. uniqueTag ), chlkup' ^. symEntryInfo)
        return $ CClose cxt ch'
    f (CHalt cxt ch) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch 
        let chlkup = lookupCh ch symtab
            chlkup' = fromJust chlkup
            ch' = _ChIdentR # (_IdentR # (ch, chlkup' ^. uniqueTag ), chlkup' ^. symEntryInfo)
        return $ CHalt cxt ch'
    f (CGet cxt patt ch) = do
        symtab <- guse envLcl
        patt' <- renamePattern patt
        tell $ outOfScopeWith lookupCh symtab ch 
        let chlkup = lookupCh ch symtab
            chlkup' = fromJust chlkup
            ch' = _ChIdentR # (_IdentR # (ch, chlkup' ^. uniqueTag ), chlkup' ^. symEntryInfo)
        return $ CGet cxt patt' ch'
    f (CPut cxt expr ch) = do
        symtab <- guse envLcl
        expr' <- renameExpr expr
        tell $ outOfScopeWith lookupCh symtab ch 
        let chlkup = lookupCh ch symtab
            chlkup' = fromJust chlkup
            ch' = _ChIdentR # (_IdentR # (ch, chlkup' ^. uniqueTag ), chlkup' ^. symEntryInfo)
        return $ CPut cxt expr' ch'

    f (CHCase cxt ch cases) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch
        let chlkup = lookupCh ch symtab
            ch' = do
                chlkup' <- chlkup
                return $ _ChIdentR # (_IdentR # (ch, chlkup' ^. uniqueTag ), chlkup' ^. symEntryInfo)
            ch'' = fromJust ch' 

        cases' <- traverse (g ch') cases
        return $ CHCase cxt ch'' cases'
      where
        g ch' (cxt, ident, cmds) = do
            symtab <- guse envLcl
            tell $ outOfScopeWith lookupConcPhrase symtab ident
            let lkup = lookupConcPhrase ident symtab
                lkup' = fromJust lkup 
                ident' = tagIdentPWithSymEntry ident lkup'

                ch'' = fromJust ch'
            {-
            tell $ bool [] 
                (hCaseError 
                    (ch'' ^. chIdentRIdentR % identRIdentP, ch'' ^. polarity)
                    (ident' ^. identRIdentP, lkup' ^. symEntryInfo)
                    ) 
                (isJust ch' && isJust lkup)
                -}

            cmds' <- splitUniqueSupply $ renameCmds cmds
            envLcl .= symtab
            return (cxt, ident', cmds')
    f (CHPut cxt ident ch) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch
        tell $ outOfScopeWith lookupConcPhrase symtab ident
        let chlkup = lookupCh ch symtab
            chlkup' = fromJust chlkup
            ch' = _ChIdentR # (_IdentR # (ch, chlkup' ^. uniqueTag ), chlkup' ^. symEntryInfo)

            lkup = lookupConcPhrase ident symtab
            lkup' = fromJust lkup 
            ident' = tagIdentPWithSymEntry ident lkup'

        {-
        tell $ bool [] 
            (hPutError 
                (ch' ^. chIdentRIdentR % identRIdentP, ch' ^. polarity)
                (ident' ^. identRIdentP, lkup' ^. symEntryInfo)
                ) 
            (isJust chlkup && isJust lkup)
        -}

        return $ CHPut cxt ident' ch'
        
    f (CSplit cxt ch (ch1,ch2)) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch
        let chlkup = lookupCh ch symtab
            chlkup' = fromJust chlkup
            ch' = _ChIdentR # (_IdentR # (ch, chlkup' ^. uniqueTag ), chlkup' ^. symEntryInfo)
        ch1' <- fmap (review _ChIdentR . (,ch' ^. polarity)) $ splitUniqueSupply $ tagIdentP ch1
        ch2' <- fmap (review _ChIdentR . (,ch' ^. polarity)) $ splitUniqueSupply $ tagIdentP ch2
        envLcl %= deleteCh ch
        envLcl %= ((collectSymTab [ch1',ch2'])<>)
        return $ CSplit cxt ch' (ch1',ch2')

    f (CFork cxt ch ((ch1, (p1, cxt1), cmds1), (ch2, (p2, cxt2), cmds2))) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch
        let chlkup = lookupCh ch symtab
            chlkup' = fromJust chlkup
            ch' = fromJust $ tagIdentPToChIdentRWithSymEntry ch <$> chlkup

            cxt1' = zipWith tagIdentPToChIdentRWithSymEntry cxt1 
                    $ fromJust
                    $ traverse (flip lookupCh symtab) cxt1
            cxt2' = zipWith tagIdentPToChIdentRWithSymEntry cxt2 
                    $ fromJust
                    $ traverse (flip lookupCh symtab) cxt2
            -- TODO: Currently, if there is a user provided context and a variable out of 
            -- scope, this will simply just ignore it... change this so that it really checks
            -- it, by providing the information of whether it was user supplied so we know whether
            -- to do out of scope checks.
        if p1 == UserProvidedContext
            then do 
                tell $ outOfScopesWith lookupCh symtab cxt1 
                tell $ overlappingDeclarations cxt1 
            else return ()
        if p2 == UserProvidedContext
            then do
                tell $ outOfScopesWith lookupCh symtab cxt2 
                tell $ overlappingDeclarations cxt1 
            else return ()

        envLcl %= deleteCh ch
        symtab <- guse envLcl

        -- tell $ forkExpectedDisjointChannelsButHasSharedChannels cxt1 cxt2

        ch1' <- fmap (review _ChIdentR . (,ch' ^. polarity)) $ splitUniqueSupply $ tagIdentP ch1
        ch2' <- fmap (review _ChIdentR . (,ch' ^. polarity)) $ splitUniqueSupply $ tagIdentP ch2

        envLcl .= symtab
        envLcl %= ((collectSymTab ch1')<>) . restrictChs cxt1
        
        cmds1' <- renameCmds cmds1

        envLcl .= symtab
        envLcl %= ((collectSymTab ch2')<>) . restrictChs cxt2
        cmds2' <- renameCmds cmds2

        envLcl .= symtab

        return $ CFork cxt ch' ((ch1', cxt1', cmds1'), (ch2', cxt2', cmds2'))

    f (CId cxt (ch1, ch2)) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch1
        tell $ outOfScopeWith lookupCh symtab ch2
        let ch1lkup = lookupCh ch1 symtab
            ch1' = fromJust $ tagIdentPToChIdentRWithSymEntry ch1 <$> ch1lkup
            ch2lkup = lookupCh ch2 symtab
            ch2' = fromJust $ tagIdentPToChIdentRWithSymEntry ch2 <$> ch2lkup
        return $ CId cxt (ch1', ch2')

    f (CIdNeg cxt (ch1, ch2)) = do
        symtab <- guse envLcl
        tell $ outOfScopeWith lookupCh symtab ch1
        tell $ outOfScopeWith lookupCh symtab ch2
        let ch1lkup = lookupCh ch1 symtab
            ch1' = fromJust $ tagIdentPToChIdentRWithSymEntry ch1 <$> ch1lkup
            ch2lkup = lookupCh ch2 symtab
            ch2' = fromJust $ tagIdentPToChIdentRWithSymEntry ch2 <$> ch2lkup
        return $ CIdNeg cxt (ch1', ch2')

    f (CRace cxt races) = do
        races' <- traverse g races
        return $ CRace cxt races'
      where
        g (ch, cmds) = do
            symtab <- guse envLcl
            let chlkup = lookupCh ch symtab
                ch' = fromJust $ tagIdentPToChIdentRWithSymEntry ch <$> chlkup
            cmds' <- renameCmds cmds

            envLcl .= symtab

            return (ch', cmds')

    f (CPlugs (keyword, (p, cxt)) (phr1, phr2, phrs)) = do
        ~symtab <- guse envLcl

        sup <- freshUniqueSupply

        let ~scopes = map fst $ channelsInScope symtab
            ~plugged = if p == ComputedContext 
                then cxt \\ scopes
                else cxt
            ~plugged' = (`evalState` sup) $ traverse tagIdentP plugged

        tell $ bool [] (overlappingDeclarations plugged) (p == UserProvidedContext)

        ~(phr1':phr2':phrs') <- traverse (g plugged') (phr1:phr2:phrs)

        return $ CPlugs (keyword, plugged') (phr1', phr2', phrs')
      where
        g :: _ -> ((), ([IdentP], [IdentP]), NonEmpty (MplCmd MplCmdFreeVars)) ->
            _ ((), ([ChIdentR], [ChIdentR]), NonEmpty (MplCmd MplRenamed))
        g plugged ((), (ins, outs), cmds) = do
            initsymtab <- guse envLcl

            -- check overlapping declarations...
            tell $ overlappingDeclarations $ ins ++ outs

            -- restrict and check out of scope for the input channels
            envLcl %= (restrictChs ins (collectSymTab (map (review _ChIdentR . (,Input)) plugged))<>)
            symtab <- guse envLcl
            tell $ outOfScopesWith lookupCh symtab ins 
            let inslkup = fromJust $ traverse (flip lookupCh symtab) ins
                ins' = zipWith tagIdentPToChIdentRWithSymEntry ins inslkup
            -- envLcl %= restrictChs ins

            -- similarly, for the output channels
            envLcl %= (restrictChs outs (collectSymTab (map (review _ChIdentR . (,Output)) plugged))<>)
            symtab <- guse envLcl
            tell $ outOfScopesWith lookupCh symtab outs 
            let outslkup = fromJust $ traverse (flip lookupCh symtab) outs
                outs' = zipWith tagIdentPToChIdentRWithSymEntry outs outslkup
            envLcl %= restrictChs (ins ++ outs)

            ~cmds' <- renameCmds cmds

            envLcl .= initsymtab

            return ((), (ins', outs'), cmds')

    {-
    | CCase 
        !(XCCase x) 
        (XMplExpr x) 
        (NonEmpty (XMplPattern x, NonEmpty (MplCmd x)))
        {-
        { _cCase :: Expr pattern letdef typedef seqcalleddef ident
        , _cCases :: [(pattern, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)] }
        -}
    | CSwitch !(XCSwitch x) (NonEmpty (XMplExpr x, NonEmpty (MplCmd x)))
        -- { _cSwitches :: NonEmpty (Expr pattern letdef typedef seqcalleddef ident, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) }
        -}
