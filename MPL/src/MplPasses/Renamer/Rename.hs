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

import Control.Monad.State
import Control.Monad.Writer

import MplUtil.UniqueSupply

import Data.Maybe
import Data.Functor.Foldable (Base, cata)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable

runRename ::
    Rename (MplProg MplParsed) (MplProg MplRenamed)
runRename (MplProg stmts) = 
    MplProg <$> traverse f stmts
  where
    f stmt = do
        -- get the current symbol table
        symtab <- guse symTab
        -- rename the statement (modifies symbol table
        stmt' <- renameStmt stmt
        -- reset the symbol table
        symTab .= symtab
        -- modify the symbol table to include the context 
        -- of the statement
        symTab %= (collectSymTab stmt'<>)
        -- return the statement
        return stmt'

renameStmt ::   
    Rename (MplStmt MplParsed) (MplStmt MplRenamed)
renameStmt (MplStmt defns wheres) = do
    tell $ overlappingDeclarations 
        $ foldMap (NE.toList .  mplStmtTopLevelIdents) wheres 
            -- ++ foldMap (NE.toList . mplDefnIdents) defns

    wheres' <- traverse renameStmt wheres
    let entries = concatMap collectSymTab wheres'
    supply <- freshUniqueSupply
    defns' <- NE.fromList <$> renameDefns (NE.toList defns)
    return $ MplStmt defns' wheres'

renameDefns ::
    Rename 
        [MplDefn MplParsed]
        [MplDefn MplRenamed]
renameDefns [] = return []
renameDefns (defn : defns) = do
    uniqsup <- freshUniqueSupply

    rec defn' <- (`evalStateT` (_RenameEnv # (uniqsup, symtab)))
                $ renameDefn defn
        symTab %= ((collectSymTab defn') <>)
        defns' <- renameDefns defns
        symtab <- guse symTab

    return (defn' : defns')

renameDefn ::
    Rename (MplDefn MplParsed) (MplDefn MplRenamed)
renameDefn (ObjectDefn obj) = ObjectDefn <$> case obj of
    DataDefn n -> DataDefn <$> renameTypeClauseSpine n
    CodataDefn n -> CodataDefn <$> renameTypeClauseSpine n
    ProtocolDefn n -> ProtocolDefn <$> renameTypeClauseSpine n
    CoprotocolDefn n -> CoprotocolDefn <$> renameTypeClauseSpine n
renameDefn (FunctionDefn (MplFunction name funtype defn)) = do
    name' <- tagIdentP name

    funtype' <- flip (maybe (return Nothing)) funtype $ \(froms, to) -> do
        froms' <- traverse (splitUniqueSupply . renameType) froms
        to' <- splitUniqueSupply $ renameType to 
        return $ Just (froms', to')
    
    defn' <- traverse renamePattsExpr defn

    return $ FunctionDefn $ _MplFunction # (name', funtype', defn')

renameDefn (ProcessDefn (MplProcess name proctype defn)) = do
    name' <- tagIdentP name
    proctype' <- flip (maybe (return Nothing)) proctype $ \(seqs, ins, outs) -> do
        seqs' <- traverse (splitUniqueSupply . renameType) seqs
        ins' <- traverse (splitUniqueSupply . renameType) ins
        outs' <- traverse (splitUniqueSupply . renameType) outs
        return $ Just (seqs', ins', outs')

    defn' <- traverse renameProcBodyPhrase defn

    return $ ProcessDefn $ _MplProcess # (name', proctype', defn')

renamePattsExpr ::   
    Rename 
        ([MplPattern MplParsed], MplExpr MplParsed) 
        ([MplPattern MplRenamed], MplExpr MplRenamed)
renamePattsExpr (patts, expr) = do
        symtab <- guse symTab
        tell $ overlappingDeclarations 
            $ concatMap collectPVarIdPs patts
        patts' <- traverse (splitUniqueSupply . renamePattern) patts
        expr' <- splitUniqueSupply (renameExpr expr)
        symTab .= symtab
        return (patts', expr')

renameProcBodyPhrase :: 
    Rename
        ( ([MplPattern MplParsed], [ChP MplParsed], [ChP MplParsed])
        , NonEmpty (MplCmd MplParsed)) 
        ( ([MplPattern MplRenamed], [ChP MplRenamed], [ChP MplRenamed])
        , NonEmpty (MplCmd MplRenamed)) 
renameProcBodyPhrase ((patts, ins, outs), cmds) = do
    symtab <- guse symTab

    patts' <- traverse (splitUniqueSupply . renamePattern) patts
    ins' <- traverse g ins
    outs' <- traverse g outs

    symTab .= symtab

    cmds' <- splitUniqueSupply $ renameCmds cmds
    return ((patts', ins', outs'), cmds' )
  where
    g ident = do
        ident' <- tagIdentP ident
        undefined 


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
        symtab <- guse symTab
        let ident' = fromJust $ lookupSym ident _Nothing symtab
        tell $ outOfScope symtab ident 
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
            symtab <- guse symTab
            patt' <- renamePattern patt
            expr' <- mexpr
            symTab .= symtab
            return (patt', expr')
    f (EObjCallF cxt ident args) = do
        symtab <- guse symTab
        let ident' = fromJust $ lookupSym ident _Nothing symtab
        tell $ outOfScope symtab ident 

        args' <- sequenceA args

        return $ _EObjCall # 
            (cxt, _IdentR # (ident, ident' ^. uniqueTag), args')
    -- same thing for call...
    f (ECallF cxt ident args) = do
        ~(EObjCall cxt ident args) <- f (EObjCallF cxt ident args)
        return $ _ECall # (cxt, ident, args)
    f (ERecordF cxt args) = do
        args' <- traverse g args
        return $ _ERecord # (cxt, args')
      where
        g (cxt, ident, (patts, expr)) = do
            symtab <- guse symTab
            let ident' = fromJust $ lookupSym ident _Nothing symtab
            tell $ outOfScope symtab ident 

            patts' <- traverse (splitUniqueSupply . renamePattern) patts
            expr' <- expr

            symTab .= symtab

            return 
                ( cxt
                , _IdentR # (ident, ident' ^. uniqueTag)
                , (patts', expr')
                )

-- Renaming commands...
renameCmds ::
    Rename (NonEmpty (MplCmd MplParsed)) (NonEmpty (MplCmd MplRenamed))
renameCmds (cmd :| []) = do
    case cmd of
        (CClose cxt _) -> tell [_IllegalLastCommand # cxt]
        (CGet cxt _ _) -> tell [_IllegalLastCommand # cxt]
        (CPut cxt _ _) -> tell [_IllegalLastCommand # cxt]
        (CHPut cxt _ _) -> tell [_IllegalLastCommand # cxt]
        (CSplit cxt _ _) -> tell [_IllegalLastCommand # cxt]
        _ -> tell []
    (:|[]) <$> renameCmd cmd
renameCmds (cmd :| rst) = do
    case cmd of
        CFork cxt _ _ -> tell [_IllegalNonLastCommand # cxt]
        CId cxt _ -> tell [_IllegalNonLastCommand # cxt]
        CIdNeg cxt _ -> tell [_IllegalNonLastCommand # cxt]
        CRun _ cxt _ _ _ -> tell [_IllegalNonLastCommand # cxt]
        CHCase cxt _ _ -> tell [_IllegalNonLastCommand # cxt]
        CHalt cxt _ -> tell [_IllegalNonLastCommand # cxt]
        CRace cxt _ -> tell [_IllegalNonLastCommand # cxt]
        _ -> tell []
    cmd' <- renameCmd cmd
    rst' <- NE.toList <$> renameCmds (NE.fromList rst)
    return (cmd' :| rst')

renameCmd ::
    Rename (MplCmd MplParsed) (MplCmd MplRenamed)
renameCmd = f
  where
    f (CRun cxt ident seqs ins outs) = do
        undefined
        {-
        symtab <- guse symTab
        let identlkup = lookupSym ident (_Just % _SymProcInfo) symtab
            ident' = _IdentR # (ident, identlkup ^. to fromJust % uniqueTag )
            inslkup' = fromJust $ 
                traverse (flip lookupCh symtab) ins
            outslkup' = fromJust $ 
                traverse (flip lookupCh symtab) outs

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
            , outOfScopes symtab ins 
            , outOfScopes symtab outs ]

        seqs' <- traverse (\expr -> do
                expr' <- splitUniqueSupply >=> renameExpr $ expr
                -- let bindings change the symbol table, so we make sure
                -- to reset this..
                symTab .= symtab
                return expr' ) seqs
        return $ _CRun # (cxt, ident', seqs', ins', outs')
        -}

    {-
    CRun !(XCRun x) (IdP x) [XMplExpr x] [ChP x] [ChP x] 
        -- called, seq args, input chs, outchs 
    | CClose !(XCClose x) (ChP x )
    | CHalt !(XCHalt x) (ChP x)

    | CGet !(XCGet x) (XMplPattern x) (ChP x)
    | CPut !(XCPut x) (XMplExpr x) (ChP x)

    | CHCase 
        !(XCHCase x)
        (ChP x)
        (NonEmpty (XCHCasePhrase x, IdP x, NonEmpty (MplCmd x)))
        {-
        (NonEmpty 
            ( ident
            , conccalleddef
            , ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident))
            -}
    | CHPut  
        !(XCHPut x) (IdP x) (ChP x)
            -- hput (Co)Protocol on channel
        
        -- { _cHPut :: ident, _cHPutDef :: conccalleddef , _cHPutCh :: chident }

    | CSplit !(XCSplit x) (ChP x) (ChP x, ChP x)
    -- { _cSplit :: chident, _cSplitInto :: (chident, chident) }
    | CFork !(XCFork x) 
        (ChP x) 
            ( (ChP x, XCForkPhrase x, NonEmpty (MplCmd x))
            , (ChP x, XCForkPhrase x, NonEmpty (MplCmd x))
            )
        {-
        { 
        _cFork :: chident
        , _cForkInto :: 
        ( (chident, [chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)
        , (chident, [chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) ) }
        -}

    | CId !(XCId x) (ChP x, ChP x) -- { _cIdLarg :: chident, _cIdRarg :: chident}
    | CIdNeg !(XCIdNeg x) (ChP x, ChP x) -- { _cIdLarg :: chident, _cIdNegArg :: chident}
    
    | CRace !(XCRace x) 
        (NonEmpty (ChP x, NonEmpty (MplCmd x)))
        -- { _cRaces :: NonEmpty (chident, ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) }

    | CPlug !(XCPlug x) (CPlugPhrase x, CPlugPhrase x)
    | CPlugs !(XCPlugs x) (CPlugPhrase x, CPlugPhrase x, [CPlugPhrase x])
        {-
        -- | plugged together
        [chident] 
        -- | plug phrases
        ([chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident) 
        ([chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)
        [([chident], ProcessCommands pattern letdef typedef seqcalleddef conccalleddef ident chident)]
        -}

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
