{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.PatternCompiler.PatternCompile where

import Optics
import Optics.State.Operators

import Data.Function

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled
import MplAST.MplProgUtil

import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Control.Applicative.Lift
import Data.Functor.Compose
import Data.Kind
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Either
import Data.Coerce


import MplPasses.PatternCompiler.PatternCompileErrors
import MplPasses.PatternCompiler.PatternCompileUtils
import MplPasses.PatternCompiler.PatternCompileMplExprSub
import MplPasses.PatternCompiler.PatternCompileUtils

import Data.Foldable
import qualified Data.Map as Map
import Data.Map ( Map (..) )

import Control.Exception

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Functor.Foldable (cata, embed)
import Data.Tuple
import Data.Traversable
import Data.Bool

import Debug.Trace
import Data.Proxy

import Data.Set (Set)
import qualified Data.Set as Set

import Unsafe.Coerce

-- TODO: The let floating should really all be in the writer monad instead
-- of doing the manual plumbing of passing the state up..

runPatternCompile' ::
    AsPatternCompileErrors err => 
    (TopLevel, UniqueSupply) -> 
    MplProg MplTypeChecked ->
    Either [err] (MplProg MplPatternCompiled)
runPatternCompile' (top, sup) = flip evalState (_Env # (top, sup, mempty, mempty)) . runPatternCompile

runPatternCompile ::
    forall err m .
    ( MonadState PatternCompileEnv m 
    , AsPatternCompileErrors err )  => 
    MplProg MplTypeChecked ->
    m (Either [err] (MplProg MplPatternCompiled))
runPatternCompile (MplProg prog) = go 
  where
    go :: m (Either [err] (MplProg MplPatternCompiled))
    go = do 
        ~(v, lg) <- runWriterT $ traverse patternCompileStmt prog
        if null lg
            then return $ Right $ MplProg v
            else return $ Left $ lg
{-
runPatternCompile (MplProg prog) = fmap runErrors $ getCompose go
  where
    go :: Compose m (Errors [err]) (MplProg MplPatternCompiled)
    go = fmap MplProg $ traverse 
        ( Compose
        . fmap eitherToErrors 
        . runExceptT 
        . patternCompileStmt )  
        prog
-}

patternCompileStmt ::
    PatternCompile (MplStmt MplTypeChecked) (MplStmt MplPatternCompiled) 
patternCompileStmt stmt = do
    wheres' <- traverse (localEnvSt id . patternCompileStmt) wheres
    defns' <- fmap (NE.fromList . concat) $ traverse patternCompileDefn defns
    return $ MplStmt defns' wheres'
  where
    wheres = stmt ^. stmtWhereBindings
    defns = stmt ^. stmtDefns

{- | 'patternCompileDefn'. Pattern compiles definitions.. why does this return a list of definitions?  
this is because when pattern compiling a 'ProcessDefn', we can generate more process definitions when
pattern compiling 'CCase' efficiently.
-}
patternCompileDefn :: 
    PatternCompile (MplDefn MplTypeChecked) [MplDefn MplPatternCompiled]
patternCompileDefn (ObjectDefn obj) = fmap (pure . ObjectDefn) $ patternCompileObj obj
patternCompileDefn (FunctionDefn fun) = patternCompileFunDefn fun
patternCompileDefn (ProcessDefn proc) = patternCompileProcessDefn proc

-- | patternCompileObj.  This is essentially the identity function to juggle some types around.
-- TODO: I guess I should probabaly write out the translation instead of just using 'unsafeCoerce'
patternCompileObj :: 
    PatternCompile
        (MplObjectDefn MplTypeChecked)
        (MplObjectDefn MplPatternCompiled)
patternCompileObj inp = return $ unsafeCoerce inp


-- | patternCompileExpr. pattern compiles an expression -- note this is mostly just recursing through
-- the data structure, but it needs to fix @case@s given by the user; and this recurses through the 
-- expression
patternCompileExpr ::
    PatternCompile 
        (MplExpr MplTypeChecked) 
        ([MplDefn MplPatternCompiled], MplExpr MplPatternCompiled)
patternCompileExpr = cata go
  where
    go :: 
        MplExprF (MplPass 'TypeChecked) (_ ([MplDefn MplPatternCompiled], MplExpr MplPatternCompiled)) -> 
        _ ([MplDefn MplPatternCompiled], MplExpr MplPatternCompiled)
    go = \case
        EPOpsF ann primop l r -> do
            (ndefns0, l') <- l 
            (ndefns1, r') <- r
            return $ (ndefns0 ++ ndefns1, EPOps ann primop l' r')
        EVarF ann ident -> fmap (mempty,) $ pure $ EVar ann ident
        EIntF ann n -> fmap (mempty,) $ pure $ EInt (snd ann) n
        ECharF ann v -> fmap (mempty,) $ pure $ EChar (snd ann) v
        EDoubleF ann v -> fmap (mempty,) $ pure $ EDouble (snd ann) v
        EBoolF ann v -> fmap (mempty,) $ pure $ EBool (snd ann) v
        -- ECaseF ann expr cases -> ECase ann <$> expr <*> traverse sequenceA cases
        ECaseF ann expr cases -> do
            (ndefnsexpr, expr') <- expr
            (ndefnscases, cases') <- fmap (first fold . NE.unzip) $ for cases $ \(patt, expr) -> do 
                (ndefns, expr') <- localEnvSt (over envLcl((collectPattVars patt,[],[])<>)) $ expr
                return (ndefns, (patt,expr'))

            let caseslst' = fmap (over _1 pure) cases'
                exhstchk = patternCompileExhaustiveCheck $ fmap fst caseslst'
            tell $ bool [_NonExhaustiveECasePatt # ()] [] exhstchk

            ~([PVar _ u], nbdy) <- patternCompileSeqPatPhrases $ caseslst'
            {- We essentially ``let float'' this so that later when we compile this
             - we only compute this expression once instead of duplciating 
             - computation....
             - The old way of doing:
             - @
             - return $ substituteVarIdentByExpr (u, expr') nbdy
             - @
             - duplicated the computation! Very bad! Could turn linear algorithms 
             - into exponential time.
            -}
            {- This idea was slightly better, but requires doing some more work later
             - down the pipeline to get it to properly memoize the results.
            @
            let utp = getExprType expr'
                letfloated = MplStmt (letdefn :| []) []
                letdefn = FunctionDefn $ MplFunction u ([], [], utp) (([], expr') :| [])

            return 
                $ ELet () (letfloated :| []) 
                $ substituteVarIdentByExpr (u, ECall utp u []) nbdy
            @
            -}
            {- But, we're gonna go with a ``continuation passing style'' sorta thing --
             - we pass everything to a function which continues what to do next.. This will
             - for sure make sure we evaluate this.
             -
             - NB. there is a performance bug with the 'CCase' -- since there aren't let
             - bindings in do blocks, doing this same transformation is impossible. 
             - We would have to essentially end it with a ``Run'' to continue everything
             - where we left off. 
             -
             - NB. I thinkn the above is fixed
             -}
             {- Okay final revision -- we fully float all lambda definitions to
              - the top to avoid creating dupliate definitions which may occur
              - sometimes when the pattern compilation algorithm duplicates the
              - expression body which would later cause an error in the
              - assembling of the process, since we do not alpha rename later
              -}
            casef <- freshCaseFunIdP

            seqscxt <- guse (envLcl % _1)

            let letfloated = MplStmt (letdefn :| []) []
                letdefn = FunctionDefn $ MplFunction 
                    casef 
                    ([], map (\(PVar ann _) -> ann) seqscxt ++ [getExprType expr'], getExprType nbdy) 
                    (( seqscxt ++ [PVar (getExprType expr') u], nbdy) :| [])

            return 
                ( ndefnsexpr ++ ndefnscases ++ [letdefn]
                -- ELet () (letfloated :| []) $ ECall (getExprType nbdy) casef [expr']
                , ECall (getExprType nbdy) casef $ map (\(PVar ann v) -> EVar ann v) seqscxt ++ [expr']
                )
                -- substituteVarIdentByExpr (u, ECall (getExprType nbdy) u []) nbdy

        EObjCallF ann ident exprs -> do
            (ndefns, exprs') <- fmap (first concat . unzip) $ sequenceA exprs
            return (ndefns, EObjCall ann ident exprs')

        -- TODO: we need to check if creating records is exhaustive..
        ERecordF ann phrases -> do
            (ndefns, phrases') <- fmap (first fold . NE.unzip) $ for phrases $ \(phrase, identt, (patts, mexpr)) -> do
                let exhstchk = patternCompileExhaustiveCheck (patts :| [])
                tell $ bool [ _NonExhaustiveRecordPatt # identt] [] exhstchk
                (ndefns, expr) <- mexpr
                (us, expr') <- patternCompileSeqPatPhrases ((patts, expr) :| [])

                return (ndefns, (phrase, identt, (us, expr')) )

            return (ndefns, _ERecord # (snd ann, phrases'))

        ECallF ann ident exprs -> do
            (ndefns, exprs') <- fmap (first concat . unzip) $ sequenceA exprs 
            return (ndefns, ECall ann ident exprs')

        EListF ann exprs -> do
            (ndefns, exprs') <- fmap (first concat . unzip) $ sequenceA exprs 
            return (ndefns, EList (snd ann) exprs')

        EStringF ann str -> 
            pure $ (mempty, EString (snd ann) str)

        EUnitF ann -> pure $ (mempty, EUnit $ snd ann)

        ETupleF ann (mt0,mt1,mts) -> do
            (ndefns0, t0) <- mt0 
            (ndefns1, t1) <- mt1 
            (ndefnss, ts) <- fmap (first concat . unzip) $ sequenceA mts

            return $ (ndefns0 ++ ndefns1 ++ ndefnss, ETuple (snd ann) (t0,t1,ts))

        EBuiltInOpF ann op l r -> do
            (ndefns0, l') <- l
            (ndefns1, r') <- r
            return ( ndefns0 ++ ndefns1, EBuiltInOp (snd ann) op l' r')

        EIfF ann iff thenf elsef -> do
            (ndefns0, iff') <- iff
            (ndefns1, thenf') <- thenf
            (ndefns2, elsef') <- elsef
            return (ndefns0 ++ ndefns1 ++ ndefns2, EIf ann iff' thenf' elsef')

        ESwitchF ann switches -> do
            (ndefns, switches') <- fmap (first fold . NE.unzip) $ for switches $ \(l,r) -> do
                (ndefns0, l') <- l
                (ndefns1, r') <- r
                return (ndefns0 ++ ndefns1, (l',r'))

            switches'' <- patternCompileExhaustiveESwitchCheck switches'

            return (ndefns, foldr f (EIllegalInstr $ getExprType $ snd $ NE.head switches') switches'')
            {-
            sequenceOf (traversed % each) switches 
                >>= \switches' -> patternCompileExhaustiveESwitchCheck switches'
                    >>= return . foldr f (EIllegalInstr $ getExprType $ snd $ NE.head switches')
            -}
          where
            f :: 
                (MplExpr MplPatternCompiled, MplExpr MplPatternCompiled) -> 
                MplExpr MplPatternCompiled -> 
                MplExpr MplPatternCompiled
            f (bexpr, thenc) acceq = EIf (getExprType thenc) bexpr thenc acceq
            
        ELetF ann lets expr -> do
            lets' <- traverse patternCompileStmt lets 
            (ndefns, expr') <- expr
            return (ndefns, ELet ann lets' expr')

        {-
        {- We gave up on folds and unfolds
        EFoldF ann foldon phrases -> EFold ann foldon phrases
        EUnfoldF ann expr phrases -> EUnfold ann expr phrases
        XExprF ann -> XExpr ann
        -}
        -}

-- | Pattern compiles a function definition.
patternCompileFunDefn :: 
    PatternCompile (XFunctionDefn MplTypeChecked) [MplDefn MplPatternCompiled]
patternCompileFunDefn (MplFunction funName funTp funDefn) = do
    -- funDefn' <- traverseOf (traversed % _2) patternCompileExpr funDefn
    -- funDefn' <- traverseOf (traversed % _2) patternCompileExpr funDefn

    (ndefns, funDefn') <- fmap (first fold . NE.unzip) $ for funDefn $ \(patts, expr) -> do
        -- (ndefns, expr') <- patternCompileExpr expr
        (ndefns, expr') <- localEnvSt (over envLcl ((foldMap collectPattVars patts, [], [])<>)) 
                $ patternCompileExpr expr
        return (ndefns, (patts, expr'))
        

    let exhstchk = patternCompileExhaustiveCheck $ fmap fst funDefn'
    tell $ bool [_NonExhaustiveFunPatt # funName] [] exhstchk

    (patts, pattexpr) <- patternCompileSeqPatPhrases funDefn'
    return $ ndefns ++ [FunctionDefn $ MplFunction funName funTp $ (patts, pattexpr)  :| []]

-- | Pattern compiles a process definition.
patternCompileProcessDefn ::
    -- PatternCompile (XProcessDefn MplTypeChecked) [XProcessDefn MplPatternCompiled]
    PatternCompile (XProcessDefn MplTypeChecked) [MplDefn MplPatternCompiled]
patternCompileProcessDefn (MplProcess procName procTp procDefn) = do
    (ndefns, cmds@(cmd0 :| rstcmds)) <- fmap (first (reverse . concat) . NE.unzip) $ for procDefn $ \((seqs, ins, outs), cmds) -> do
         -- cmds' <- localEnvSt (set envLcl (coerce (foldMap collectPattVars seqs, ins, outs))) $ patternCompileCmds cmds
         (ndefns, cmds') <- localEnvSt (set envLcl ((foldMap collectPattVars seqs, ins, outs))) $ patternCompileCmds cmds
         return (ndefns, ((seqs, ins, outs), cmds'))

    -- we need to rescope all the channels to use the same variable names...
    let cmds0ins = view (_1 % _2)  cmd0
        cmds0outs = view (_1 % _3) cmd0
        rescope ((patts, ins, outs), cmdblk) =  
            let subTo chs chs' blk = foldr (\sub -> fmap (substituteCh sub)) blk (zip chs chs')
            in subTo outs cmds0outs $ subTo ins cmds0ins cmdblk
        rstcmds' = fmap rescope rstcmds

        cmds' = view _2 cmd0 :| rstcmds'

        patts = fmap (view (_1 % _1)) procDefn


    tell $ bool [ _NonExhaustiveProcPatt # procName] [] $ patternCompileExhaustiveCheck patts

    (npatts, ncmds) <- patternCompileConcPatPhrases $ NE.zip patts cmds'

    return $ ndefns ++ [ProcessDefn $ MplProcess procName procTp $ ((npatts, cmds0ins, cmds0outs), ncmds) :| []]

{- | 'patternCompileCmds'
Mostly should be straightforard.. some strangeness

    - We need to dynamically recompute all variables in scope (sequential varaibles and channels)
        so we can create a continuation to run if we have a 'CCase'

    - Why do we need to do this? Because we want to be able to memoize the case scrutinee of a CCase
        and unfortunately, in a by value machine, we don't have any way to do explicit sharing of 
        terms aside from passing it as an argument to a process... Moreover, this sorta plays
        nicely with the rest of the pipeline...
-}
patternCompileCmds :: 
    PatternCompile 
        (NonEmpty (MplCmd MplTypeChecked)) 
        ([MplDefn MplPatternCompiled], NonEmpty (MplCmd MplPatternCompiled))
patternCompileCmds = fmap (second NE.fromList) . go . NE.toList
  where
    go :: PatternCompile 
        [MplCmd MplTypeChecked] 
        ([MplDefn MplPatternCompiled], [MplCmd MplPatternCompiled])
    go [] = pure mempty
    go (cmd:cmds) = case cmd of
        CRun ann idp seqs ins outs -> assert (null cmds) $ do
            (ndefns, seqs') <- fmap (first fold . unzip) $ traverse patternCompileExpr seqs
            -- second (CRun ann idp seqs' ins outs:) <$> go cmds
            return (ndefns, [CRun () idp seqs' ins outs])
            -- @CRun@ should be the lsat command, so no need to do @go cmds@
        CClose ann chp -> deleteChFromContext chp >> second (CClose ann chp:) <$> go cmds
        CHalt ann chp -> assert (null cmds) $ deleteChFromContext chp >> return (mempty, [CHalt ann chp]) 
        -- cmds should be the lsat command
        -- CHalt ann chp ->  deleteChFromContext chp >> second (CHalt ann chp:) <$> go cmds

        {- Old version that had the following strategy: 
         
                - pattern compile the get
                
                - replace all uses of the pattern by casing on the normal variable from
                    the pattern compiled version.
            So, this would duplciate a lot of the pattern mathcing work.. although, this was quite
            inexpesnive (for sure since it is just patter matching).
            Actually, just kidding we do this still anyways... 

            N.B. A better way to do this:
                
                - restructure 
                @
                    get <Patt> on ch
                @
                to
                @
                    get u on ch
                    case u of
                        <Patt> -> ...
                @
                
                - Then, reuse the rest of the pattern compilation pipeline 
        -}
        {-
        CGet ann patt chp -> do
            let exhstchk = patternCompileExhaustiveCheck ([patt] :| [])
            tell $ bool [ _NonExhaustiveGet # ann ] [] exhstchk 
            u <- freshUIdP
            let utp = getPattType patt
                uexpr = _EVar # (utp, u) :: MplExpr MplPatternCompiled
                upatt = _PVar # (utp, u) :: MplPattern MplPatternCompiled

            cmds'' <- go cmds >>= \cmds' -> for cmds' $ \cmd -> do
                let k expr = do
                        ~([PVar _ u'], expr') <- patternCompileSeqPatPhrases (([patt], expr) :| [])
                        return $ substituteVarIdentByExpr (u', uexpr) expr'
                traverseMplExpr k cmd

            return $ CGet ann upatt chp : cmds''
        -}
        {- special cases when we just have a variable pattern for get..  WHy is this here? Well if it's
            just a variable, there's no need to memoize the result at all.. moreover, we would have to
            trivailly re-pattern compile evrything to ensure that the variable is substituted out; hence, this
            is faster for both the compiler and the final code generated.
        -}
        CGet ann (PVar utp u) chp -> assert (not $ null cmds) $ do
            deleteChFromContext chp
            insertChInContext chp

            let upatt = _PVar # (utp, u) :: MplPattern MplPatternCompiled

            insertPattInContext upatt

            second (CGet ann upatt chp:) <$> go cmds
        -- this special case is identical to the PVar case (no need to add it in the context however..)..
        CGet ann (PNull utp) chp -> assert (not $ null cmds) $ do
            deleteChFromContext chp
            insertChInContext chp

            u <- freshUIdP 
            let upatt = _PVar # (snd utp, u) :: MplPattern MplPatternCompiled

            second (CGet ann upatt chp:) <$> go cmds

        -- get should NOT be the last command for sure..
        CGet ann patt chp -> assert (not $ null cmds) $ do
            deleteChFromContext chp
            insertChInContext chp


            let exhstchk = patternCompileExhaustiveCheck ([patt] :| [])
            tell $ bool [ _NonExhaustiveGet # ann ] [] exhstchk 
            u <- freshUIdP
            let utp = getPattType patt
                uexpr = _EVar # (utp, u) :: MplExpr MplPatternCompiled
                upatt = _PVar # (utp, u) :: MplPattern MplPatternCompiled

            insertPattInContext upatt

            -- technically using the annotation from CGet is wrong, but
            -- we already check that this is exhaustive from just immediately above.
            -- Actually, techincayll, this will produce two errors.. 
            -- N.B. Investigate this bug and fix it a little more... 
            second (CGet ann upatt chp:) <$> go [CCase ann (_EVar # (utp, u)) ((patt, NE.fromList cmds) :| []) ]


        CPut ann expr chp -> do
            deleteChFromContext chp
            insertChInContext chp
            (ndefns, expr') <- patternCompileExpr expr
            (mappend ndefns *** (CPut ann expr' chp:)) <$> go cmds

        CHCase ann chp phrases -> assert (null cmds) $ do
            -- phrases' <-  traverseOf (traversed % _3) patternCompileCmds phrases
            (ndefns, phrases') <- fmap (first concat . NE.unzip) $ for phrases $ \(ann, cts, cmds) -> do
                (ndefns, cmds') <- localEnvSt id $ patternCompileCmds cmds
                return (ndefns, (ann,cts,cmds'))
                
                -- traverseOf (traversed % _3) patternCompileCmds phrases
            -- TODO: should do some exhaustiveness checking? 

            return (ndefns, [CHCase ann chp phrases'])
            -- second (CHCase ann chp phrases':) <$> go cmds
            -- 'CHCase' should be the last command, so no need to do @go cmds@.

        CHPut ann idp chp -> deleteChFromContext chp
            >> insertChInContext chp
            >> second (CHPut ann idp chp:) <$> go cmds

        CSplit ann chp chs -> deleteChFromContext chp
            >> traverseOf each insertChInContext chs
            >> second (CSplit ann chp chs:) <$> go cmds
            
        CFork ann chp phrases -> assert (null cmds) $ do
            ((ndefns0, phrase0), (ndefns1, phrase1)) <- forOf each phrases $ \(forkon, withs, cmds) -> do
                let modenv = over envLcl 
                        $ (case forkon ^. polarity of Input -> over _2 (forkon:) ; Output -> over _3 (forkon:))
                        . over _3 (filter (`elem`withs))
                        . over _2 (filter (`elem`withs))
                (ndefns, cmds') <- localEnvSt modenv $ patternCompileCmds cmds
                return (ndefns, (forkon, withs, cmds'))

            return (ndefns0 ++ ndefns1, [CFork ann chp (phrase0, phrase1)])
            -- (CFork ann chp phrases':) <$> go cmds
            {-
            phrases' <- traverseOf (each % _3) patternCompileCmds phrases
            (CFork ann chp phrases':) <$> go cmds
            -}
            
        CId ann chs -> assert (null cmds) $ return (mempty, [CId ann chs])
        -- CId ann chs ->  return (CId ann chs:) <$> go cmds

        CIdNeg ann chs -> assert (null cmds) $ return (mempty, [CIdNeg ann chs]) 


        CRace ann races -> assert (null cmds) $ do
            forOf (traversed % _1) races $ \ch -> deleteChFromContext ch >> insertChInContext ch
            (ndefns, races') <- fmap (first concat . NE.unzip) $ for races $ \(ch, cmds) -> do
                (ndefns, cmds') <- localEnvSt id $ patternCompileCmds cmds
                return (ndefns, (ch, cmds'))
            return (ndefns, [CRace ann races'])
            {-
            races' <- traverseOf (traversed % _2 ) patternCompileCmds races
            -- (CRace ann races':) <$> go cmds
            return $ [CRace ann races']
            -- @go cmds@ should be empy here
            -}

        -- CPlug !(XCPlug x) (CPlugPhrase x, CPlugPhrase x)
        CPlugs ann (phrase0, phrase1, phrases) -> assert (null cmds) $ do
            ~(ndefns, phrase0':phrase1':phrases') <- fmap (first concat . unzip) $ for (phrase0:phrase1:phrases) $ \(ann, (inpwiths, outwiths), cmds) -> do
                let modenv = over envLcl $ set _2 inpwiths . set _3 outwiths
                (ndefns, cmds') <- localEnvSt modenv $ patternCompileCmds cmds
                return (ndefns, (ann, (inpwiths, outwiths), cmds'))
            return (ndefns, [CPlugs ann (phrase0', phrase1', phrases')])

        {-
        CPlugs ann (phrase0, phrase1, phrases) -> do
            ~(phrase0':phrase1':phrases') <- 
                traverseOf (traversed % _3) patternCompileCmds 
                $ phrase0:phrase1:phrases
            -- _ :: ((), ([ChIdentT], [ChIdentT]), NonEmpty (MplCmd MplTypeChecked))
            return $ [CPlugs ann (phrase0', phrase1', phrases')]
            -- return $ (CPlugs ann (phrase0', phrase1', phrases'):) <$>  go cmds
            -- @go cmds@ should always be empty here
        -}

        {- The idea is as follows.
            
            - Given
                @
                ...
                case <EXPR> of
                    Patt0(..) -> ...
                    Patt1(..) -> ...
                    Patt2(..) -> ...
                        ...
                    PattN(..) -> ...
                @
                where we let @seqs@, @ins@, @outs@ denote the context of 
                sequential variables, input channels, and output channels currently
                in scope at the @case <EXPR> of@ line.
            - Transform this to
                @
                run casep(seqs, <EXPR> | ins => outs )
                @
                where we define
                @
                proc casep =
                    seqs, Patt0(..) | ins => outs -> ...
                    seqs, Patt1(..) | ins => outs -> ...
                    seqs, Patt2(..) | ins => outs -> ...
                        ...
                    seqs, PattN(..) | ins => outs -> ...
                @
                and pattern compile casep appropriately..... (although in the code below, we
                pattern compile first, then construct casep process) 

            Honestly, a bit awkard, but this gets around the fact that there is no lambda
            lifting for processes, so we essentially ``do it ourselves'' here.

            Also, recall we do this since it forces the memoization of the result of the case
            expression -- regardless of the fact it is a tuple, data, or codata.... since a
            case really only evaluates it if it is data... 
        -}
        CCase ann expr pattscmds -> assert (null cmds) $ do
            (ndefnsexpr, expr') <- patternCompileExpr expr
            (ndefns, pattscmds') <- fmap (first concat . NE.unzip) $ for pattscmds $ \(patt, cmds) -> do
                (ndefns, cmds') <- localEnvSt (over envLcl((collectPattVars patt,[],[])<>)) $ patternCompileCmds cmds
                -- we have '[patt]' since when we later pattern compile it, we need a list of patterns.
                -- Indeed, we can only have one pattern.
                return (ndefns, ([patt], cmds'))

            tell $ bool [ _NonExhaustiveCCasePatt  # () ] [] $ 
                patternCompileExhaustiveCheck $ fmap (view _1) pattscmds'

            ~([PVar _ u], ncmds) <- localEnvSt id $ patternCompileConcPatPhrases pattscmds'

            -- building the new run function
            (seqs, ins, outs) <- guse envLcl
            casep <- freshCaseProcIdP
            let pfloated = MplProcess
                    casep
                    ( [] -- N.B. Should probably rescope the type variables here..
                    , map (\(PVar ann _) -> ann) seqs ++ [getExprType expr']
                    , map (view chIdentTType) ins
                    , map (view chIdentTType) outs
                    )
                    (((seqs ++ [PVar (getExprType expr') u], ins, outs), ncmds) :| [])

            -- TODO: Okay, refactor this so we don't retain the call information anymore
            -- this is useless and never used later.. then we can remove this error call..
            return 
                ( ndefnsexpr ++ ndefns ++ [ProcessDefn pfloated]
                , 
                    [ CRun 
                        ()
                        casep 
                        (map (\(PVar ann v) -> EVar ann v) seqs ++ [expr'])
                        ins 
                        outs
                    ]
                )
        {-
        CCase ann expr pattscmds -> do
            expr' <- patternCompileExpr expr
            pattscmds' <- fmap (over (mapped % _1) pure) $ traverseOf (traversed % _2) patternCompileCmds pattscmds
            ~([PVar _ u], ncmds) <- patternCompileConcPatPhrases pattscmds'
            let ncmds' = fmap (substituteVarIdentByExpr (u, expr')) ncmds

            tell $ bool [ _NonExhaustiveCCasePatt  # () ] [] $ 
                patternCompileExhaustiveCheck $ fmap (view _1) pattscmds'

            -- (NE.toList ncmds' <>) <$> go cmds
            return $ NE.toList ncmds' 
            -- @go cmds@ should always be empty list here
        -}

        {-
        CSwitch ann switches -> do
            traverseOf (traversed % _2) patternCompileCmds switches
                >>= traverseOf (traversed % _1) patternCompileExpr
                >>= patternCompileExhaustiveCSwitchCheck 
                >>= return . NE.toList . foldr f (pure $ CIllegalInstr ())
            -- @go cmds@ should always be empty list here
          where
            f (bexpr, thenc) acceq = pure $ CIf () bexpr thenc acceq
        -}
        CSwitch ann switches -> assert (null cmds) $ do
            (ndefns, switches') <- fmap (first concat . NE.unzip) $ for switches $ \(switchon, cmds) -> do
                (ndefnsexpr, switchon') <- patternCompileExpr switchon
                (ndefns, cmds') <- localEnvSt id $ patternCompileCmds cmds
                return (ndefnsexpr ++ ndefns, (switchon', cmds'))

            switches'' <- patternCompileExhaustiveCSwitchCheck switches'

            return (ndefns, NE.toList . foldr f (pure $ _CIllegalInstr # ()) $ switches'')
          where
            f (bexpr, thenc) acceq = pure $ CIf () bexpr thenc acceq

        {-
        CIf ann cond cthen celse -> 
            fmap pure $ CIf () <$> patternCompileExpr cond <*> patternCompileCmds cthen <*> patternCompileCmds celse
            -- @go cmds@ should always be empty list here
        -}

        CIf ann cond cthen celse -> assert (null cmds) $ do
            (ndefns0, cond') <- patternCompileExpr cond
            (ndefns1, cthen') <- localEnvSt id $ patternCompileCmds cthen
            (ndefns2, celse') <- localEnvSt id $ patternCompileCmds celse
            return (ndefns0 ++ ndefns1 ++ ndefns0, [CIf () cond' cthen' celse']) 
            -- fmap pure $ CIf () <$> patternCompileExpr cond <*> patternCompileCmds cthen <*> patternCompileCmds celse
            -- @go cmds@ should always be empty list here

{-
  CCase !(XCCase x)
          (XMplExpr x)
          (NonEmpty (XMplPattern x, NonEmpty (MplCmd x)))
  CSwitch !(XCSwitch x)
            (NonEmpty (XMplExpr x, NonEmpty (MplCmd x)))
-}

{- | Tests if a switch statement is exhaustive.
This will 'tell' an error if the pattern is non exhaustive, and return only the switches up to (and including)
the True.
-}
patternCompileExhaustiveESwitchCheck ::
    PatternCompile 
        (NonEmpty (MplExpr MplPatternCompiled, MplExpr MplPatternCompiled)) 
        (NonEmpty (MplExpr MplPatternCompiled, MplExpr MplPatternCompiled))
patternCompileExhaustiveESwitchCheck switches = 
     case trueandpast of
        [] -> tell [_NonExhaustiveSwitch # ()] >> return switches
        trueres:_ -> return $ NE.fromList $ notrue <> [trueres]
  where
    (notrue, trueandpast) = NE.break (fromMaybe False . preview (_1 % _EBool % _2)) switches

{- essentially duplciated code -}
patternCompileExhaustiveCSwitchCheck ::
    PatternCompile 
        (NonEmpty (MplExpr MplPatternCompiled, NonEmpty (MplCmd MplPatternCompiled))) 
        (NonEmpty (MplExpr MplPatternCompiled, NonEmpty (MplCmd MplPatternCompiled)))
patternCompileExhaustiveCSwitchCheck switches = 
     case trueandpast of
        [] -> tell [_NonExhaustiveCSwitch # ()] >> return switches
        trueres:_ -> return $ NE.fromList $ notrue <> [trueres]
  where
    (notrue, trueandpast) = NE.break (fromMaybe False . preview (_1 % _EBool % _2)) switches
    


{- | Tests if a pattern match is exhaustive i.e., tests if something like
@
    SomePattern(a,b,MorePatterns(c)), a,b -> < .. >
    f,a,b -> < .. >
@
is exhaustive. Returns true if it is, and false if it is not.
-}
patternCompileExhaustiveCheck ::
    NonEmpty [MplPattern MplTypeChecked] -> 
    Bool
patternCompileExhaustiveCheck = and . fmap go . transpose . NE.toList
  where
    go :: [MplPattern MplTypeChecked] -> Bool
    -- assert all patterns should be the same length
    go [] = True
    go patts@(a:as) = or 
        [ has (folded % _PVar) patts
        , has (folded % _PNull) patts

        -- each of the vertical lines of matches on records must be exhaustive as well.
        , and 
            [ isJust mdts
            , getAll $ foldMap (All . patternCompileExhaustiveCheck . NE.fromList . transpose . pure) dtspatts'
            ]

        -- tuple 
        , and 
            [ isJust mtuple
            , patternCompileExhaustiveCheck $ NE.fromList tuple
            ]

        -- we can actually exhaust all the bool types 
        -- (unlike Chars and Ints.. we just assume it is impossible to exhaust them completely)
        , and 
            [ isJust $ traverse  (preview _PBool) patts
            , orOf (folded % _PBool % _2) patts
            , orOf (folded % _PBool % _2 % to not) patts
            ]

        -- the built in list
        , and
            -- if all are lists, and we have a cons, and a null we are exhaustive 
            [ all (\patt -> has _PListCons patt || has _PList patt) patts
            , orOf (folded % _PListCons % to (const True)) patts
            , orOf (folded % _PList % _2 % to null) patts
            ]

        -- the built in unit test
        , and
            -- if all are units, we are done
            [ all (\patt -> has _PUnit patt) patts
            ]
            
        -- each constructor is present in this vertical line of patterns, and each of the patterns
        -- are itself exhaustive
        , and 
            [ isJust mcts
            , andOf (folded % typePhraseName % to (`elem` over mapped (view _2) cts)) allphrases
            , andOf (folded % to patternCompileExhaustiveCheck) (over (mapped % mapped) (view _3) groupedcts)
            ]
        ]
      where
        -- constructor
        mcts :: Maybe 
            [ 
                ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag) , XMplType MplTypeChecked)
                , IdentT
                , [MplPattern (MplPass 'TypeChecked)])
            ] 
        mcts = traverse (preview _PConstructor) patts
        cts = fromJust mcts
        
        groupedcts ::
            [ 
                NonEmpty
                ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag) , XMplType MplTypeChecked)
                , IdentT
                , [MplPattern (MplPass 'TypeChecked)]
                )
            ]
        groupedcts = NE.groupBy ((==) `on` view _2) $ sortBy (compare `on` view _2) cts
        tpclause = head cts ^. _1 % _1 % typePhraseExt :: MplTypeClause MplTypeChecked ('SeqObjTag 'DataDefnTag)
        allphrases = tpclause ^. typeClausePhrases

        -- destructors
        mdts :: Maybe
            [ 
                ( (Location, XMplType MplTypeChecked)
                , NonEmpty
                    ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
                    , IdentT
                    , MplPattern (MplPass 'TypeChecked)
                    )
                )
            ]
        mdts = traverse (preview _PRecord) patts
        dts = fromJust mdts

        {- Note: 
         -      - this will get the destructor name, and the patterns
         -      - then, it will put all the patterns for each destructor in a list
         -}
        dtspatts = dts 
            & mapped %~ view _2
            & mapped %~ NE.toList 
            & mapped % mapped %~ (view _2 &&& view _3)
        dtspatts' = Map.elems
            $ flip execState Map.empty
            $ forOf (traversed % traversed) dtspatts 
            $ \(dts, patt) -> modify (Map.alter (Just . maybe [patt] (patt:)) dts)

        -- tuple
        mtuple :: Maybe
               [((Location, XMplType MplTypeChecked),
                 (MplPattern (MplPass 'TypeChecked),
                  MplPattern (MplPass 'TypeChecked),
                  [MplPattern (MplPass 'TypeChecked)]))]
        mtuple = traverse (preview _PTuple) patts
        tuple = fromJust (traverse (preview _PTuple) patts)
            & mapped %~ view _2
            & mapped %~ \(a,b,c) -> a:b:c

{- | Compiles out string patterns to a lists of characters -}
removeStringPatt ::
    MplPattern MplTypeChecked -> 
    MplPattern MplTypeChecked  
removeStringPatt = \case
    PString ann str -> PList ann (map (PChar ann) str)
    n -> n

{- | Pattern compiles a pattern phrase i.e., this will pattern compile something like
@
    SomePattern(a,b,MorePatterns(c)), a,b -> <final result expression>
    f,a,b -> <final result expression>
    ....
@
to replace the patterns with variables and translate them into cases in the final expression.

-}
patternCompileSeqPatPhrases ::
    forall s m .
    ( MonadState s m
    , HasUniqueSupply s ) =>
    (NonEmpty ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)) -> 
    m ([MplPattern MplPatternCompiled], MplExpr MplPatternCompiled)
patternCompileSeqPatPhrases pattphrases = 
    -- assert that all the patterns have the same length
    assert ((length $ NE.group $ fmap (length . fst) pattphrases) == 1) $ do
        -- get the number of patterns
        let numpats = length $ fst $ NE.head pattphrases 
        -- then, we want to replace all the patterns with these unique pattern variables
        us <- replicateM numpats freshUIdP 
        -- indeed, we maintain the type information
        let usandtp = zipWith (curry (second getPattType)) us (fst . NE.head $ pattphrases) 
            (patts :: [MplPattern MplPatternCompiled]) = fmap (review _PVar . swap) usandtp

        -- we need to remove the string patterns to allow efficient compilation of strings
        pattexpr <- go 
            (map VarSub usandtp) 
            (NE.toList pattphrases) 
            (EIllegalInstr restp) 

        return (patts, pattexpr)  
  where
    -- the type of the codomain 
    restp :: XMplType MplTypeChecked
    restp = getExprType $ snd $ NE.head pattphrases

    

    go :: 
        [Substitutable]  -> 
        [([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)] -> 
        MplExpr MplPatternCompiled -> 
        _ (MplExpr MplPatternCompiled)
    go [] patts mexpr 
        | null patts = pure $ mexpr
        | otherwise = pure $ snd $ head patts 
    go (u:us) (fmap (first (map removeStringPatt))-> patts) mexpr = case groupedpatts of
        -- the case when all patts are of the same type
        _ :| [] -> fmap (fromJust . asum) 
            $ sequenceA 
                [ varrule
                , ctersrule
                , dstersrule
                , tuplerule
                , intrule
                , boolrule
                , charrule
                , listrule
                , unitrule
                ]
          where
            varrule :: _ (Maybe (MplExpr MplPatternCompiled))
            varrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match :: MplPattern MplTypeChecked -> Maybe (Either () IdentT)
                match patt
                    | Just identt <- patt ^? _PVar % _2 = _Just % _Right # identt
                    | Just _ <- patt ^? _PNull = _Just % _Left # ()
                    | otherwise = Nothing

                f :: NonEmpty (Either () IdentT) -> m (MplExpr MplPatternCompiled)
                f vars = go 
                    us 
                    (NE.toList $ NE.zipWith 
                        h
                        vars 
                        patttails
                        ) 
                    mexpr

                {- Either is
                 -      Left: a null pattern @_@
                 -      Right: a var pattern @u@
                 -}
                h :: Either () IdentT -> 
                    ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled) -> 
                    ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                h (Right v) pattsexpr = pattsexpr & _2 %~ substituteExpr (v,u)
                h _ pattsexpr = pattsexpr 

            -- rule for a built in list.. (essentially 'ctersrule' but hardcoded for lists)
            listrule  :: _ (Maybe (MplExpr MplPatternCompiled))
            listrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                -- Left is PListCons
                -- Right is Either PList or PString
                match :: MplPattern MplTypeChecked -> Maybe 
                    (Either 
                        ((Location, MplType MplTypeChecked), MplPattern MplTypeChecked, MplPattern MplTypeChecked)
                        ((Location, MplType MplTypeChecked), [MplPattern MplTypeChecked])
                    )
                match = \case 
                    PListCons cxt a b -> Just $ Left (cxt,a,b)
                    PList cxt lst -> Just $ Right (cxt, lst)
                    _ -> Nothing

                f :: NonEmpty _ -> m (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [lstcons, lstnil]
                    -- [ lstcons $ mapMaybe g0 $ NE.toList cters , lstnil  $ mapMaybe g1 $ NE.toList cters ]
                  where 
                    (consacc, nilacc) = foldl' g mempty $ reverse $ zip (NE.toList cters) (NE.toList patttails)
                      where
                        g acc (lstpatt, patttail) = case lstpatt of
                            Left (ann, l, r) -> first ((patttail & _1 %~ ([l, r]<>)):) acc
                            -- regular list
                            Right (ann, l:r) -> 
                                first ((patttail & _1 %~ ([l, PList ann r]<>)):) acc
                            Right (ann, []) -> second (patttail:) acc 
                            {-
                            -- string
                            Right (Right (ann, l:r)) -> 
                                first ((patttail & _1 %~ ([PChar ann l, PList ann $ map (PChar ann) r]<>)):) acc
                            Right (Right (ann, [])) -> second (patttail:) acc 
                            -}

                    -- get the type of the list.. this is a bit of a mess just by design of the 
                    -- language
                    ~tplst@(TypeBuiltIn (TypeListF _ tphead)) = getPattType $ fst $ NE.head pattheads 

                    lstcons = do
                        u0 <- freshUIdP
                        u1 <- freshUIdP

                        let patt' =  PSimpleListCons tplst u0 u1

                        if null consacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go (map VarSub [(u0, tphead), (u1, tplst)] ++ us) consacc mexpr

                    lstnil = do
                        let patt' =  PSimpleListEmpty tplst 

                        if null nilacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go us nilacc mexpr

                {-
                match :: MplPattern MplTypeChecked -> Maybe 
                    (Either 
                        ((Location, MplType MplTypeChecked), MplPattern MplTypeChecked, MplPattern MplTypeChecked)
                        ((Location, MplType MplTypeChecked), [MplPattern MplTypeChecked])
                    )
                match = \case 
                    PListCons cxt a b -> Just $ Left (cxt,a,b)
                    PList cxt lst -> Just $ Right (cxt, lst)
                    _ -> Nothing


                f :: NonEmpty _ -> m (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [lstcons, lstnil]
                    -- [ lstcons $ mapMaybe g0 $ NE.toList cters , lstnil  $ mapMaybe g1 $ NE.toList cters ]
                  where 
                    (consacc, nilacc) = foldl' g mempty $ reverse $ zip (NE.toList cters) (NE.toList patttails)
                      where
                        g acc (lstpatt, patttail) = case lstpatt of
                            Left (ann, l, r) -> first ((patttail & _1 %~ ([l, r]<>)):) acc
                            Right (ann, l:r) -> first ((patttail & _1 %~ ([l, PList ann r]<>)):) acc
                            Right (ann, []) -> second (patttail:) acc 

                    -- get the type of the list.. this is a bit of a mess just by design of the 
                    -- language
                    ~tplst@(TypeBuiltIn (TypeListF _ tphead)) = getPattType $ fst $ NE.head pattheads 

                    lstcons = do
                        u0 <- freshUIdP
                        u1 <- freshUIdP

                        let patt' =  PSimpleListCons tplst u0 u1

                        if null consacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go (map VarSub [(u0, tphead), (u1, tplst)] ++ us) consacc mexpr

                    lstnil = do
                        let patt' =  PSimpleListEmpty tplst 

                        if null nilacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go us nilacc mexpr
                    -}


            -- hard coded case for the unit rule
            unitrule  :: _ (Maybe (MplExpr MplPatternCompiled))
            unitrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match = preview _PSimpleUnit 

                f :: NonEmpty (XPSimpleUnit (MplPass 'TypeChecked)) -> m (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [unitpat]
                  where
                    unitpat = do
                        let patt' = PSimpleUnit ty
                        fmap (patt',) $ go us (NE.toList patttails) mexpr


            ctersrule :: _ (Maybe (MplExpr MplPatternCompiled))
            ctersrule = sequenceA $ match pattheads <&> f
              where
                match :: 
                    NonEmpty (MplPattern MplTypeChecked, MplExpr MplPatternCompiled) -> 
                    Maybe 
                        ( NonEmpty 
                            ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), MplType MplTypeChecked)
                            , IdentT
                            , [MplPattern (MplPass 'TypeChecked)])
                        )
                match = traverse (preview (_1 % _PConstructor))

                f :: NonEmpty 
                    ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), XMplType MplTypeChecked)
                    , IdentT
                    , [MplPattern (MplPass 'TypeChecked)]) -> _ (MplExpr MplPatternCompiled)
                f cters = fmap (ECase restp (getExprFromSubstitutable u) . NE.fromList) 
                    $ for (tpclause ^. typeClausePhrases) $ \phrase -> do
                        usandtps <- traverse (sequenceOf _1 . (freshUIdP,)) 
                            $ phrase ^. typePhraseFrom
                        -- let patt' = PSimpleConstructor (phrase, snd nu) (phrase ^. typePhraseName) $ usandtps
                        let patt' = PSimpleConstructor 
                                ( phrase
                                , getDataPhraseTypeResult phrase
                                ) 
                                (phrase ^. typePhraseName) 
                                $ usandtps
                        case ctersmap ^. at (phrase ^. typePhraseName) of
                            Just cternpatts -> (patt',) <$> 
                                go (map VarSub usandtps ++ us) (NE.toList cternpatts) mexpr
                            Nothing -> pure $ (patt',) mexpr
                    
                  where
                    tpclause = NE.head cters ^. _1 % _1 % typePhraseExt :: MplTypeClause MplTypeChecked ('SeqObjTag 'DataDefnTag)
                    ctersmap 
                        = foldl' g Map.empty 
                        $ NE.reverse  -- need to reverse for the order of the map
                        $ NE.zip cters patttails
                      where
                        g :: 
                            Map _ _ -> 
                            ( 
                                ( ( MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag)
                                  , MplType MplTypeChecked
                                  )
                                , IdentT
                                , [MplPattern (MplPass 'TypeChecked)]
                                )
                            , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)) -> 
                            Map _ _
                        g acc ((phrase, identt, cterpatts), patttail) = 
                            let patttail' = patttail & _1 %~ (cterpatts<>)
                            in acc & at identt %~ Just . maybe ( patttail' :| [] ) (NE.cons patttail')

            {- There's some somewhat complicated interactions going on here (this simlarly occurs with the tuple).. 
             - Since we did not choose to group based on destructors, we know that when 
             - we actually do get to a destructor case, this should:
             -          - be of length exactly 1 (otherwise we would go to the catch all case 
             -              of mixed patterns and partition based on the groups)
             -}
            dstersrule :: _ (Maybe (MplExpr MplPatternCompiled))
            dstersrule = sequenceA $ match patts <&> f
              where
                match :: 
                    [([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)] -> 
                    Maybe (((Location, MplType MplTypeChecked), NonEmpty
                      (MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag), IdentT,
                       MplPattern (MplPass 'TypeChecked))), ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled))
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PRecord
                match _ = Nothing

                f :: 
                    ( ( (Location, XMplType MplTypeChecked)
                      , NonEmpty
                            ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
                            , IdentT
                            , MplPattern (MplPass 'TypeChecked)
                            )
                      )
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    _ (MplExpr MplPatternCompiled)
                f (((_loc, _clausetp), recordphrases), (remainingpatts, resexpr)) = 
                    go 
                        ( NE.toList pattsubs ++ us)
                        [ ( NE.toList pattphrases ++ remainingpatts, resexpr) ]
                        mexpr
                  where
                    pattsubs = recordphrases & mapped %~ RecordSub u . (view _1 &&& view _2)
                    pattphrases = recordphrases & mapped %~ view _3

            tuplerule :: _ (Maybe (MplExpr MplPatternCompiled))
            tuplerule = sequenceA $ match patts <&> f
              where
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PTuple
                match _ = Nothing

                f :: 
                    ( 
                        ( (Location, XMplType MplTypeChecked)
                        , 
                            ( MplPattern (MplPass 'TypeChecked)
                            , MplPattern (MplPass 'TypeChecked)
                            , [MplPattern (MplPass 'TypeChecked)]
                            )
                        )
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> _ (MplExpr MplPatternCompiled)
                f (((_loc, TypeBuiltIn (TypeTupleF _ (ty0, ty1, tys)))
                    , (patt0, patt1, patts))
                    , (remainingpatts, remexpr)) = 
                        go (tuplesubs ++ us) [(tuplespatts ++ remainingpatts, remexpr)] mexpr
                  where
                    tuplesubs = zipWith (curry (TupleSub u)) (ty0:ty1:tys) [0..]
                    tuplespatts = patt0 : patt1 : patts

                -- TOOD change this to an exception later
                f _ = error "Error in compilation of pattern matching -- tuple is not a tuple type"

            {- constant rules. TODO: in the future, since the strucutre is the same for all three of these,
             - modify the AST so that it is just ONE constructor for ALL built in types like this. 
             -}
            intrule :: _ (Maybe (MplExpr MplPatternCompiled))
            intrule = sequenceA $ traverse (preview (_1 % _PInt)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Int) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Int)
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints 
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EInt # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 

            {- duplicted code from intrule for bool and char-}
            charrule :: _ (Maybe (MplExpr MplPatternCompiled))
            charrule = sequenceA $ traverse (preview (_1 % _PChar)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Char) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Char)
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeCharF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EChar # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 

            boolrule :: _ (Maybe (MplExpr MplPatternCompiled))
            boolrule = sequenceA $ traverse (preview (_1 % _PBool)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Bool) -> _ (MplExpr MplPatternCompiled)
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Bool)
                    , ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
                    ) -> 
                    MplExpr MplPatternCompiled -> 
                    _ (MplExpr MplPatternCompiled)
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EBool # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ EIf (getExprType thenc) ccond thenc accexpr 
                
        _ -> foldrM (go (u:us)) mexpr $ fmap NE.toList groupedpatts 
      where
        ty :: XMplType MplTypeChecked
        ty = getPattType $ fst $ NE.head pattheads

        -- Note: this is quite confusing that both 'pattheads' and 'pattails' both include the same expression.. honestly this was kind of a bad decision in the beginning and makes the code a bit more confusing than it should be.
        pattheads :: NonEmpty (MplPattern MplTypeChecked, MplExpr MplPatternCompiled)
        patttails :: NonEmpty ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled)
        (pattheads, patttails) = NE.unzip 
            $ fmap (\(~(p:ps), expr) -> ((p, expr), (ps, expr))) 
            $ NE.fromList patts

        {-
         - groups the patterns based on their "type" i.e., put them in groups of if they are
         -      - variables
         -      - constructors
         -      - records
         -      - tuples
         -}
        groupedpatts :: NonEmpty (NonEmpty ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled))
        groupedpatts =  NE.groupBy1 p $ NE.fromList patts
          where
            p :: 
                ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled) -> 
                ([MplPattern MplTypeChecked], MplExpr MplPatternCompiled) -> 
                Bool
            p a b =  let p' predicate = ((&&) `on`  predicate . head . fst) a b in or 
                [ p' (\n -> has _PVar n || has _PNull n) 
                , p' (has _PConstructor)

                -- built in primitive types
                , p' (has _PInt)
                , p' (has _PChar)
                , p' (has _PBool)

                , p' (\n -> has _PList n || has _PListCons n)
                , p' (has _PUnit) 

                -- , p' (has _PRecord)
                -- , p' (has _PTuple)
                ]



{- Essentially the same duplciated code from above.. this is a bit unfortunate from the way the AST is structured-}
patternCompileConcPatPhrases ::
    forall s m .
    ( MonadState s m
    , HasUniqueSupply s ) =>
    (NonEmpty ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))) -> 
    m ([MplPattern MplPatternCompiled], NonEmpty (MplCmd MplPatternCompiled))
patternCompileConcPatPhrases pattphrases = 
    -- assert that all the patterns have the same length
    assert ((length $ NE.group $ fmap (length . fst) pattphrases) == 1) $ do
        -- get the number of patterns
        let numpats = length $ fst $ NE.head pattphrases 
        -- then, we want to replace all the patterns with these unique pattern variables
        us <- replicateM numpats freshUIdP 
        -- indeed, we maintain the type information
        let usandtp = zipWith (curry (second getPattType)) us (fst . NE.head $ pattphrases) 
            (patts :: [MplPattern MplPatternCompiled]) = fmap (review _PVar . swap) usandtp

        pattexpr <- go 
            (map VarSub usandtp) 
            (NE.toList pattphrases) 
            (CIllegalInstr () :| [])

        return $ (patts, pattexpr)  
  where
    -- the type of the codomain 
    go :: 
        [Substitutable]  -> 
        [([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))] -> 
        NonEmpty (MplCmd MplPatternCompiled) -> 
        _ (NonEmpty (MplCmd MplPatternCompiled))
    go [] patts mexpr 
        | null patts = pure $ mexpr
        | otherwise = pure $ snd $ head patts 
    go (u:us) (fmap (first (map removeStringPatt)) -> patts) mexpr = case groupedpatts of
        -- the case when all patts are of the same type
        _ :| [] -> fmap (fromJust . asum) 
            $ sequenceA 
                [ varrule
                , ctersrule
                , dstersrule
                , tuplerule
                , intrule
                , boolrule
                , charrule
                , listrule
                , unitrule
                ]
          where
            varrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            varrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match :: MplPattern MplTypeChecked -> Maybe (Either () IdentT)
                match patt
                    | Just identt <- patt ^? _PVar % _2 = _Just % _Right # identt
                    | Just _ <- patt ^? _PNull = _Just % _Left # ()
                    | otherwise = Nothing

                f :: NonEmpty (Either () IdentT) -> m (NonEmpty (MplCmd MplPatternCompiled))
                f vars = go 
                    us 
                    (NE.toList $ NE.zipWith 
                        h
                        vars 
                        patttails
                        ) 
                    mexpr

                {- Either is
                 -      Left: a null pattern @_@
                 -      Right: a var pattern @u@
                 -}
                h :: Either () IdentT -> 
                    ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)) -> 
                    ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                h (Right v) pattsexpr = pattsexpr & _2 % mapped %~ substitute (v,u)
                h _ pattsexpr = pattsexpr 

            -- rule for a built in list.. (essentially 'ctersrule' but hardcoded for lists)
            listrule  :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            listrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                -- Left is PListCons
                -- Right is Either PList or PString
                match :: MplPattern MplTypeChecked -> Maybe 
                    (Either 
                        ((Location, MplType MplTypeChecked), MplPattern MplTypeChecked, MplPattern MplTypeChecked)
                        ((Location, MplType MplTypeChecked), [MplPattern MplTypeChecked])
                    )
                match = \case 
                    PListCons cxt a b -> Just $ Left (cxt,a,b)
                    PList cxt lst -> Just $ Right (cxt, lst)
                    _ -> Nothing

                f :: NonEmpty _ -> m (NonEmpty (MplCmd MplPatternCompiled))
                f cters = fmap (pure . CCase () (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [lstcons, lstnil]
                    -- [ lstcons $ mapMaybe g0 $ NE.toList cters , lstnil  $ mapMaybe g1 $ NE.toList cters ]
                  where 
                    (consacc, nilacc) = foldl' g mempty $ reverse $ zip (NE.toList cters) (NE.toList patttails)
                      where
                        g acc (lstpatt, patttail) = case lstpatt of
                            Left (ann, l, r) -> first ((patttail & _1 %~ ([l, r]<>)):) acc
                            -- regular list
                            Right (ann, l:r) -> first ((patttail & _1 %~ ([l, PList ann r]<>)):) acc
                            Right (ann, []) -> second (patttail:) acc 

                    -- get the type of the list.. this is a bit of a mess just by design of the 
                    -- language
                    ~tplst@(TypeBuiltIn (TypeListF _ tphead)) = getPattType $ fst $ NE.head pattheads 

                    lstcons = do
                        u0 <- freshUIdP
                        u1 <- freshUIdP

                        let patt' =  PSimpleListCons tplst u0 u1

                        if null consacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go (map VarSub [(u0, tphead), (u1, tplst)] ++ us) consacc mexpr

                    lstnil = do
                        let patt' =  PSimpleListEmpty tplst 

                        if null nilacc
                            then pure $ (patt',) mexpr
                            else fmap (patt',) $ go us nilacc mexpr



            -- hard coded case for the unit rule
            unitrule  :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            unitrule = sequenceA $ traverse (match . fst) pattheads <&> f
              where
                match = preview _PUnit

                ty = getPattType $ fst $ NE.head pattheads 

                f :: NonEmpty (XPUnit (MplPass 'TypeChecked)) -> m (NonEmpty (MplCmd MplPatternCompiled))
                f cters = fmap (pure . CCase () (getExprFromSubstitutable u) . NE.fromList) $ sequenceA [unitpat]
                  where
                    unitpat = do
                        let patt' = PSimpleUnit ty
                        fmap (patt',) $ go us (NE.toList patttails) mexpr
                        

            ctersrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            ctersrule = sequenceA $ match pattheads <&> f
              where
                match :: 
                    NonEmpty (MplPattern MplTypeChecked, NonEmpty (MplCmd MplPatternCompiled)) -> 
                    Maybe 
                        ( NonEmpty 
                            ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), MplType MplTypeChecked)
                            , IdentT
                            , [MplPattern (MplPass 'TypeChecked)])
                        )
                match = traverse (preview (_1 % _PConstructor))

                f :: NonEmpty 
                    ( (MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag), XMplType MplTypeChecked)
                    , IdentT
                    , [MplPattern (MplPass 'TypeChecked)]) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f cters = fmap (pure . CCase () (getExprFromSubstitutable u) . NE.fromList) 
                    $ for (tpclause ^. typeClausePhrases) $ \phrase -> do
                        usandtps <- traverse (sequenceOf _1 . (freshUIdP,)) 
                            $ phrase ^. typePhraseFrom
                        -- let patt' = PSimpleConstructor (phrase, snd nu) (phrase ^. typePhraseName) $ usandtps
                        let patt' = PSimpleConstructor 
                                ( phrase
                                , getDataPhraseTypeResult phrase
                                ) 
                                (phrase ^. typePhraseName) 
                                $ usandtps
                        case ctersmap ^. at (phrase ^. typePhraseName) of
                            Just cternpatts -> (patt',) <$> 
                                go (map VarSub usandtps ++ us) (NE.toList cternpatts) mexpr
                            Nothing -> pure $ (patt',) mexpr
                    
                  where
                    tpclause = NE.head cters ^. _1 % _1 % typePhraseExt :: MplTypeClause MplTypeChecked ('SeqObjTag 'DataDefnTag)
                    ctersmap 
                        = foldl' g Map.empty 
                        $ NE.reverse  -- need to reverse for the order of the map
                        $ NE.zip cters patttails
                      where
                        g :: 
                            Map _ _ -> 
                            ( 
                                ( ( MplTypePhrase MplTypeChecked ('SeqObjTag 'DataDefnTag)
                                  , MplType MplTypeChecked
                                  )
                                , IdentT
                                , [MplPattern (MplPass 'TypeChecked)]
                                )
                            , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))) -> 
                            Map _ _
                        g acc ((phrase, identt, cterpatts), patttail) = 
                            let patttail' = patttail & _1 %~ (cterpatts<>)
                            in acc & at identt %~ Just . maybe ( patttail' :| [] ) (NE.cons patttail')



            {- There's some somewhat complicated interactions going on here (this simlarly occurs with the tuple).. 
             - Since we did not choose to group based on destructors, we know that when 
             - we actually do get to a destructor case, this should:
             -          - be of length exactly 1 (otherwise we would go to the catch all case 
             -              of mixed patterns and partition based on the groups)
             -}
            dstersrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            dstersrule = sequenceA $ match patts <&> f
              where
                match :: 
                    [([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))] -> 
                    Maybe (((Location, MplType MplTypeChecked), NonEmpty
                      (MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag), IdentT,
                       MplPattern (MplPass 'TypeChecked))), ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)))
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PRecord
                match _ = Nothing

                f :: 
                    ( ( (Location, XMplType MplTypeChecked)
                      , NonEmpty
                            ( MplTypePhrase MplTypeChecked ('SeqObjTag 'CodataDefnTag)
                            , IdentT
                            , MplPattern (MplPass 'TypeChecked)
                            )
                      )
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                f (((_loc, _clausetp), recordphrases), (remainingpatts, resexpr)) = 
                    go 
                        ( NE.toList pattsubs ++ us)
                        [ ( NE.toList pattphrases ++ remainingpatts, resexpr) ]
                        mexpr
                  where
                    pattsubs = recordphrases & mapped %~ RecordSub u . (view _1 &&& view _2)
                    pattphrases = recordphrases & mapped %~ view _3

            tuplerule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            tuplerule = sequenceA $ match patts <&> f
              where
                match [(fstpatt:remainingpatts, resexpr)] = (,(remainingpatts, resexpr)) <$> fstpatt ^? _PTuple
                match _ = Nothing

                f :: 
                    ( 
                        ( (Location, XMplType MplTypeChecked)
                        , 
                            ( MplPattern (MplPass 'TypeChecked)
                            , MplPattern (MplPass 'TypeChecked)
                            , [MplPattern (MplPass 'TypeChecked)]
                            )
                        )
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f (((_loc, TypeBuiltIn (TypeTupleF _ (ty0, ty1, tys)))
                    , (patt0, patt1, patts))
                    , (remainingpatts, remexpr)) = 
                        go (tuplesubs ++ us) [(tuplespatts ++ remainingpatts, remexpr)] mexpr
                  where
                    tuplesubs = zipWith (curry (TupleSub u)) (ty0:ty1:tys) [0..]
                    tuplespatts = patt0 : patt1 : patts

                -- TOOD change this to an exception later
                f _ = error "Error in compilation of pattern matching -- tuple is not a tuple type"

            {- constant rules. TODO: in the future, since the strucutre is the same for all three of these,
             - modify the AST so that it is just ONE constructor for ALL built in types like this. 
             -}
            intrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            intrule = sequenceA $ traverse (preview (_1 % _PInt)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Int) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Int)
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    NonEmpty (MplCmd MplPatternCompiled) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                g ints accexpr = do 
                    let hints = NE.head ints 
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EInt # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ pure $ CIf () ccond thenc accexpr 

            {- duplicted code from intrule for bool and char-}
            charrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            charrule = sequenceA $ traverse (preview (_1 % _PChar)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Char) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Char)
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    NonEmpty (MplCmd MplPatternCompiled) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeCharF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EChar # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ pure $ CIf () ccond thenc accexpr 

            boolrule :: _ (Maybe (NonEmpty (MplCmd MplPatternCompiled)))
            boolrule = sequenceA $ traverse (preview (_1 % _PBool)) pattheads <&> f
              where
                f :: NonEmpty ((Location, XMplType MplTypeChecked), Bool) -> _ (NonEmpty (MplCmd MplPatternCompiled))
                f ints = foldrM g mexpr  
                    $ NE.groupBy1 ((==) `on` view (_1 % _2)) 
                    $ NE.zip ints patttails

                g :: NonEmpty 
                    ( ((Location, MplType MplTypeChecked), Bool)
                    , ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
                    ) -> 
                    NonEmpty (MplCmd MplPatternCompiled) -> 
                    _ (NonEmpty (MplCmd MplPatternCompiled))
                g ints accexpr = do 
                    let hints = NE.head ints -- head ints
                        ccond = 
                            _EPOps #
                                ( _TypeBoolF # Nothing
                                , PrimitiveEq
                                , getExprFromSubstitutable u
                                , _EBool # (hints ^. _1 % _1 % _2, hints ^. _1 % _2)
                                )
                    thenc <- go us (NE.toList $ ints & mapped %~ view _2) mexpr
                    return $ pure $ CIf () ccond thenc accexpr 
                
        _ -> foldrM (go (u:us)) mexpr $ fmap NE.toList groupedpatts 
      where
        -- Note: this is quite confusing that both 'pattheads' and 'pattails' both include the same expression.. honestly this was kind of a bad decision in the beginning and makes the code a bit more confusing than it should be.
        pattheads :: NonEmpty (MplPattern MplTypeChecked, NonEmpty (MplCmd MplPatternCompiled))
        patttails :: NonEmpty ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled))
        (pattheads, patttails) = NE.unzip 
            $ fmap (\(~(p:ps), expr) -> ((p, expr), (ps, expr))) 
            $ NE.fromList patts

        {-
         - groups the patterns based on their "type" i.e., put them in groups of if they are
         -      - variables
         -      - constructors
         -      - records
         -      - tuples
         -}
        groupedpatts :: NonEmpty (NonEmpty ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)))
        groupedpatts =  NE.groupBy1 p $ NE.fromList patts
          where
            p :: 
                ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)) -> 
                ([MplPattern MplTypeChecked], NonEmpty (MplCmd MplPatternCompiled)) -> 
                Bool
            p a b =  let p' predicate = ((&&) `on`  predicate . head . fst) a b in or 
                [ p' (\n -> has _PVar n || has _PNull n) 
                , p' (has _PConstructor)

                -- built in primitive types
                , p' (has _PInt)
                , p' (has _PChar)
                , p' (has _PBool)

                , p' (\n -> has _PList n || has _PListCons n)
                , p' (has _PUnit) 

                -- , p' (has _PRecord)
                -- , p' (has _PTuple)
                ]

