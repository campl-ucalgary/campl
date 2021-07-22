{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Renamer.RenameCmdFreeVars where

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
import Control.Monad.Reader
import Control.Monad.Writer

import MplUtil.UniqueSupply

import Control.Arrow
import Data.Maybe
import Data.Functor.Foldable (Base, cata)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable
import Data.Functor.Foldable

import Debug.Trace

import Data.Foldable
import Data.Void

import Debug.Trace
import qualified Text.Show.Pretty as PrettyShow

{- Module for getting the free variables in mpl commands..
 - This is needed to find the context for which a Fork / plug command
 - are called with. The algorithm is as follows.
 -  - First, work bottom up and collect all of the free variables at each binder
 -  - Then, work top down (while knowing the variables that are actually bound)
 -      and with that, resolve the variable declarations working down the AST
 -}

type FreeVars = [IdP MplParsed]

data MplCmdFreeVars

data ContextInfo =
    UserProvidedContext
    | ComputedContext
  deriving (Show, Eq)

type instance IdP MplCmdFreeVars = IdP MplParsed
type instance ChP MplCmdFreeVars = ChP MplParsed

type instance XMplExpr MplCmdFreeVars = XMplExpr MplParsed
type instance XMplPattern MplCmdFreeVars = XMplPattern MplParsed

-- Process Command
type instance XMplCmd MplCmdFreeVars = MplCmd MplCmdFreeVars
type instance XCRun MplCmdFreeVars = XCRun MplParsed
type instance XCClose MplCmdFreeVars = KeyWordNameOcc
type instance XCHalt MplCmdFreeVars = KeyWordNameOcc
type instance XCGet MplCmdFreeVars = KeyWordNameOcc
type instance XCPut MplCmdFreeVars = KeyWordNameOcc
type instance XCHCase MplCmdFreeVars = KeyWordNameOcc
type instance XCHPut MplCmdFreeVars = KeyWordNameOcc
type instance XCSplit MplCmdFreeVars = KeyWordNameOcc
type instance XCFork MplCmdFreeVars = KeyWordNameOcc
type instance XCId MplCmdFreeVars = KeyWordNameOcc
type instance XCIdNeg MplCmdFreeVars = KeyWordNameOcc
type instance XCRace MplCmdFreeVars = KeyWordNameOcc
type instance XCPlugs MplCmdFreeVars = (KeyWordNameOcc, (ContextInfo, [IdP MplCmdFreeVars]))
    -- plug command, plug scoped bound variables.
type instance XCCase MplCmdFreeVars = KeyWordNameOcc
type instance XCCasePattern MplCmdFreeVars = XMplPattern MplCmdFreeVars
type instance XCSwitch MplCmdFreeVars = KeyWordNameOcc
type instance XCIf MplCmdFreeVars = KeyWordNameOcc
type instance XCHCasePhrase MplCmdFreeVars  = ()
type instance XCForkPhrase MplCmdFreeVars  = (ContextInfo, [IdP MplCmdFreeVars])
type instance XCPlugPhrase MplCmdFreeVars  = ()
type instance XCIllegalInstr MplCmdFreeVars  = Void
type instance XXCmd MplCmdFreeVars = Void 


{- | This will essentially move the free variables bottom up, and store them in spots in the
AST so that the context of a @plug@ or a @fork@ command can be computed -}
cmdsBindFreeVars ::
    ( MonadState FreeVars m ) =>
    NonEmpty (MplCmd MplParsed) -> 
    m (NonEmpty (MplCmd MplCmdFreeVars))
cmdsBindFreeVars = fmap NE.fromList . foldr f (pure []) . NE.toList
  where
    f a as = do
        -- note the order of how we sequence the effects here...
        as' <- as
        a' <- cmdBindFreeVars a 
        return (a':as') 

cmdBindFreeVars ::
    ( MonadState FreeVars m ) =>
    MplCmd MplParsed -> 
    m (MplCmd MplCmdFreeVars)
cmdBindFreeVars = f
  where
    f :: ( MonadState [IdP MplParsed] m ) => MplCmd MplParsed -> m (MplCmd MplCmdFreeVars)
    f (CRun cxt ident seqs ins outs) = do
        equality %= ((ins <> outs) <>)
        return $ CRun cxt ident seqs ins outs 
    
    f (CClose cxt ch) = do
        equality %= (ch:)
        return $ CClose cxt ch
    f (CHalt cxt ch) = do
        equality %= (ch:)
        return $ CHalt cxt ch

    f (CGet cxt patt ch) = do
        equality %= (ch:)
        return $ CGet cxt patt ch
    f (CPut cxt expr ch) = do
        equality %= (ch:)
        return $ CPut cxt expr ch
        
    f (CHCase cxt ch cases) = do
        equality %= (ch:)

        (nst, cases') <- fmap NE.unzip $ traverse g cases

        equality .= foldr union [] nst

        return $ CHCase cxt ch cases'
      where
        g (cxt, ident, cmds) = do
            (nst, cmds') <- localState $ cmdsBindFreeVars cmds
            return (nst, (cxt, ident, cmds'))
    f (CHPut cxt ident ch) = do
        equality %= (ch:)
        return $ CHPut cxt ident ch

    f (CSplit cxt ch (ch1, ch2)) = do
        equality %= (ch:) . filter (not . (`elem`[ch1,ch2])) 
        return $ CSplit cxt ch (ch1, ch2)
    f (CFork cxt ch ((ch1, cxt1, cmds1), (ch2, cxt2, cmds2))) = do
        -- some invalid programs will have free variables after this
        -- fork command. We reset to use these free variables again
        -- because they technically are a part of the context.
        initfreevars <- guse equality

        equality .= mempty
        cmds1' <- cmdsBindFreeVars cmds1
        cxt1' <- guses equality (filter (/=ch1) . nub)

        -- the free variables from each fork branch.. These
        -- are disjoint.
        equality .= mempty

        cmds2' <- cmdsBindFreeVars cmds2
        cxt2' <- guses equality (filter (/=ch2) . nub)

        -- pass the free variables up more..
        equality .= nub (ch:initfreevars ++ cxt2' ++ cxt1')

        -- return $ CFork cxt ch ((ch1, fromMaybe cxt1' cxt1, cmds1), (ch2, fromMaybe cxt2' cxt2, cmds2))
        return $ CFork cxt ch
            ( (ch1, fromMaybe (ComputedContext, cxt1') ((UserProvidedContext,) <$> cxt1), cmds1')
            , (ch2, fromMaybe (ComputedContext, cxt2') ((UserProvidedContext,) <$> cxt2), cmds2')
            )

    f (CId cxt (s, t)) = do
        equality %= ([s,t]<>)
        return $ CId cxt (s,t)
    f (CIdNeg cxt (s, t)) = do
        equality %= ([s,t]<>)
        return $ CIdNeg cxt (s,t)
        
    f (CRace cxt races) = do
        races' <- traverse g races
        return $ CRace cxt races'
      where
        g (ch, cmds) = do
            cmds' <- cmdsBindFreeVars cmds
            equality %= (ch:)
            return (ch, cmds')

    f (CPlugs cxt (phr1, phr2, phrs)) = do
        initfreevars <- guse equality
        (free1, phr1') <- g initfreevars phr1
        (free2, phr2') <- g initfreevars phr2
        (free3, phrs') <- unzip <$> traverse (g initfreevars) phrs
        equality .= initfreevars
        equality %= ((nub $ free1 <> free2 <> concat free3)<>)
        return $ CPlugs 
            (over _2 (fromMaybe (ComputedContext, cxt')) (fmap (UserProvidedContext,) <$> cxt)) 
            (phr1', phr2', phrs')
      where
        g initfreevars (cxt, (ins, outs), cmds) = do
            equality .= initfreevars
            cmds' <- cmdsBindFreeVars cmds
            vs <- guse equality
            -- return ((\\outs) . (\\ins) . nub $ vs, (cxt, (ins,outs), cmds'))
            return (nub $ vs ++ ins ++ outs, (cxt, (ins,outs), cmds'))

        -- we need to do this filter since we are only interested
        cxt' = nub 
            $ filter (uncurry (&&) <<< (`elem` allinputs) &&& (`elem` alloutputs))
            $ concatMap (uncurry (<>) . view _2) (phr1:phr2:phrs)

        allinputs =  foldOf (folded % _2 % _1) $ phr1:phr2:phrs
        alloutputs =  foldOf (folded % _2 % _2) $ phr1:phr2:phrs


    f (CCase cxt expr cases) = do   
        (nst, cases') <- fmap NE.unzip $ traverse g cases

        equality .= foldr union [] nst

        return $ CCase cxt expr cases'
      where
        g (patt, cmds) = do
            (nst, cmds') <- localState $ cmdsBindFreeVars cmds
            return (nst, (patt, cmds'))

    f (CSwitch cxt switches) = do
        (nst, switches') <- fmap NE.unzip $ traverse g switches

        equality .= foldr union [] nst

        return $ CSwitch cxt switches'
      where
        g (expr, cmds) = do
            (nst, cmds') <- localState $ cmdsBindFreeVars cmds
            return (nst, (expr, cmds'))

    f (CIf cxt condc thenc elsec) = do
        (thennst, thenc') <- localState $ cmdsBindFreeVars thenc
        (elsenst, elsec') <- localState $ cmdsBindFreeVars elsec
        equality .= thennst `union` elsenst
        return $ CIf cxt condc thenc' elsec'

    localState ma = do
        oldst <- guse equality
        a <- ma 
        nst <- guse equality
        equality .= oldst
        return (nst, a)
        

{- | This will correct the contxt AFTER computing the free variables. 
See note above for why this is necessary..
-}
cmdsCorrectContext ::
    -- | given context
    [IdentP] ->
    -- | cmds
    NonEmpty (MplCmd MplCmdFreeVars) -> 
    -- | resulting cmds after correcting the contexts
    NonEmpty (MplCmd MplCmdFreeVars)
-- cmdsCorrectContext context = fmap (cata go)
cmdsCorrectContext context = NE.fromList . go context . NE.toList
  where
    go :: [IdentP] -> 
        [MplCmd MplCmdFreeVars] -> 
        [MplCmd MplCmdFreeVars]
    go context (cmd:cmds) = case cmd of
        -- CForkF cxt ch ((ch1, cxt1, cmds1), (ch2, cxt2, cmds2)) -> undefined
        CFork cxt ch (lcmds, rcmds) -> CFork cxt ch (f lcmds, f rcmds) : go context cmds
          where
            f :: (IdentP, (ContextInfo, [IdentP]), NonEmpty (MplCmd MplCmdFreeVars)) -> 
                (IdentP, (ContextInfo, [IdentP]), NonEmpty (MplCmd MplCmdFreeVars))
            {-
            f (ch', (ComputedContext, cxt'), cmds') = 
                ( ch'
                , (ComputedContext, cxt')
                , cmdsCorrectContext cmds'
                )
            -}
            f (ch', (ComputedContext, cxt'), cmds') = 
                ( ch' 
                , (ComputedContext, filter (`elem` context) cxt') 
                , cmdsCorrectContext (ch':context) cmds')
            f (ch', cxt', cmds') = ( ch' , cxt' , cmdsCorrectContext ((ch':context) \\ [ch]) cmds')
            -- _ cxt1
        CPlugs cxt (phr1, phr2, phrs)
            -> CPlugs cxt' (f phr1, f phr2, map f phrs) : go context cmds
          where 
            f :: ((), ([IdentP], [IdentP]), NonEmpty (MplCmd MplCmdFreeVars)) ->
                    ((), ([IdentP], [IdentP]), NonEmpty (MplCmd MplCmdFreeVars)) 

            f (ann, splt, cmds') = (ann, splt, cmdsCorrectContext (cxt' ^. _2 % _2 ++ context) cmds')
 
            cxt' = over _2 g cxt
              where
                g :: (ContextInfo, [IdentP]) -> (ContextInfo, [IdentP])
                g (ComputedContext, ids) = (ComputedContext, filter (`notElem` context ) ids)
                g res = res

        CRun ann id expr ins outs -> CRun ann id expr ins outs :  go context cmds
        CClose ann ch -> CClose ann ch : go context cmds
        CHalt ann ch -> CHalt ann ch : go context cmds
        CGet ann pat ch -> CGet ann pat ch : go context cmds
        CPut ann expr ch -> CPut ann expr ch : go context cmds
        CHCase ann ch phrases -> CHCase ann ch (phrases & mapped % _3 %~ cmdsCorrectContext context) : go context cmds
        CHPut ann idp chp -> CHPut ann idp chp : go context cmds
        CSplit ann chp (lchp,rchp) -> CSplit ann chp (lchp,rchp) : go ((lchp:rchp:context) \\ [chp]) cmds
        CId ann (lch,rch) -> CId ann (lch,rch) : go context cmds
        CIdNeg ann (lch, rch) -> CIdNeg ann (lch, rch) : go context cmds
        CRace ann races -> CRace ann (races & mapped % _2 %~ cmdsCorrectContext context) : go context cmds
        CCase ann expr cases -> CCase ann expr (cases & mapped % _2 %~ cmdsCorrectContext context) : go context cmds
        CSwitch ann switches ->
            CSwitch ann (switches & mapped % _2 %~ cmdsCorrectContext context) : go context cmds

        CIf ann expr thenc elsec ->
            CIf ann expr (cmdsCorrectContext context thenc) (cmdsCorrectContext context elsec) : go context cmds

        
    go _context [] = []
