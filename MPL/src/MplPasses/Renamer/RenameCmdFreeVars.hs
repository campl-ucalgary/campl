{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Monad.Writer

import MplUtil.UniqueSupply

import Data.Maybe
import Data.Functor.Foldable (Base, cata)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Foldable


import Data.Foldable
import Data.Void

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
type instance XCPlug MplCmdFreeVars = Void
type instance XCPlugs MplCmdFreeVars = (KeyWordNameOcc, (ContextInfo, [IdP MplCmdFreeVars]))
    -- plug command, plug scoped bound variables.
type instance XCCase MplCmdFreeVars = KeyWordNameOcc
type instance XCSwitch MplCmdFreeVars = KeyWordNameOcc
type instance XCHCasePhrase MplCmdFreeVars  = ()
type instance XCForkPhrase MplCmdFreeVars  = (ContextInfo, [IdP MplCmdFreeVars])
type instance XCPlugPhrase MplCmdFreeVars  = (ContextInfo, [IdP MplCmdFreeVars])
type instance XXCmd MplCmdFreeVars = Void 

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
        cases' <- traverse g cases
        return $ CHCase cxt ch cases'
      where
        g (cxt, ident, cmds) = do
            cmds' <- cmdsBindFreeVars cmds
            return (cxt, ident, cmds')
    f (CHPut cxt ident ch) = do
        equality %= (ch:)
        return $ CHPut cxt ident ch

    f (CSplit cxt ch (ch1, ch2)) = do
        equality %= (ch:) . filter (not . (`elem`[ch1,ch2])) . nub
        return $ CSplit cxt ch (ch1, ch2)
    f (CFork cxt ch ((ch1, cxt1, cmds1), (ch2, cxt2, cmds2))) = do
        -- some invalid programs will have free variables after this
        -- fork command. We reset to use these free variables again
        -- because they technically are a part of the context.
        initfreevars <- guse equality

        cmds1' <- cmdsBindFreeVars cmds1
        cxt1' <- guses equality (filter (/=ch1) . nub)

        -- the free variables from each fork branch.. These
        -- are disjoint.
        equality .= initfreevars

        cmds2' <- cmdsBindFreeVars cmds2
        cxt2' <- guses equality (filter (/=ch2) . nub)

        -- reset the free vars (indeed, this when the fork command is
        -- empty, but add the forked on variable which is indeed free)
        equality .= ch:initfreevars

        -- return $ CFork cxt ch ((ch1, fromMaybe cxt1' cxt1, cmds1), (ch2, fromMaybe cxt2' cxt2, cmds2))
        return $ CFork cxt ch 
            ( (ch1, fromMaybe (ComputedContext, cxt1') ((UserProvidedContext,) <$> cxt1), cmds1')
            , (ch2, fromMaybe (ComputedContext, cxt2') ((UserProvidedContext,) <$> cxt2), cmds2')
            )

    f cmd@(CId cxt (s, t)) = do
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
        phr1' <- g initfreevars phr1
        phr2' <- g initfreevars phr2
        phrs' <- traverse (g initfreevars) phrs
        -- return $ CPlugs (cxt, ) (phr1', phr2', phrs')
        cxt' <- guse equality
        equality .= initfreevars
        return $ CPlugs (over _2 (fromMaybe (ComputedContext, cxt')) (fmap (UserProvidedContext,) <$> cxt)) (phr1', phr2', phrs')
      where
        g initfreevars (cxt, cmds) = do
            cmds' <- cmdsBindFreeVars cmds
            cxt' <- guse equality
            return (fromMaybe (ComputedContext, cxt') ((UserProvidedContext,) <$> cxt), cmds')

    f (CCase cxt expr cases) = do   
        cases' <- traverse g cases
        return $ CCase cxt expr cases'
      where
        g (patt, cmds) = do
            cmds' <- cmdsBindFreeVars cmds
            return (patt, cmds')
    f (CSwitch cxt switches) = do
        switches' <- traverse g switches
        return $ CSwitch cxt switches'
      where
        g (expr, cmds) = (expr,) <$> cmdsBindFreeVars cmds
