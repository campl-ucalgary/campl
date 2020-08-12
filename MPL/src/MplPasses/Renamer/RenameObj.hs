{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MplPasses.Renamer.RenameObj where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.Renamer.RenameUtils
import MplPasses.Renamer.RenameSym
import MplPasses.Renamer.RenameErrors
import MplPasses.Renamer.RenameType

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import MplUtil.UniqueSupply

import Data.Bool
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable

renameTypeClauseSpine :: 
   RenameTypePhrase t => 
    Rename (MplTypeClauseSpine MplParsed t)
        (MplTypeClauseSpine MplRenamed t)
renameTypeClauseSpine (UMplTypeClauseSpine spine) = do
    -- first, check if all the args are the same 
    -- (mutually recursive types must have the same type variables)
    tell $ bool [] 
        [_TypeClauseGraphArgsMustContainTheSameTypeVariables # allargseqclasses] 
        $ length allargs >= 2
    
    -- moreover, we need to test for overlapping declarations of the 
    -- arguments and the state vars
    tell $ overlappingDeclarations 
        $ fold allargs <> NE.toList allstatevars

    -- tag the args and state vars
    args' <- traverse tagIdentP args 
    statevars' <- traverse tagIdentP allstatevars

    -- add the arguments to the symbol table (all the args 
    -- should be the the same)
    symTab %= ((collectSymTab args' &
        mapped % _2 % symEntryInfo .~ _Just % _SymTypeVar # ())<>)

    -- add the state vars to the symbol table 
    symTab %= ((collectSymTab statevars' & 
        mapped % _2 % symEntryInfo .~ _Just % _SymTypeVar # ())<>)

    -- then rename the type clause
    spine' <- traverse renameTypeClause spine
    return $ UMplTypeClauseSpine spine'
  where
    allargs = fmap (view typeClauseArgs) spine 
    allargseqclasses = NE.group allargs
    args = nub $ concat $ foldMap NE.toList $ allargseqclasses

    allstatevars = fmap (view typeClauseStateVar) spine 

renameTypeClause ::
   RenameTypePhrase t => 
   Rename (MplTypeClause MplParsed t) (MplTypeClause MplRenamed t)
renameTypeClause clause = do
    symtab <- guse symTab
    -- tag the clause
    name' <- clause ^. typeClauseName % to tagIdentP

    -- look up the arguments
    tell $ outOfScopes symtab (clause ^. typeClauseArgs)
    let args' = fromJust $ clause ^. typeClauseArgs 
            % to (traverse (flip lookupTypeVar symtab))
        -- Note: we know the state variables and 
        -- the type variables are disjoint, so we know we can safely
        -- look this up
        st' = fromJust $ lookupTypeVar (clause ^. typeClauseStateVar) symtab

    phrases' <- traverse 
        (renameTypePhrase 
            (clause ^. typeClauseStateVar, st' ^. uniqueTag)) 
        (clause ^. typeClausePhrases)

    return $ _MplTypeClause #
        ( name' 
        , zipWith 
            (\arg arg' -> _IdentR # (arg, arg' ^. symEntryUniqueTag)) 
                (clause ^. typeClauseArgs) args'
        , undefined 
        , phrases' 
        , ())

class RenameTypePhrase (t :: SMplObjectDefn) where
    renameTypePhrase :: 
        ( AsRenameError e
        , MonadState RenameEnv m
        , MonadWriter [e] m
        , MonadFix m ) => 
            (IdentP, UniqueTag) -> 
            MplTypePhrase MplParsed t -> 
            m (MplTypePhrase MplRenamed t)

instance RenameTypePhrase SDataDefn where
    renameTypePhrase st typephrase = do
        symtab <- guse symTab
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let vfroms = typephrase ^. typePhraseFrom
            vto = typephrase ^. typePhraseTo
            vto' = _IdentR # (vto, snd st)

        vfroms' <- traverse ((`runReaderT`symtab) . renameScopedType) 
            vfroms

        tell $ bool [] [_ExpectedStateVarButGot # (fst st, vto)]
            (vto /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , vfroms'
            , _TypeVar # ((), vto', [])
            , ()
            )

instance RenameTypePhrase SCodataDefn where
    renameTypePhrase st typephrase = do
        symtab <- guse symTab
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let (vfroms, vst) = typephrase ^. typePhraseFrom
            vto = typephrase ^. typePhraseTo
            vst' = _IdentR # (vst, snd st)

        vfroms' <- traverse ((`runReaderT`symtab) . renameScopedType) 
            vfroms
        vto' <- (`runReaderT`symtab) . renameScopedType $ vto

        tell $ bool [] [_ExpectedStateVarButGot # (fst st, vst)]
            (vst /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , (vfroms', _TypeVar # ((), vst', []))
            , vto'
            , ()
            )

instance RenameTypePhrase SProtocolDefn where
    renameTypePhrase st typephrase = do
        symtab <- guse symTab
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let vfrom = typephrase ^. typePhraseFrom
            vto = typephrase ^. typePhraseTo
            vto' = _IdentR # (vto, snd st)

        vfrom' <- (`runReaderT`symtab) . renameScopedType $ vfrom

        tell $ bool [] [_ExpectedStateVarButGot # (fst st, vto)]
            (vto /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , vfrom'
            , (_TypeVar # ((), vto', []))
            , ()
            )

instance RenameTypePhrase SCoprotocolDefn where
    renameTypePhrase st typephrase = do
        symtab <- guse symTab
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let vfrom = typephrase ^. typePhraseFrom
            vfrom' = _IdentR # (vfrom, snd st)
            vto = typephrase ^. typePhraseTo

        vto' <- (`runReaderT`symtab) . renameScopedType $ vto

        tell $ bool [] [_ExpectedStateVarButGot # (fst st, vfrom)]
            (vfrom /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , (_TypeVar # ((), vfrom', []))
            , vto'
            , ()
            )


