{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PolyKinds #-}
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
import MplPasses.Env

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import MplUtil.UniqueSupply

import Data.Bool
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Function

import Debug.Trace

import Data.Foldable

renameTypeClauseSpine :: 
    ( TypeClauseVarsSymTab t 
    , OverlappingDeclarations (MplTypeClauseSpine MplParsed t) 
    , RenameTypeClause t
    , RenameTypePhrase t 
    ) => 
    Rename (MplTypeClauseSpine MplParsed t)
        (MplTypeClauseSpine MplRenamed t)
renameTypeClauseSpine spine = do
    -- first, check if all the args are the same 
    -- (mutually recursive types must have the same type variables)
    -- tell $ typeClauseSpineSameVarError spine
    -- moreover, we need to test for overlapping declarations of the 
    -- arguments and the state vars
    tell $ overlappingDeclarations spine

    -- TODO: This should all be moved into 1 traverse call, so it puts things
    -- in the symbol table (but does not duplicate additions in the symbol) 
    -- table as it goes along!
    --
    -- get the type variable symbol table declarations // add to the symbol table.
    -- Note that all type clauses SHOULD share the same type variables! So we only include
    -- the variables in common -- hence the nubBy call... 
    stargs' <- nubBy ((==) `on` fst) . concat 
        <$> traverse typeClauseVarsSymTab (spine ^. typeClauseSpineClauses)

    envLcl %= (stargs'<>)

    -- rename the type clauses
    spine' <- traverse renameTypeClause (spine ^. typeClauseSpineClauses)
    return $ MplTypeClauseSpine spine' ()

class TypeClauseVarsSymTab t where
    typeClauseVarsSymTab :: 
        ( HasUniqueSupply s 
        , MonadState s m 
        , RenameTypePhrase t ) =>
        MplTypeClause MplParsed t -> 
        m SymTab

instance TypeClauseVarsSymTab (SeqObjTag t) where
    typeClauseVarsSymTab clause = do
        stargs' <- traverse tagIdentP $ st:args
        return $ collectSymTab stargs' & mapped % _2 % symEntryInfo .~ _Just % _SymTypeVar # ()
      where
        args = clause ^. typeClauseArgs
        st = clause ^. typeClauseStateVar
        

instance TypeClauseVarsSymTab (ConcObjTag t) where
    typeClauseVarsSymTab clause = do
        stargs' <- traverse tagIdentP $ st:args
        return $ collectSymTab stargs' & mapped % _2 % symEntryInfo .~ _Just % _SymTypeVar # ()
      where
        args = clause ^. typeClauseArgs % to (uncurry mappend)
        st = clause ^. typeClauseStateVar

class RenameTypeClause t where
    renameTypeClause :: RenameTypePhrase t =>
        Rename (MplTypeClause MplParsed t) (MplTypeClause MplRenamed t)

instance RenameTypeClause (SeqObjTag t) where
    renameTypeClause clause = do
        symtab <- guse envLcl
        -- tag the clause
        name' <- clause ^. typeClauseName % to tagIdentP

        -- look up the arguments
        tell $ outOfScopesWith lookupTypeVar symtab (clause ^. typeClauseArgs)
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
            , zipWith tagIdentPWithSymEntry (clause ^. typeClauseArgs) args'
            , tagIdentPWithSymEntry (clause ^. typeClauseStateVar) st'
            , phrases' 
            , ())

instance RenameTypeClause (ConcObjTag t) where
    renameTypeClause clause = do
        symtab <- guse envLcl
        -- tag the clause
        name' <- clause ^. typeClauseName % to tagIdentP

        -- look up the arguments
        tell $ outOfScopesWith lookupTypeVar symtab (clause ^. typeClauseArgs % to (uncurry mappend))
        let args' = fromJust $ clause ^. typeClauseArgs 
                % to (traverseOf each (traverse (flip lookupTypeVar symtab)))
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
            , 
                ( zipWith tagIdentPWithSymEntry 
                    (clause ^. typeClauseArgs % _1) (args' ^. _1)
                , zipWith tagIdentPWithSymEntry
                        (clause ^. typeClauseArgs % _2) (args' ^. _2)
                )
            , tagIdentPWithSymEntry (clause ^. typeClauseStateVar) st'
            , phrases' 
            , ())
    
class RenameTypePhrase (t :: ObjectDefnTag) where
    renameTypePhrase :: 
        ( AsRenameErrors e
        , MonadState RenameEnv m
        , MonadWriter [e] m) => 
            -- State variable ident, unique tag
            (IdentP, UniqueTag) -> 
            MplTypePhrase MplParsed t -> 
            m (MplTypePhrase MplRenamed t)

instance RenameTypePhrase (SeqObjTag DataDefnTag) where
    renameTypePhrase st typephrase = do
        symtab <- guse envLcl
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let vfroms = typephrase ^. typePhraseFrom
            vto = typephrase ^. typePhraseTo
            vto' = _IdentR # (vto, snd st)

        vfroms' <- traverse ((`runReaderT`symtab) . renameScopedType) 
            vfroms

        -- tell $ bool [] [_ExpectedStateVarButGot # (fst st, vto)] (vto /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , vfroms'
            , vto'
            , ()
            )

instance RenameTypePhrase (SeqObjTag CodataDefnTag) where
    renameTypePhrase st typephrase = do
        symtab <- guse envLcl
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let (vfroms, vst) = typephrase ^. typePhraseFrom
            vto = typephrase ^. typePhraseTo
            vst' = _IdentR # (vst, snd st)

        vfroms' <- traverse ((`runReaderT`symtab) . renameScopedType) 
            vfroms
        vto' <- (`runReaderT`symtab) . renameScopedType $ vto

        -- tell $ bool [] [_ExpectedStateVarButGot # (fst st, vst)] (vst /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , (vfroms', vst')
            , vto'
            , ()
            )

instance RenameTypePhrase (ConcObjTag ProtocolDefnTag) where
    renameTypePhrase st typephrase = do
        symtab <- guse envLcl
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let vfrom = typephrase ^. typePhraseFrom
            vto = typephrase ^. typePhraseTo
            vto' = _IdentR # (vto, snd st)

        vfrom' <- (`runReaderT`symtab) . renameScopedType $ vfrom

        -- tell $ bool [] [_ExpectedStateVarButGot # (fst st, vto)] (vto /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , vfrom'
            , vto'
            , ()
            )

instance RenameTypePhrase (ConcObjTag CoprotocolDefnTag) where
    renameTypePhrase st typephrase = do
        symtab <- guse envLcl
        name' <- typephrase ^. typePhraseName % to tagIdentP 
        let vfrom = typephrase ^. typePhraseFrom
            vfrom' = _IdentR # (vfrom, snd st)
            vto = typephrase ^. typePhraseTo

        vto' <- (`runReaderT`symtab) . renameScopedType $ vto

        -- tell $ bool [] [_ExpectedStateVarButGot # (fst st, vfrom)] (vfrom /= fst st)

        return $ _MplTypePhrase # 
            ( name'
            , vfrom'
            , vto'
            , ()
            )

