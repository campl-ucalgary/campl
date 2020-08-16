{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Renamer.RenameType where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.Renamer.RenameUtils
import MplPasses.Renamer.RenameSym
import MplPasses.Renamer.RenameErrors
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

import Data.Foldable
import Data.Functor.Foldable 

import Data.Void

-- TODO:
-- We can generalize this all nicely with the continuation monad (ContT)
-- since querying the data is all the same... Although, we would have to define
-- another AST which is decorated with the symbol table look ups.

renameScopedType :: 
    forall e m.
    ( AsRenameErrors e
    , MonadReader SymTab m
    , MonadWriter [e] m ) =>
    MplType MplParsed -> 
    m (MplType MplRenamed)
renameScopedType = cata f
  where
    f :: Base (MplType MplParsed) (m (MplType MplRenamed)) -> 
        m (MplType MplRenamed)
    f (TypeSeqVarWithArgsF () ident args) = do
        args' <- sequenceA args
        ~lkup <- gviews equality $ lookupSym ident (_Just % _SymTypeInfo)

        tell $ maybe [ _OutOfScope # ident ] (const []) lkup

        return $ case fromJust lkup of
            SymEntry tag n 
                | has _SymTypeClause n -> 
                    _TypeSeqWithArgs # ( (), _IdentR # (ident, tag), args' )
                | otherwise -> 
                    _TypeSeqVarWithArgs # ((), _IdentR # (ident, tag), args')

    -- more or less duplicated code from above
    f (TypeConcVarWithArgsF () ident (seqs, concs)) = do
        seqs' <- sequenceA seqs
        concs' <- sequenceA concs
        ~lkup <- gviews equality $ lookupSym ident (_Just % _SymTypeInfo)

        tell $ maybe [ _OutOfScope # ident ] (const []) lkup

        return $ case fromJust lkup of
            SymEntry tag n 
                | has _SymTypeClause n -> 
                    _TypeConcWithArgs # ( (), _IdentR # (ident, tag), (seqs',concs') )
                | otherwise -> 
                    _TypeConcVarWithArgs # ((), _IdentR # (ident, tag), (seqs',concs'))

    f (TypeVarF () ident) = do
        ~lkup <- gviews equality $ lookupSym ident (_Just % _SymTypeInfo)
        tell $ maybe [ _OutOfScope # ident ] (const []) lkup
        return $ case fromJust lkup of
            SymEntry tag (SymTypeClause objtag)
                | has _SeqObjTag objtag  -> 
                    _TypeSeqWithArgs # ( (), _IdentR # (ident, tag), mempty )
                | otherwise  -> 
                    _TypeConcWithArgs # ( (), _IdentR # (ident, tag), mempty )
            SymEntry tag SymTypeVar -> _TypeVar # ((), _IdentR # (ident, tag))

renameType :: 
    Rename 
        (MplType MplParsed) 
        ([IdentR], MplType MplRenamed)
        -- explictly declared type variables, renamed type
renameType = cata f
  where
    f :: Base (MplType MplParsed) (_ ([IdentR], MplType MplRenamed)) -> 
        _ ([IdentR], MplType MplRenamed)
    f (TypeSeqVarWithArgsF () ident args) = do
        args' <- sequenceA args
        ~lkup <- guses envLcl $ lookupSym ident (_Just % _SymTypeInfo)

        case lkup of
            Just (SymEntry tag n) 
                | has _SymTypeClause n -> return 
                    ( concatMap fst args' 
                    , _TypeSeqWithArgs # ( (), _IdentR # (ident, tag), fmap snd args' )
                    )
                | otherwise -> return
                    ( concatMap fst args'
                    , _TypeSeqVarWithArgs # ((), _IdentR # (ident, tag), fmap snd args')
                    )
            _ -> do
                ident' <- tagIdentP ident
                envLcl %= ((collectSymTab ident' & mapped % _2 % symEntryInfo 
                        .~ _Just % _SymTypeVar # () )<>)
                return 
                    ( ident' : concatMap fst args'
                    , _TypeSeqVarWithArgs # ((), ident', fmap snd args')
                    )

    -- more or less duplciated code..
    f (TypeConcVarWithArgsF () ident args) = do
        (seqs', concs') <- sequenceOf (each % traversed) args
        ~lkup <- guses envLcl $ lookupSym ident (_Just % _SymTypeInfo)

        case lkup of
            Just (SymEntry tag n) 
                | has _SymTypeClause n -> return 
                    ( concatMap fst seqs' <> concatMap fst concs'
                    , _TypeConcWithArgs # ( (), _IdentR # (ident, tag), (fmap snd seqs', fmap snd concs'))
                    )
                | otherwise -> return
                    ( concatMap fst seqs' <> concatMap fst concs'
                    , _TypeConcVarWithArgs # ( (), _IdentR # (ident, tag), (fmap snd seqs', fmap snd concs'))
                    )
            _ -> do
                ident' <- tagIdentP ident
                envLcl %= ((collectSymTab ident' & mapped % _2 % symEntryInfo 
                        .~ _Just % _SymTypeVar # () )<>)
                return 
                    ( ident' : concatMap fst seqs' <> concatMap fst concs'
                    , _TypeConcVarWithArgs # ( (), ident', (fmap snd seqs', fmap snd concs'))
                    )
    f (TypeVarF () ident) = do
        ~lkup <- guses envLcl $ lookupSym ident (_Just % _SymTypeInfo)
        case lkup of 
            Just (SymEntry tag n)
                | has _SymTypeClause n -> return 
                    ( mempty
                    , _TypeVar # ( (), _IdentR # (ident, tag))
                    )
                | otherwise -> do
                    ident' <- tagIdentP ident
                    return 
                        ( pure ident'
                        , _TypeVar # ( (), ident' )
                        )


{-
renameSeqType :: 
    ( AsRenameErrors e
    , MonadWriter [e] m ) =>
    MplSeqTypesF MplParsed (m (MplType MplRenamed)) ->
    m (MplSeqTypesF MplRenamed (MplType MplRenamed))
renameSeqType = f 
  where
    f (TypeIntF n) = pure $ TypeIntF n
    f (TypeCharF n) = pure $ TypeCharF n
    f (TypeDoubleF n) = pure $ TypeDoubleF n

    {-
    f (TypeStringF _) = error "TODO"
    f (TypeBoolF _) = error "TODO"
    f (TypeUnitF _) = error "TODO"
    f (TypeListF _ _) = error "TODO"
    f (TypeTupleF _ _) = error "TODO"
    -}

renameConcType :: 
    ( AsRenameErrors e
    , MonadWriter [e] m ) =>
    MplConcTypesF MplParsed (m (MplType MplRenamed)) ->
    m (MplConcTypesF MplRenamed (MplType MplRenamed))
renameConcType = f
  where
    f (TypeGetF l s t) = TypeGetF l <$> s <*> t
    f (TypePutF l s t) = TypePutF l <$> s <*> t
    f (TypeTensorF l s t) = TypeTensorF l <$> s <*> t
    f (TypeParF l s t) = TypeParF l <$> s <*> t 
    f (TypeNegF l s) = TypeNegF l <$> s
    f (TypeTopBotF l) = pure $ TypeTopBotF l
    -}
