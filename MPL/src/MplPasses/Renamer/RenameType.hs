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

renameScopedType :: 
    forall e m.
    ( AsRenameError e
    , MonadReader SymTab m
    , MonadWriter [e] m ) =>
    MplType MplParsed -> 
    m (MplType MplRenamed)
renameScopedType = cata f
  where
    f :: Base (MplType MplParsed) (m (MplType MplRenamed)) -> 
        m (MplType MplRenamed)
    f (TypeWithArgsF () ident args) = do
        args' <- sequenceA args
        ~lkup <- gviews equality $ lookupSym ident (_Just % _SymTypeInfo)

        tell $ maybe [ _OutOfScope # ident ] (const []) lkup

        return $ case fromJust lkup of
            SymEntry tag n 
                | has _SymTypeClause n -> 
                    _TypeWithArgs # ( (), _IdentR # (ident, tag), args' )
                | otherwise -> 
                    _TypeVar # ((), _IdentR # (ident, tag), args')

    f (TypeSeqF seq) =  do
        seq' <- renameSeqType seq
        return $ TypeSeq seq'
    f (TypeConcF conc) =  do
        conc' <- renameConcType conc
        return $ TypeConc conc' 

renameType :: 
    Rename (MplType MplParsed) (MplType MplRenamed)
renameType = cata f
  where
    f :: Base (MplType MplParsed) (_ (MplType MplRenamed)) -> 
        _ (MplType MplRenamed)
    f (TypeWithArgsF () ident args) = do
        args' <- sequenceA args
        ~lkup <- guses symTab $ lookupSym ident (_Just % _SymTypeInfo)

        case lkup of
            Just (SymEntry tag n) 
                | has _SymTypeClause n -> return $ 
                    _TypeWithArgs # ( (), _IdentR # (ident, tag), args' )
                | otherwise -> return $ 
                    _TypeVar # ((), _IdentR # (ident, tag), args')
            Nothing -> do
                ident' <- tagIdentP ident
                symTab %= ((collectSymTab ident' &
                    mapped % _2 % symEntryInfo 
                        .~ _Just % _SymTypeVar # () )<>)
                return $ _TypeVar # ((), ident', args' ) 

    f (TypeSeqF seq) =  do
        seq' <- renameSeqType seq
        return $ TypeSeq seq'
    f (TypeConcF conc) =  do
        conc' <- renameConcType conc
        return $ TypeConc conc'


renameSeqType :: 
    ( AsRenameError e
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
    ( AsRenameError e
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
