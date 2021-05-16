{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.Renamer.RenamePatt where

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

import Data.Foldable
import Data.Functor.Foldable (cata, Base)

renamePattern :: 
    Rename (MplPattern MplParsed)
        (MplPattern MplRenamed)
renamePattern = cata f
  where
    f :: Base (MplPattern MplParsed) (_ (MplPattern MplRenamed)) ->
        (_ (MplPattern MplRenamed))
    f (PConstructorF () ident args) = do
        symtab <- guse envLcl
        args' <- sequenceA args
        let ident' = fromJust $ 
                lookupSymSeqPhrase ident symtab
        tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 

        return $ _PConstructor # 
            ( ()
            , _IdentR # (ident, ident' ^. uniqueTag)
            , args' )
    f (PRecordF loc phrases) = do
        phrases' <- traverse g phrases

        return $ _PRecord # ( loc, phrases' )
      where
        g ((), ident, patt) = do
            symtab <- guse envLcl
            patt' <- patt
            let ident' = fromJust $ lookupSymSeqPhrase ident symtab
            tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 
            return ((), _IdentR # (ident, ident' ^. uniqueTag), patt')
    -- extends the variable context
    f (PVarF () ident) = do
        ident' <- tagIdentP ident
        envLcl %= ((collectSymTab ident')<>)
        return $ _PVar # ((), ident')
     
    f (PNullF loc) = do
        return $ _PNull # loc

    f (PTupleF loc (t0,t1,ts)) = do
        ~(t0':t1':ts') <- sequenceA $ t0:t1:ts
        return $ PTuple loc (t0',t1',ts')

    f (PCharF loc c) = return $ PChar loc c

    f (PIntF loc c) = return $ PInt loc c

    {- Here, we replace some of the code with user provided types data.
     - Honestly, this is a bit of a cheap hack!  -}
    f (PUnitF loc) = do
        symtab <- guse envLcl
        let ident = _IdentP # 
                ( _NameOcc # 
                    ( "Unit" ^. coerced
                    , loc
                    )
                , _TermLevel # ()
                )
            ident' = fromJust $ lookupSymSeqPhrase ident symtab
        tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 

        return $ _PConstructor # 
            ( ()
            , _IdentR # (ident, ident' ^. uniqueTag)
            , []
            )
    f (PListF loc []) = do
        symtab <- guse envLcl
        let ident = _IdentP # 
                ( _NameOcc # 
                    ( "Nil" ^. coerced
                    , loc
                    )
                , _TermLevel # ()
                )
            ident' = fromJust $ lookupSymSeqPhrase ident symtab
        tell $ outOfScopeWith lookupSymSeqPhrase symtab ident 

        return $ _PConstructor # 
            ( ()
            , _IdentR # (ident, ident' ^. uniqueTag)
            , [] )

    f (PListF loc args) = plist loc args
    f (PStringF loc args) = plist loc $ map (pure . PChar loc) args
    f (PListConsF loc a as) = do
        symtab <- guse envLcl
        a' <- a
        as' <- as

        let identcons = _IdentP # 
                ( _NameOcc # 
                    ( "Cons" ^. coerced
                    , loc
                    )
                , _TermLevel # ()
                )
            identcons' = fromJust $ lookupSymSeqPhrase identcons symtab

        tell $ outOfScopeWith lookupSymSeqPhrase symtab identcons
        return $
            _PConstructor # 
            ( ()
            , _IdentR # (identcons, identcons' ^. uniqueTag)
            , [a',as'] )
        

    plist :: 
        Location -> 
        [_ (MplPattern MplRenamed)] -> 
        _ (MplPattern MplRenamed)
    plist loc args = do
        args' <- sequenceA args 

        symtab <- guse envLcl
        let identcons = _IdentP # 
                ( _NameOcc # 
                    ( "Cons" ^. coerced
                    , loc
                    )
                , _TermLevel # ()
                )
            identcons' = fromJust $ lookupSymSeqPhrase identcons symtab

        let identnil = _IdentP # 
                ( _NameOcc # 
                    ( "Nil" ^. coerced
                    , loc
                    )
                , _TermLevel # ()
                )
            identnil' = fromJust $ lookupSymSeqPhrase identnil symtab

        tell $ outOfScopeWith lookupSymSeqPhrase symtab identcons
        tell $ outOfScopeWith lookupSymSeqPhrase symtab identnil 

        let rcons a acc = 
                _PConstructor # 
                ( ()
                , _IdentR # (identcons, identcons' ^. uniqueTag)
                , [a,acc] )
        let rnil = _PConstructor # 
                    ( ()
                    , _IdentR # (identnil, identnil' ^. uniqueTag)
                    , [] )

        return $ foldr rcons rnil args'
