{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheck where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.TypeChecker.TypeCheckObj 
import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 
import MplPasses.TypeChecker.TypeCheckMplTypeSubUtil
import MplPasses.TypeChecker.TypeEqns
import MplPasses.Env

import MplUtil.UniqueSupply

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import Control.Arrow
import Data.Maybe
import Data.Bool


import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE


runTypeCheck ::
    TypeCheck (MplProg MplRenamed) (MplProg MplTypeChecked)
runTypeCheck (MplProg stmts) = 
    MplProg <$> traverse typeCheckStmts stmts

typeCheckStmts ::
    TypeCheck (MplStmt MplRenamed) (MplStmt MplTypeChecked)
typeCheckStmts (MplStmt defns wheres) = do
    wheres' <- traverse typeCheckStmts wheres
    (defns', eqns) <- fmap (NE.unzip . NE.fromList) 
            $ typeCheckDefns 
            $ NE.toList defns
    -- defns' <- fmap NE.fromList $ undefined $ NE.toList defns
    return $ MplStmt defns' wheres'

typeCheckDefns ::
    TypeCheck
        [MplDefn MplRenamed] 
        [ ( MplDefn MplTypeChecked
          , Maybe ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub]))
        ]
-- same as the rename step.. need to do the magic recursive do in order
-- to get the recursive declarations together properly.
typeCheckDefns (defn : defns) = do
    rec defn' <- envLcl % typeInfoSymTab .= symtab >> typeCheckDefn defn
        envGbl %= (collectSymTab (fst defn')<>)
        defns' <- typeCheckDefns defns
        symtab <- guse envGbl
    return $ defn' : defns'
typeCheckDefns [] = return []

typeCheckDefn ::
    TypeCheck (MplDefn MplRenamed) 
        ( MplDefn MplTypeChecked
        , Maybe ([TypeP MplTypeSub], [TypeP MplTypeSub], [TypeEqns MplTypeSub]))
typeCheckDefn (ObjectDefn obj) = undefined -- ObjectDefn <$> case obj of
    {-
    DataDefn n -> DataDefn <$> typeCheckClauseSpine n
    CodataDefn n -> CodataDefn <$> typeCheckTypeClauseSpine n
    ProtocolDefn n -> ProtocolDefn <$> typeCheckTypeClauseSpine n
    CoprotocolDefn n -> CoprotocolDefn <$> typeCheckTypeClauseSpine n
    -}
typeCheckDefn (FunctionDefn (MplFunction name funtype defn)) = do
    undefined
    {-
    funtype' <- flip (maybe (return Nothing)) funtype $ \(froms, to) -> do
        lcl <- guse envLcl
        (bds, froms') <- unzip <$> traverse typeCheckType froms
        (bd, to') <- typeCheckType to 
        envLcl .= lcl
        return $ Just (bd ++ fold bds, froms', to')
    
    defn' <- traverse typeCheckPattsExpr defn

    return $ FunctionDefn $ _MplFunction # (name', funtype', defn')
    -}

typeCheckDefn (ProcessDefn proc@(MplProcess name proctype defn)) = do
    mproctype <- sequenceA 
        -- $ listen . kindCheckProcessType <$> proctype
        $ listen . undefined <$> proctype

    sup <- freshUniqueSupply
    let symtp = flip evalState sup $ case mproctype of
            Just (tp, lg)
                -- | null lg -> Just <$> instantiateType tp
                | null lg -> undefined
                | otherwise -> return Nothing
            Nothing -> do
                tag <- freshTypeTag
                return $ Just 
                    ( []
                    , undefined -- _TypeVar # ((), _TypeIdentT # 
                        -- ( tag, _TypeVarPProc # proc )) 
                        )

    rec envLcl % typeInfoSymTab % at (name ^. uniqueTag) ?= 
            _SymRunInfo # (fmap snd symtp, proc') 
        (defn', eqns) <- typeCheckProcess defn
        let proc' = MplProcess name undefined defn'

    undefined
    {-
    undefined
    -- return $ ProcessDefn proc'
    -}

{-
( MplDefn MplTypeChecked
, Maybe ([TypeP MplTypeSub], [TypeP MplTypeSub], TypeEqns MplTypeSub))
-}

-------------------------
-- Type checking process...
-------------------------
typeCheckProcess ::
    TypeCheck
    (NonEmpty
        ( ([MplPattern MplRenamed], [ChIdentR], [ChIdentR])
        , NonEmpty (MplCmd MplRenamed)))
    (NonEmpty
        ( ([MplPattern MplTypeChecked], [ChIdentT], [ChIdentT])
        , NonEmpty (MplCmd MplTypeChecked))
        , Maybe [TypeEqns MplTypeSub])
typeCheckProcess bdy = do
    (ttypephrases, (bdy', eqns)) <- second NE.unzip . NE.unzip <$> 
        traverse (withFreshTypeTag . typeCheckProcessBody) bdy
    undefined

typeCheckProcessBody ::
    TypeCheck
    ( ([MplPattern MplRenamed], [ChIdentR], [ChIdentR])
        , NonEmpty (MplCmd MplRenamed) )
    ( (([MplPattern MplTypeChecked], [ChIdentT], [ChIdentT])
        , NonEmpty (MplCmd MplTypeChecked))
    , Maybe [TypeEqns MplTypeSub])
typeCheckProcessBody = undefined

