{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheckMplTypeSubUtil where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

import Control.Monad.State
import MplUtil.UniqueSupply

import Data.List.NonEmpty

{- Module for defining utilities of types which are / may be substituted
 -
 -}


newtype TypeTag = TypeTag UniqueTag
  deriving (Show, Eq, Ord)

data TypeIdentT = TypeIdentT {
    _typeIdentTUniqueTag :: TypeTag
    , _typeIdentTInfo :: TypeIdentTInfo
}  deriving Show

instance Eq TypeIdentT where
    a == b = _typeIdentTUniqueTag a == _typeIdentTUniqueTag b

instance Ord TypeIdentT where
    a <= b = _typeIdentTUniqueTag a <= _typeIdentTUniqueTag b

data TypeChAnn 
    = TypeChAnnNameOcc NameOcc
    | TypeChAnnCmd (MplCmd MplRenamed)
  deriving Show



data TypeAnn 
    -- Function
    = TypeAnnFun (MplFunction MplRenamed)
    -- Process
    | TypeAnnProc (MplProcess MplRenamed)

    | TypeAnnProcPhrase 
        (([MplPattern MplRenamed], [ChIdentR], [ChIdentR]), NonEmpty (MplCmd MplRenamed) )
    | TypeAnnFunPhrase 
        ([MplPattern MplRenamed], MplExpr MplRenamed)

    -- Command
    | TypeAnnCmd (MplCmd MplRenamed)

    -- Expression
    | TypeAnnExpr (MplExpr MplRenamed)
    -- Type pattern..
    | TypeAnnPatt (MplPattern MplRenamed)

    -- Type channel..
    | TypeAnnCh ChIdentR

  deriving Show

data TypeIdentTInfo 
    -- type variable..
    = TypeIdentTInfoTypeVar (TypeP MplTypeChecked)
    | TypeIdentTInfoTypeAnn TypeAnn
  deriving Show

$(makeLenses ''TypeIdentT)
$(makePrisms ''TypeIdentT)
$(makePrisms ''TypeIdentTInfo)
$(makePrisms ''TypeChAnn)
$(makeClassyPrisms ''TypeAnn)

{- typeIdentTToTypeT. Converts a 'TypeIdentT' to 'TypeP MplTypeChecked' 
(recall this is just 'TypeT'). 
Note that we always use the outer most type tag since that is the one used in the 
unification algorithm.
 -}
typeIdentTToTypeT :: TypeIdentT -> TypeP MplTypeChecked
typeIdentTToTypeT (TypeIdentT tag (TypeIdentTInfoTypeVar tp)) 
    = case tp of
        GenNamedType _ -> GenNamedType tag'
        NamedType tp' -> NamedType 
            (tp' & identRUniqueTag .~ tag')
  where
    tag' = tag ^. coerced
typeIdentTToTypeT (TypeIdentT (TypeTag tag) _) = GenNamedType tag

typeTToTypeIdentT :: 
    TypeP MplTypeChecked ->
    TypeIdentT 
typeTToTypeIdentT = go 
  where
    go tp 
        = TypeIdentT (tp ^. uniqueTag % to TypeTag) $ TypeIdentTInfoTypeVar tp



freshTypeTag ::
    ( MonadState s m
    , HasUniqueSupply s ) => 
    m TypeTag
freshTypeTag = TypeTag . UniqueTag . uniqueFromSupply <$> freshUniqueSupply


