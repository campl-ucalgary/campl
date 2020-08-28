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

newtype TypeTag = TypeTag UniqueTag
  deriving (Show, Eq, Ord)

data TypeIdentT = TypeIdentT {
    _typeIdentTUniqueTag :: TypeTag
    , _typeIdentTInfo :: TypeIdentTInfo
}  deriving Show

instance Eq TypeIdentT where
    a ==  b = _typeIdentTUniqueTag a  == _typeIdentTUniqueTag b

instance Ord TypeIdentT where
    a <=  b = _typeIdentTUniqueTag a  <= _typeIdentTUniqueTag b


data TypeAnn = 
    -- Function
    TypeAnnFun (MplFunction MplRenamed)
    -- Process
    | TypeAnnProc (MplProcess MplRenamed)

    | TypeAnnProcPhrase 
        (([MplPattern MplRenamed], [ChIdentR], [ChIdentR]), NonEmpty (MplCmd MplRenamed) )

    | TypeAnnFunPhrase 
        ([MplPattern MplRenamed], MplExpr MplRenamed)

    -- Expression
    | TypeAnnExpr (MplExpr MplRenamed)
    -- Type pattern..
    | TypeAnnPatt (MplPattern MplRenamed)
    -- Type channel..
    | TypeAnnCh ChIdentR
  deriving Show

data TypeIdentTInfo = 
    -- type variable..
    TypeIdentTInfoTypeVar (TypeP MplTypeChecked)
    | TypeIdentTInfoTypeAnn TypeAnn
  deriving Show

$(makeLenses ''TypeIdentT)
$(makePrisms ''TypeIdentT)
$(makePrisms ''TypeIdentTInfo)
$(makeClassyPrisms ''TypeAnn)

instance AsTypeAnn TypeIdentTInfo where

freshTypeTag ::
    ( MonadState s m
    , HasUniqueSupply s ) => 
    m TypeTag
freshTypeTag = TypeTag . UniqueTag . uniqueFromSupply <$> freshUniqueSupply


