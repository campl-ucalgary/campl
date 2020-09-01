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

data TypeChAnn =
    TypeChAnnNameOcc NameOcc
    | TypeChAnnCmd (MplCmd MplRenamed)
    -- Change this in the future! We have a 
    -- few problems... When we go from 
    -- MplType MplTypeSub --> MplType MplTypeChecked
    -- we lose the annotation information if it
    -- is an inferred type from a variable....
    -- So this acts as the ``Nothing" in the Maybe
    | TypeChAnnEmpty
  deriving Show



data TypeAnn = 
    -- Function
    TypeAnnFun (MplFunction MplRenamed)
    -- Process
    | TypeAnnProc (MplProcess MplRenamed)

    | TypeAnnProcPhrase 
        (([MplPattern MplRenamed], [ChIdentR], [ChIdentR]), NonEmpty (MplCmd MplRenamed) )
    | TypeAnnFunPhrase 
        ([MplPattern MplRenamed], MplExpr MplRenamed)

    -- | Note that TypeAnnFunCall / TypeAnnProcCall may or may not exist
    -- if they did not compile properly...
    | TypeAnnFunCall (MplFunction MplTypeChecked)
    -- Process
    | TypeAnnProcCall (MplProcess MplTypeChecked)

    -- Command
    | TypeAnnCmd (MplCmd MplRenamed)

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
$(makePrisms ''TypeChAnn)
$(makeClassyPrisms ''TypeAnn)


freshTypeTag ::
    ( MonadState s m
    , HasUniqueSupply s ) => 
    m TypeTag
freshTypeTag = TypeTag . UniqueTag . uniqueFromSupply <$> freshUniqueSupply


