module MplPasses.TypeChecker.TypeCheckMplTypeSubUtil where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

import Control.Monad.State
import MplUtil.UniqueSupply

newtype TypeTag = TypeTag UniqueTag
  deriving (Show, Eq, Ord)

data TypeIdentT = TypeIdentT {
    _typeIdentTUniqueTag :: TypeTag
    , _typeIdentTInfo :: TypeIdentTInfo
}

data TypeAnn = 
    -- Function
    TypeAnnFun (MplFunction MplRenamed)
    -- Process
    | TypeAnnProc (MplProcess MplRenamed)
    -- Expression
    | TypeAnnExpr (MplExpr MplRenamed)
    -- Type pattern..
    | TypeAnnPatt (MplPattern MplRenamed)
    -- Type channel..
    | TypeAnnCh ChIdentR

data TypeIdentTInfo = 
    -- type variable..
    TypeIdentTInfoTypeVar (TypeP MplRenamed)
    | TypeIdentTInfoTypeAnn TypeAnn

$(makeLenses ''TypeIdentT)
$(makePrisms ''TypeIdentT)
$(makePrisms ''TypeIdentTInfo)
$(makeClassyPrisms ''TypeAnn)

instance AsTypeAnn TypeIdentTInfo where
    _TypeAnn = _TypeIdentTInfoTypeAnn 

class ToTypeIdentT t where
    toTypeIdentT :: ( MonadState s m, HasUniqueSupply s) => 
        t -> m TypeIdentT

-- | Note: The IdentR is for type variables!
-- only!
instance ToTypeIdentT IdentR where
    toTypeIdentT identr = do
        tag <- freshTypeTag
        return $ _TypeIdentT # (tag, _TypeIdentTInfoTypeVar # identr) 

freshTypeTag ::
    ( MonadState s m
    , HasUniqueSupply s ) => 
    m TypeTag
freshTypeTag = 
    undefined
    -- TypeTag . UniqueTag . uniqueFromSupply <$> freshUniqueSupply
