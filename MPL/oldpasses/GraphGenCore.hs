{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module MPLPasses.GraphGenCore where

import Optics
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLAST.MPLProgGraph
import MPLPasses.TieDefnsErrors 

import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Bifunctor as Bifunctor
import Control.Arrow

import Data.Maybe
import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import MPLUtil.UniqueSupply

import Data.IORef

newtype GraphGenCore a = GraphGenCore { 
    unGraphGenCore :: RWS GraphGenCoreEnv [TieDefnsError] GraphGenCoreState a
}  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState GraphGenCoreState
    , MonadReader GraphGenCoreEnv
    , MonadWriter [TieDefnsError]
    , MonadRWS GraphGenCoreEnv [TieDefnsError] GraphGenCoreState  )

data GraphGenCoreEnv = GraphGenCoreEnv {
    _graphGenCoreEnvBuiltInTypesUniqueTags :: BuiltInTypesUniqueTags
}

data BuiltInTypesUniqueTags = BuiltInTypesUniqueTags {
    _concGetTag :: UniqueTag
    , _concPutTag :: UniqueTag
    , _concTensorTag :: UniqueTag
    , _concParTag :: UniqueTag
    , _concNegTag :: UniqueTag
    , _concTopBotTag :: UniqueTag
}

data GraphGenCoreState = GraphGenCoreState {
    _graphGenCoreStateUniqueSupply :: UniqueSupply
} 

splitGraphGenCore :: 
    GraphGenCore a -> 
    GraphGenCore (a, [TieDefnsError])
splitGraphGenCore m = do
    env <- ask 
    sup <- freshUniqueSupply
    let ~(a, st, w) = runRWS (unGraphGenCore m) env (GraphGenCoreState sup)
    return (a, w)


defaultGraphGenCore :: IO (GraphGenCoreEnv, GraphGenCoreState)
defaultGraphGenCore = do
    ref <- newIORef (0 :: Word)
    supply <- initUniqueSupply ref

    let (builtin, supply') = (BuiltInTypesUniqueTags 
            <$> freshUniqueTag
            <*> freshUniqueTag
            <*> freshUniqueTag
            <*> freshUniqueTag
            <*> freshUniqueTag
            <*> freshUniqueTag) `runState` supply 

    return $ (GraphGenCoreEnv builtin, GraphGenCoreState supply')

$(concat <$> traverse makeClassy 
    [ ''GraphGenCoreState
    , ''GraphGenCoreEnv 
    , ''BuiltInTypesUniqueTags]
 )
$(concat <$> traverse makePrisms
    [ ''GraphGenCoreEnv 
    , ''GraphGenCoreState ]
 )

instance HasUniqueSupply GraphGenCoreState where
    uniqueSupply = graphGenCoreStateUniqueSupply

instance HasBuiltInTypesUniqueTags GraphGenCoreEnv where
    builtInTypesUniqueTags = graphGenCoreEnvBuiltInTypesUniqueTags 

tagConcTypeF ::
    ConcTypeF BnfcIdent t ->
    GraphGenCore (ConcTypeF TaggedBnfcIdent t)
tagConcTypeF n = 
    case n of
        TypeGetF ident a b -> do 
            tag <- gview concGetTag
            return $ TypeGetF (_TaggedBnfcIdent # (ident, tag)) a b
        TypePutF ident a b -> do 
            tag <- gview concPutTag
            return $ TypePutF (_TaggedBnfcIdent # (ident, tag)) a b
        TypeTensorF ident a b -> do 
            tag <- gview concTensorTag
            return $ TypeTensorF (_TaggedBnfcIdent # (ident, tag)) a b
        TypeParF ident a b -> do 
            tag <- gview concParTag
            return $ TypeParF (_TaggedBnfcIdent # (ident, tag)) a b
        TypeTopBotF ident -> do 
            tag <- gview concTopBotTag
            return $ TypeTopBotF (_TaggedBnfcIdent # (ident, tag)) 
        TypeNegF ident t -> do 
            tag <- gview concNegTag
            return $ TypeNegF (_TaggedBnfcIdent # (ident, tag)) t
        TypeConcArrF seq ins outs ->
            return $ TypeConcArrF seq ins outs

tagSeqTypeF ::
    SeqTypeF BnfcIdent t -> 
    GraphGenCore (SeqTypeF TaggedBnfcIdent t)
tagSeqTypeF n = case n of
    TypeIntF ident -> undefined
    TypeCharF ident  -> undefined
    TypeDoubleF ident -> undefined
    
