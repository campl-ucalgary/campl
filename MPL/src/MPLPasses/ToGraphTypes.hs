{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.ToGraphTypes where

import Optics
import Optics.State.Operators

import MPLPasses.SymbolTable
import MPLAST.MPLASTCore
import Control.Monad.State

data ToGraphEnv = ToGraphEnv {
    }

data ToGraphState = ToGraphState {
        _toGraphSymbolTable :: SymbolTable
        , _toGraphUniqueTagGen :: UniqueTag
    }

$(concat <$> traverse makeClassy 
    [ ''ToGraphEnv
    , ''ToGraphState
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''ToGraphEnv
    , ''ToGraphState
    ]
 )

instance HasUniqueTag ToGraphState where
    uniqueTag = toGraphUniqueTagGen

defaultToGraphState :: ToGraphState
defaultToGraphState = _ToGraphState # ([], (UniqueTag 0))

{-

defaultGraphEnv :: ToGraphEnv
defaultGraphEnv = review _ToGraphEnv ()

defaultToGraphState :: ToGraphState
defaultToGraphState = review _ToGraphState ([], (UniqueTag 0))

freshUniqueTag ::
    ( MonadState c m
    , HasToGraphState c ) => 
    m UniqueTag
freshUniqueTag = 
    toGraphUniqueTagGen <<%= succ

addToTopScope ::
    ( MonadState c m
    , HasToGraphState c ) => 
    Scope ->
    m ()
addToTopScope scoped = do
    toGraphSymbolTable % _Cons % _1 %= (scoped++)

withNewScope ::
    ( MonadState c m
    , HasToGraphState c ) => 
    m a -> m a
withNewScope m = do
    newScope
    m' <- m
    popScope
    return m'

newScope ::
    ( MonadState c m
    , HasToGraphState c ) => 
    m ()
newScope = toGraphSymbolTable %= ([]:)

popScope ::
    ( MonadState c m
    , HasToGraphState c ) => 
    m ()
popScope = toGraphSymbolTable %= tail


-- freshUniqueTag ::
--     ( MonadState c m
--     , HasToGraphState c ) => 
--     m UniqueTag
-- freshUniqueTag = 
-}
