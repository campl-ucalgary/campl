{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.ToGraphTypes where

import Optics
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLPasses.SymbolTable

import Control.Monad.RWS
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
defaultToGraphState = _ToGraphState # (mempty, (UniqueTag 0))
