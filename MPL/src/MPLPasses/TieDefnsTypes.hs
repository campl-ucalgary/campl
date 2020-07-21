{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.TieDefnsTypes where

import Optics
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLPasses.SymbolTable

import Control.Monad.RWS
import Control.Monad.State

data TieDefnsEnv = TieDefnsEnv {
    }


data TieDefnsState = TieDefnsState {
        _tieDefnsUniqueTagGen :: UniqueTag
        , _tieDefnsSymbolTable :: SymbolTable
    }
  deriving Show 

$(concat <$> traverse makeClassy 
    [ ''TieDefnsEnv
    , ''TieDefnsState
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''TieDefnsEnv
    , ''TieDefnsState
    ]
 )

instance HasUniqueTag TieDefnsState where
    uniqueTag = tieDefnsUniqueTagGen

defaultTieDefnsState :: TieDefnsState
defaultTieDefnsState = _TieDefnsState # (UniqueTag 0, [])
