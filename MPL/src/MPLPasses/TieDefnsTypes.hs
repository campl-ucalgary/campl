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
    _tieDefnsSymbolTable :: SymbolTable
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

defaultTieDefnsState :: TieDefnsState
defaultTieDefnsState = _TieDefnsState # []
