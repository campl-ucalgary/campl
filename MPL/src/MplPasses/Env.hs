{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module MplPasses.Env where

import Optics

import Control.Monad.State 

import MplUtil.UniqueSupply


data TopLevel = TopLevel
  deriving Show

data Env gbl lcl = Env {
    -- | top level information that never changes about
    -- the module in question.
    _envTop :: TopLevel
    -- | Unique supply of names..
    , _envUniqueSupply :: UniqueSupply
    -- | Global information about the things being compiled
    , _envGbl :: gbl
    -- | Nested stuff changes as we go into an expression
    , _envLcl :: lcl

}  deriving Show



$(makePrisms ''Env)
$(makeLenses ''Env)


instance HasUniqueSupply (Env a b) where
    uniqueSupply = envUniqueSupply 

-- | locall runs an action in the environemnt
-- while managing the unique supply foryou..
localEnvSt :: 
    ( s ~ Env gbl lcl
    , MonadState s m ) => 
    (s -> s) ->
    StateT s m a -> 
    m a
localEnvSt f act = do
    sup <- freshUniqueSupply
    st <- guse equality
    flip evalStateT 
        ( st & uniqueSupply .~ sup
             & equality %~ f
        ) $ act
