{-# LANGUAGE TemplateHaskell #-}
module MplPasses.Env where

import Optics

import MplUtil.UniqueSupply

data TopLevel = TopLevel

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

}

$(makePrisms ''Env)
$(makeLenses ''Env)


instance HasUniqueSupply (Env a b) where
    uniqueSupply = envUniqueSupply 

