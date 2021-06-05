{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module MplAsmAST.MplAsmProg where

import Optics

import MplAsmAST.MplAsmCommand

-- |  protocol / coprotocol info.
data TypeAndConcSpecs x = TypeAndConcSpecs 
    { _typeAndConcSpecsType :: IdP x 
    , _typeAndConcSpecsSpecs :: [IdP x]
    }
-- | constructor / destructor info. We hve number of args.
data TypeAndSeqSpecs x = TypeAndSeqSpecs 
    { _typeAndSeqSpecsType :: IdP x 
    , _typeAndSeqSpecsSpecs ::  [(IdP x, Word)]
    }

data MplAsmProg x = MplAsmProg 
    { _mplAsmStmts :: [MplAsmStmt x]
    , _mplAsmMain :: Maybe (IdP x, ([IdP x], [IdP x], [IdP x]), MplAsmComs x)
    }

data MplAsmStmt x
    = Protocols [TypeAndConcSpecs x]
    | Coprotocols [TypeAndConcSpecs x]
    | Constructors [TypeAndSeqSpecs x]
    | Destructors [TypeAndSeqSpecs x]
    -- | Function name, arguments, commands
    | Functions [(IdP x, [IdP x], MplAsmComs x)]
    -- | process name, arguments, commands
    | Processes [(IdP x, ([IdP x], [IdP x], [IdP x]), MplAsmComs x)] 


newtype Name = Name { _nameStr :: String }
  deriving (Show, Eq, Ord)

$(makeClassy ''Name)
$(makeLenses ''TypeAndConcSpecs)
$(makeLenses ''TypeAndSeqSpecs)
$(makeLenses ''MplAsmProg)
$(makePrisms ''MplAsmProg)

