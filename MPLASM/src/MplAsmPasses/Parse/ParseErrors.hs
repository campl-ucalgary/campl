module MplAsmPasses.Parse.ParseErrors where

import Optics

data ParseError 
    = BnfcParseError String

$(makeClassyPrisms ''ParseError)
