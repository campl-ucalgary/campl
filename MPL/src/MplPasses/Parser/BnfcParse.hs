module MplPasses.Parser.BnfcParse (
    module Language.AbsMPL
    , module Language.ErrM
    , module Language.LayoutMPL
    , module Language.LexMPL
    , module Language.ParMPL
    , module Language.PrintMPL
    , module Language.SkelMPL
    , runBnfc
    ) where

import Language.AbsMPL
import Language.ErrM
import Language.LayoutMPL
import Language.LexMPL
import Language.ParMPL
import Language.PrintMPL
import Language.SkelMPL

runBnfc :: String -> Err MplProg
runBnfc = pMplProg . resolveLayout True . myLexer

