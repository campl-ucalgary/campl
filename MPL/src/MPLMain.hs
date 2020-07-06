module MPLMain where

import Language.AbsMPL
import Language.LayoutMPL
import Language.ParMPL
import Language.ErrM


mplMain :: IO ()
mplMain = undefined

parseAndLex :: String -> Err MplProg
parseAndLex = pMplProg . resolveLayout True . myLexer

