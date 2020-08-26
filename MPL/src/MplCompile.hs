{-# LANGUAGE QuasiQuotes #-}
module MplCompile where

import Text.RawString.QQ

import MplPasses.Parser.Parse
import MplPasses.Parser.BnfcParse

parsebnfc n = runParse' <$> runBnfc n

test = [r|

fun myfun =
    a ->( 
        let fun c =
                d -> d
        in c(a))

|]

-- runPassesTester
