{-# LANGUAGE QuasiQuotes #-}
module MplCompile where

import Text.RawString.QQ

import MplPasses.Parser.Parse
import MplPasses.Parser.BnfcParse
parsebnfc n = runParse' <$> runBnfc n

test = [r|

fun test :: Huh(Int| Int) -> Int =
    _ -> 3 
|]


