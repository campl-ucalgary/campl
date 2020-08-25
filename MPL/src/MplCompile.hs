{-# LANGUAGE QuasiQuotes #-}
module MplCompile where

import Text.RawString.QQ

import MplPasses.Parser.Parse
import MplPasses.Parser.BnfcParse

parsebnfc n = runParse' <$> runBnfc n

test = [r|
data 
    Test(A,B) -> C =
        -- Testt :: A,B -> C
        Testtt :: A(B) -> C
|]
