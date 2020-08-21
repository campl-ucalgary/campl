{-# LANGUAGE QuasiQuotes #-}
module MplCompile where

import Text.RawString.QQ

import MplPasses.Parser.Parse
import MplPasses.Parser.BnfcParse

parsebnfc n = runParse' <$> runBnfc n

test = [r|

data
    MyData(A,B) -> C =
        MyData :: A,B -> C
fun appwrapper =
    a -> case a of
        MyData(a,b) -> a
        MyData(_,_) -> a
|]

