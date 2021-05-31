{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyCase #-}
module Main where

import Lib

import Text.RawString.QQ

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

import MplParse.MplParse
import MplParse.Stack
import MplParse.Util

main :: IO ()
main = someFunc

testDataParse :: String -> Either P.ParseError ()
testDataParse inp = testRunParser (pJunk *> pMplDataClauseSpine <* P.eof) inp

testing = [r|
data 
    Clause -> S = 
        Testing :: S (+) UppercaseIdentifier-> S
|]
