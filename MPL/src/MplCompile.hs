{-# LANGUAGE QuasiQuotes #-}
module MplCompile where

import Text.RawString.QQ

import MplPasses.Parser.Parse
import MplPasses.Parser.BnfcParse


{- Module for compiling (i.e., running all the passes) an Mpl program 
 - Currently, lots of work to be done with this! For now, it is the home
 - of testing functions..
 -}

parsebnfc n = runParse' <$> runBnfc n

