{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module MplParse.Assertions where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit

import MplParse.MplParse
import MplParse.Stack

import qualified Text.Parsec as P

import Data.Either


{-| 
 This tests if a parser /SHOULD/ parse something. Since we often are testing
 if an individual parser combinator works or not, we put the
 pJunk *> your parser <* P.eof for you so it is easier to write test cases (this just removes the junk before and after the parser and ensures we attempt to parse the entire input).
-}
shouldParse :: (HasCallStack, Show a) => MplParser a -> String -> Expectation
shouldParse pa inp = runParser (pJunk *> pa <* P.eof) 
    "<test src file>" inp `shouldSatisfy` isRight

shouldNotParse :: (HasCallStack, Show a) => MplParser a -> String -> Expectation
shouldNotParse pa inp = runParser (pJunk *> pa <* P.eof) 
    "<test src file>" inp `shouldSatisfy` isLeft

