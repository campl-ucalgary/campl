{-# LANGUAGE ScopedTypeVariables #-}
module Data.StreamSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Stream (Stream (..))
import qualified Data.Stream as Stream


import Control.Monad
import Data.Maybe
import Control.Arrow
import Data.List

{-
    Unit tests for the queue...
-}

spec :: Spec
spec = do
    describe "Stream generation equivalences" $ do
        it "iterate succ k == Stream.iterate succ k" $ property $
            \(k :: Int) (n :: Positive Int) -> 
                let n' = getPositive n
                in take n' (iterate succ k) == Stream.take n' (Stream.iterate succ k)

        it "iterate pred k == Stream.iterate pred k" $ property $
            \(k :: Int) (n :: Positive Int) -> 
                let n' = getPositive n
                in take n' (iterate pred k) == Stream.take n' (Stream.iterate pred k)

        it "iterate f k == Stream.iterate f k" $ property $
            \(k :: Int) (n :: Positive Int) (Fn (f :: Int -> Int)) -> 
                let n' = getPositive n
                in take n' (iterate f k) == Stream.take n' (Stream.iterate f k)

        it "unfoldr f k == Stream.unfoldr f k" $ property $
            \(k :: Int) (n :: Positive Int) (Fn (f :: Int -> (Int, Int))) -> 
                let n' = getPositive n
                in take n' (unfoldr (Just . f) k) == Stream.take n' (Stream.unfoldr f k)

    describe "Stream.head equivalences" $ 
        it "k == Stream.head (Stream.iterate succ k)" $ property $
            \(k :: Int) -> k == Stream.head (Stream.iterate succ k)
