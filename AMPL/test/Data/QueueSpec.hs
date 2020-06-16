{-# LANGUAGE ScopedTypeVariables #-}
module Data.QueueSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Data.Queue (Queue (..), (<|), (|>))
import qualified Data.Queue as Queue

import Control.Monad
import Data.Maybe
import Control.Arrow

{-
    Unit tests for the queue...
-}

spec :: Spec
spec = do
    describe "Internal invariants" $ do
        it "Invariant is maintained after prepending" $ property $ 
            \(lst :: [Int]) -> Queue.invariantCheck (foldr Queue.prepend Queue.empty lst)

        it "Invariant is maintained after appending" $ property $ 
            \(lst :: [Int]) -> Queue.invariantCheck (foldr (flip Queue.append) Queue.empty lst)

        it "Invariant is maintained from prepends, takeHead" $ property $ 
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \j ->
                        Queue.invariantCheck (fst (Queue.takeHead j (Queue.prepends xs Queue.empty)))

        it "Invariant is maintained from appends, takeHead" $ property $ 
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \j ->
                        Queue.invariantCheck (fst (Queue.takeHead j (Queue.appends Queue.empty xs)))

        it "Invariant is maintained from prepends, takeLast" $ property $ 
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \j ->
                        Queue.invariantCheck (fst (Queue.takeLast j (Queue.prepends xs Queue.empty)))

        it "Invariant is maintained from appends, takeLast" $ property $ 
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \j ->
                        Queue.invariantCheck (fst (Queue.takeLast j (Queue.appends Queue.empty xs)))

        it "Invariant is maintained after takeHead, prepend, takeLast, then append" $ property $ 
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \i ->
                    forAll (choose (0, length xs - 1)) $ \j ->
                        Queue.invariantCheck (fst (prependAppendTake xs i j))

        it "Invariant is maintained after takeLast, append, takeHead, then prepend" $ property $ 
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \i -> 
                        forAll (choose (0, length xs - 1)) $ \j ->
                            Queue.invariantCheck (fst (appendPrependTakeEq xs i j))


    describe "Append/prepend equalities" $ do
        it "Prepending is the same as appending the reverse" $ property $
            \(xs :: [Int]) -> 
                foldr Queue.prepend Queue.empty xs == foldl Queue.append Queue.empty xs

        it "Prepending, taking the head, then prepending again is the same as just prepending" $ property $
            \(lst :: NonEmptyList Int) -> 
                let x:xs = getNonEmpty lst 
                    q = Queue.prepend x Queue.empty
                    f n q = case fromJust (Queue.head (Queue.prepend n q)) of
                                (q', n') -> Queue.prepend n' q'
                in foldr Queue.prepend q xs == foldr f q xs
            
        it "Appending, taking the last, then appending again is the same as just appending" $ property $
            \(lst :: NonEmptyList Int) -> 
                let x:xs = getNonEmpty lst
                    q = Queue.append Queue.empty x 
                    f n q = case fromJust (Queue.last (Queue.append q n)) of
                            (q', n') -> Queue.append q' n' 
                in foldr (flip Queue.append) q xs == foldr f q xs

        it "takeHead n items and prepending them back on is the same as the original queue" $ property $
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst 
                    q = foldr Queue.prepend Queue.empty xs
                in forAll (choose (0, length xs - 1)) $ \i ->
                    let (q', hds) = Queue.takeHead i q 
                    in Queue.prepends hds q'  == q

        it "takeLast n items and append them back on is the same as the original queue" $ property $
            \(lst :: NonEmptyList Int) -> 
                let xs = getNonEmpty lst 
                    q = foldr Queue.prepend Queue.empty xs
                in forAll (choose (0, length xs - 1)) $ \i ->
                    let (q', tls) = Queue.takeLast i q 
                    in Queue.appends q' tls == q

        it "takeLast n items, append them back on the queue, takeHead k items, and prepend is the same as the original queue" $ property $
            \(lst :: NonEmptyList Int) -> 
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \i ->
                    forAll (choose (0, length xs - 1)) $ \j ->
                        uncurry (==) (appendPrependTakeEq xs i j)

        it "takeHead n items, prepend them back on the queue, takeLast k items, and append is the same as the original queue" $ property $
            \(lst :: NonEmptyList Int) ->
                let xs = getNonEmpty lst
                in forAll (choose (0, length xs - 1)) $ \i -> 
                    forAll (choose (0, length xs - 1)) $ \j ->
                        uncurry (==) (prependAppendTake xs i j)

    describe "Examples" $ do
        it "Manipulating a q = 5 <| 4 <| 3 <| Queue.empty and taking the head" $ do
            let q = 5 <| 4 <| 3 <| Queue.empty
            assertEqual "Queue.toList q == [5,4,3]" (Queue.toList q) [5,4,3]
            assertEqual "Queue.head q == Just (4 <| 3 <| Queue.empty, 5)" (Queue.head q) (Just (4 <| 3 <| Queue.empty, 5))


        it "Queue.toList (Queue.empty |> 5 |> 4 |> 3) == [5,4,3]" $ do
            Queue.toList (Queue.empty |> 5 |> 4 |> 3) `shouldBe` [5,4,3]

        it "Queue.head (Queue.empty |> 5 |> 4 |> 3) == Just ( 4 <| 3 <| Queue.empty, 5)" $ do
            Queue.head (Queue.empty |> 5 |> 4 |> 3) `shouldBe` Just (4 <| 3 <| Queue.empty ,5)

        it "Queue.last (Queue.empty |> 5 |> 4 |> 3) == Just ( 5 <| 4 <| Queue.empty, 3)" $ do
            Queue.last (Queue.empty |> 5 |> 4 |> 3) `shouldBe` Just (5 <| 4 <| Queue.empty ,3)

appendPrependTakeEq :: [a] -> Int -> Int -> (Queue a, Queue a)
appendPrependTakeEq xs i j =
    let (q', tls) = Queue.takeLast i q 
        q'' = Queue.appends q' tls
        (q''', hds) = Queue.takeHead j q'' 
        q'''' = Queue.prepends hds q'''
    in (q'''', q)
  where
    q = foldr Queue.prepend Queue.empty xs

prependAppendTake :: [a] -> Int -> Int -> (Queue a, Queue a)
prependAppendTake xs i j =
    let (q', hds) = Queue.takeHead i q 
        q'' = Queue.prepends hds q' 
        (q''', tls) = Queue.takeLast j q'' 
        q'''' = Queue.appends q''' tls 
    in (q'''', q)
  where
    q = foldr Queue.prepend Queue.empty xs
