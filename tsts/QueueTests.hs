{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck

import Data.Queue (Queue (..))
import qualified Data.Queue as Queue

import Control.Monad
import Data.Maybe

prop_prependAppend :: [Int] -> Bool
prop_prependAppend xs = 
    foldr Queue.prepend Queue.empty xs
        == foldl Queue.append Queue.empty xs

prop_prependHeadEquality :: NonEmptyList Int -> Bool
prop_prependHeadEquality lst = 
    foldr Queue.prepend q xs
        == foldr f q xs
  where
    x:xs = getNonEmpty lst
    q = Queue.prepend x Queue.empty
    f n q = case fromJust (Queue.head (Queue.prepend n q)) of
                (q', n') -> Queue.prepend n' q'

prop_appendLastEquality :: NonEmptyList Int -> Bool
prop_appendLastEquality lst = 
    foldr (flip Queue.append) q xs
        == foldr f q xs
  where
    x:xs = getNonEmpty lst
    q = Queue.append Queue.empty x 
    f n q = case fromJust (Queue.last (Queue.append q n)) of
                (q', n') -> Queue.append q' n' 

prop_prependInvariants :: [Int] -> Bool
prop_prependInvariants lst = 
    Queue.invariantCheck (foldr Queue.prepend Queue.empty lst)

prop_appendInvariants :: [Int] -> Bool
prop_appendInvariants lst = 
    Queue.invariantCheck (foldr (flip Queue.append) Queue.empty lst)

prop_prependTakeEq :: NonEmptyList Int -> Property
prop_prependTakeEq lst = forAll (choose (0, length xs - 1)) $ \i ->
    let (q', hds) = Queue.takeHead i q in Queue.prepends hds q'  == q
  where
    xs = getNonEmpty lst
    q = foldr Queue.prepend Queue.empty xs

prop_appendTakeEq :: NonEmptyList Int -> Property
prop_appendTakeEq lst = forAll (choose (0, length xs - 1)) $ \i ->
    let (q', tls) = Queue.takeLast i q in Queue.appends q' tls == q
  where
    xs = getNonEmpty lst
    q = foldr Queue.prepend Queue.empty xs

prop_appendPrependTakeEq :: NonEmptyList Int -> Property
prop_appendPrependTakeEq lst = 
    forAll (choose (0, length xs - 1)) $ \i ->
    forAll (choose (0, length xs - 1)) $ \j ->
        uncurry (==) (appendPrependTakeEq xs i j)
  where
    xs = getNonEmpty lst

prop_appendPrependInvariants :: NonEmptyList Int -> Property
prop_appendPrependInvariants lst = 
    forAll (choose (0, length xs - 1)) $ \i ->
    forAll (choose (0, length xs - 1)) $ \j ->
        Queue.invariantCheck (fst (appendPrependTakeEq xs i j))
  where
    xs = getNonEmpty lst

appendPrependTakeEq :: [a] -> Int -> Int -> (Queue a, Queue a)
appendPrependTakeEq xs i j =
    let (q', tls) = Queue.takeLast i q 
        q'' = Queue.appends q' tls
        (q''', hds) = Queue.takeHead j q'' 
        q'''' = Queue.prepends hds q'''
    in (q'''', q)
  where
    q = foldr Queue.prepend Queue.empty xs

prop_prependAppendTakeEq :: NonEmptyList Int -> Property
prop_prependAppendTakeEq lst = 
    forAll (choose (0, length xs - 1)) $ \i ->
    forAll (choose (0, length xs - 1)) $ \j ->
        uncurry (==) (prependAppendTake xs i j)
  where
    xs = getNonEmpty lst

prependAppendTake :: [a] -> Int -> Int -> (Queue a, Queue a)
prependAppendTake xs i j =
    let (q', hds) = Queue.takeHead i q 
        q'' = Queue.prepends hds q' 
        (q''', tls) = Queue.takeLast j q'' 
        q'''' = Queue.appends q''' tls 
    in (q'''', q)
  where
    q = foldr Queue.prepend Queue.empty xs


prop_prependAppendInvariants :: NonEmptyList Int -> Property
prop_prependAppendInvariants lst = 
    forAll (choose (0, length xs - 1)) $ \i ->
    forAll (choose (0, length xs - 1)) $ \j ->
        Queue.invariantCheck (fst (prependAppendTake xs i j))
  where
    xs = getNonEmpty lst

return []
runTests = $quickCheckAll

main :: IO ()
main = void runTests 
