{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Queue where

import Data.Word
import Prelude hiding (head, last, tail)

-- technically a double ended queue
-- based off of Chris Okasaki's work with O(1) queues..

data Queue a = Queue [a] !Word [a] !Word [a] [a]
    -- left queue, size, right queue, size, left unevalutated queue, right unevalutated queue

-- suggested size of constant..
c :: Word
c = 3

empty :: Queue a
empty = Queue [] 0 [] 0 [] []

prepend :: a -> Queue a -> Queue a
prepend e (Queue l lsz r rsz lu ru) 
    = mkQueue (e:l) (succ lsz) r rsz lu ru

append :: a -> Queue a -> Queue a
append e (Queue l lsz r rsz lu ru) 
    = mkQueue l lsz (e:r) (succ rsz) lu ru

head :: Queue a -> Maybe (Queue a, a)
head (Queue [] _ [] _ _ _) = Nothing
head (Queue [] _ [r] _ _ _) = Just (empty, r)
head (Queue (l:ls) lsz r rsz le re) 
    = Just (mkQueue ls (pred lsz) r rsz (drop 2 le) (drop 2 re), l)

last :: Queue a -> Maybe (Queue a, a)
last (Queue [] _ [] _ _ _) = Nothing
last (Queue [l] _ [] _ _ _) = Just (empty, l)
last (Queue l lsz (r:rs) rsz le re) 
    = Just (mkQueue l lsz rs (pred rsz) (drop 2 le) (drop 2 re), r)

mkQueue :: [a] -> Word -> [a] -> Word -> [a] -> [a] -> Queue a
mkQueue = undefined

rotate1 :: Word -> [a] -> [a] -> [a]
rotate1 = undefined

rotate2 :: Word -> [a] -> [a] -> [a]
rotate2 = undefined

