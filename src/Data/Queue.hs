{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Data.Queue where

import Data.Word
import Prelude hiding (head, length)
import Data.List

{- 
    Technically a double ended queue
    Completely based off of Chris Okasaki's work with O(1) queues...
    We include a short summary of what Chris writes in his papers...

    TODO -- fix the seq calls (they do not prevaluate properly)..
-}

data Queue a = Queue [a] !Word [a] !Word [a] [a]
    deriving (Eq)
{-
    The datatype is as follows...
        Queue L, |L|, R, |R|, Lu, Ru
    Where the arguments (in order) are
        Left queue, Left queue size, 
        Right queue, Right queue size, 
        Left unevaluted queue, Right unevaluted queue

    We want O(1) insertion and removal for both sides of the queue
    with a single element.  To achieve this, we need both sides of 
    the queue to be about the same size.

    Indeed, this prompts the following invariant...
        |L| <= c|R| + 1 
        |R| <= c|L| + 1
    Where c >= 2 is some constant (we use 3 in this implementation).

    When the invariant is violated, we need to truncate
    the longer list to about half the length of both lists;
    then rotate the remaining portion onto the back
    of the shorter list.

    So, suppose that the invariant is violated and wlog we have 
    c|L| + 1 < |R| iff c|L| + 2 < |R| + 1 iff c|L| + 2 <= |R| since 
    we are working with integers.

    We only append at most one element at a time to the queue,
    so since c >= 2, we have |R| <= c|L| + c + 1.

    Thus, we have established the following.
        c|L| + 2 <= |R| <= c|L| + c + 1

    Consider n = (|L| + |R|) `div` 2.
    Replace R with R' = take n R, so |R'| = n.
    Replace L with L' = L ++ reverse (drop n R), so
    |L'| = |L| + (|R| - n). 

    (Notice that this is clearly correct -- we are simply
    reversing and appending to the front as desired for a queue)

    If |L| + |R| is even, then we have 2n = |L| + |R| iff |L| = 2n - |R|, so
        |L'| = |L| + |R| - n = 2n - |R| + |R| - n = n <= cn + 1 = c|R'| + 1

    If |L| + |R| is odd, then we have 2n + 1 = |L| + |R| iff |L| = 2n + 1 - |R|, so
        |L'| = |L| + |R| - n = 2n - |R| + 1 + |R| - n = n + 1 <= cn + 1 = c|R'| + 1

    Hence, this will restore the invariant.  If we are able to 
    compute 
        R' = take n R
        L ++ reverse (drop n R)
    incrementally, then we will have our O(1) queue.

    We do this by distributing the reverse, drop, and append 
    in two phases with the rotate1 and rotate2 functions.

    The first phase, corresponds to drop.

    The second phase corresponds to reverse.

    We by processing c elements of R for each element of L.

    At the end of the first phase, we will need to process an
    extra n mod c elements of R (the left overs with n < c).

    At the end of the second phase, we process at most c + 1 
    left overs of R 

    Notice, that if we incrementally do things like this, the size of
    the reverse, drop, take, (++) is all bounded by c + 1 steps -- a constant.
    This is exactly what the rotate1 and rotate2 functions do.

    We make some remarks about the unevaluted queues used for preevaluation. 
    After a rotation, we set Lu to L and Ru to R, and by evaluating Lu and Ru,
    we can incrementally prevalute the aforementioned steps (rotations).
    
    Notice that if the two lists are each of size n, we can see that 
    the next rotation can occur after only about n - n/c operations (given
    they are repeatedly removed from the same side).

    So, to ensure Lu and Ru are completed evaluated, we need to advance them
    by 2 positions each. This demands the following invariant...
        |Lu| <= max(2j + 2 - k, 0)
        |Ru| <= max(2j + 2 - k, 0)
    Where j = min(|L|, |R|) and k = min(|L|, |R|).

    It is easy to establish the bound 2j + 2 - k after a rotation (|L| and |R| differ
    by at most one), and is reduced by at most one every insert and at most
    two every removal.
-}

showQueue :: Show a => Queue a -> String
showQueue (Queue l lsz r rsz lu ru)
    = "Queue (" ++ show (lsz + rsz) ++ "): " ++ show (l ++ reverse r)

showQueueWith :: Queue a -> ([a] -> String) -> String
showQueueWith (Queue l lsz r rsz lu ru) s
    = "Queue (" ++ show (lsz + rsz) ++ "): " ++ s (l ++ reverse r)

instance Show a => Show (Queue a) where
    show = showQueue

c :: Word
c = 3

empty :: Queue a
empty = Queue [] 0 [] 0 [] []

length :: Queue a -> Int
length (Queue l lsz r rsz lu ru) = fromIntegral (lsz + rsz)

genericLength :: Integral n => Queue a -> n
genericLength (Queue l lsz r rsz lu ru) = fromIntegral (lsz + rsz)

isEmpty :: Queue a -> Bool
isEmpty = (==0) . Data.Queue.length

infixr 7 <|
(<|) :: a -> Queue a -> Queue a
(<|) = prepend

infixl 7 |>
(|>) :: Queue a -> a -> Queue a
(|>) = append

prepend :: a -> Queue a -> Queue a
prepend e (Queue l lsz r rsz lu ru) 
    = mkQueue (e:l) (succ lsz) r rsz lu ru

append :: Queue a -> a -> Queue a
append (Queue l lsz r rsz lu ru) e  
    = mkQueue l lsz (e:r) (succ rsz) lu ru

head :: Queue a -> Maybe (Queue a, a)
head (Queue [] _ [] _ _ _) = Nothing
head (Queue [] _ [r] _ _ _) = Just (empty, r)
head (Queue (l:ls) lsz r rsz lu ru) 
    = Just (mkQueue ls' (pred lsz) r' rsz lu' ru', l)
    where
        lu' = drop 2 lu 
        ls' = lu' `seq` ls
        ru' = drop 2 ru
        r' = ru' `seq` r
    -- the seq is here to ensure that by taking the head,
    -- we prevalute...
{-
head (Queue (l:ls) lsz r rsz lu re) 
    = Just (mkQueue ls (pred lsz) r rsz (drop 2 lu) (drop 2 re), l)
    -}

last :: Queue a -> Maybe (Queue a, a)
last (Queue [] _ [] _ _ _) = Nothing
last (Queue [l] _ [] _ _ _) = Just (empty, l)
last (Queue l lsz (r:rs) rsz lu ru) 
    = Just (mkQueue l' lsz rs' (pred rsz) lu' ru', r)
    where
        l' = lu' `seq` l
        lu' = drop 2 lu
        rs' = ru' `seq` rs
        ru' = drop 2 ru
    -- the seq is here to ensure that by taking the head,
    -- we prevalute...
{-
last (Queue l lsz (r:rs) rsz lu ru) 
    = Just (mkQueue l lsz rs (pred rsz) (drop 2 lu) (drop 2 ru), r)
    -}

mkQueue :: [a] -> Word -> [a] -> Word -> [a] -> [a] -> Queue a
mkQueue l lsz r rsz lu ru 
    | lsz > c * rsz + 1 
        = let len = lsz + rsz
              n = len `div` 2 
              l' = genericTake n l
              lsz' = n
              r' = rotate1 n r rsz l lsz
              rsz' = rsz + (lsz - n)
          in Queue l' lsz' r' rsz' l' r'
    -- this case is similiar to above...
    | rsz > c * lsz + 1
        = let n = (lsz + rsz) `div` 2 
              l' = rotate1 n l lsz r rsz
              r' = genericTake n r
              lsz' = lsz + (rsz - n)
              rsz' = n
          in Queue l' lsz' r' rsz' l' r'
    | otherwise = Queue l lsz r rsz lu ru

rotate1 :: Word -> [a] -> Word -> [a] -> Word -> [a]
rotate1 n (l:ls) lsz r rsz 
    -- | n >= c = l : rotate1 (n - c) ls (pred lsz) (genericDrop c r) (rsz - c)
    | n >= c = l' : rotate1 (n - c) ls (pred lsz) r' (rsz - c)
    where
        r' = genericDrop c r
        l' = r' `seq` l
-- rotate1 n l lsz r rsz = rotate2 l lsz (genericDrop n r) (rsz - n) []
rotate1 n l lsz r rsz = rotate2 l lsz r' (rsz - n) []
    where
        l' = r' `seq` l
        r' = genericDrop n r

rotate2 :: [a] -> Word -> [a] -> Word -> [a] -> [a]
rotate2 (l:ls) lsz r rsz acc 
    -- | rsz >= c = l : rotate2 ls (pred lsz) (genericDrop c r) (rsz - c) (reverse (genericTake c r) ++ acc)
    | rsz >= c = l' : rotate2 ls (pred lsz) r' (rsz - c) r'''
    where
        r' = genericDrop c r
        r'' = reverse (genericTake c r)
        r''' = r'' ++ acc
        l' = ((r'' `seq` r''') `seq` r') `seq` l
rotate2 l lsz r rsz acc = l ++ reverse r ++ acc
