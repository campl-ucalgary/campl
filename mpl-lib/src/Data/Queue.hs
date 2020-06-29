{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
    -- this allows us to define our own 
    -- pattern matching syntax which 
    -- is useful for the channel manager..
{-# LANGUAGE LambdaCase #-}
    -- allows us to write \case x:xs -> ... ; [] -> ...
    -- instead of (\lst -> case lst of x:xs -> ... ; [] -> ...)
module Data.Queue 
    ( Data.Queue.Queue
    , Data.Queue.empty
    , Data.Queue.head
    , Data.Queue.last
    , Data.Queue.prepends
    , Data.Queue.prepend
    , Data.Queue.append
    , Data.Queue.appends
    , Data.Queue.isEmpty
    , Data.Queue.invariantCheck
    , Data.Queue.takeHead
    , Data.Queue.takeLast
    , Data.Queue.toList
    , Data.Queue.fromList
    , (<|) 
    , (|>) 
    , pattern (:<|)
    , pattern (:<||)
    , pattern Empty
    )
    where

import Data.Word
import Prelude hiding (head, length)
import Control.Arrow
import Data.List as L

{- 
    Technically a double ended queue
    Completely based off of Chris Okasaki's work with O(1) queues...
    We include a short summary of what Chris writes in his papers...

    TODO -- fix the seq calls (they do not prevaluate properly)..
-}

data Queue a = Queue [a] !Word [a] !Word [a] [a]

instance Show a => Show (Queue a) where
    show = showQueue

instance Eq a => Eq (Queue a) where
    Queue l1 lsz1 r1 rsz1 _ _ == Queue l2 lsz2 r2 rsz2 _ _
        = (lsz1 + rsz1) == (lsz2 + rsz2) && l1 ++ reverse r1 == l2 ++ reverse r2


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

    Where j = min(|L|, |R|) and k = max(|L|, |R|).

    It is easy to establish the bound 2j + 2 - k after a rotation (|L| and |R| differ
    by at most one), and is reduced by at most one every insert and at most
    two every removal.
-}


-- | helpful for desiging / debugging..
invariantCheck :: Queue a -> Bool
invariantCheck (Queue l lsz r rsz lu ru)
    = L.genericLength l == lsz 
        && L.genericLength r == rsz 
        && L.genericLength l <= c * L.genericLength r + 1
        && L.genericLength r <= c * L.genericLength l + 1
        && L.genericLength lu <= max (2 * j + 2 - k) 0
        && L.genericLength ru <= max (2 * j + 2 - k) 0
  where
    j = min (L.genericLength l) (L.genericLength r)
    k = max (L.genericLength l) (L.genericLength r)



showQueue :: Show a => Queue a -> String
showQueue (Queue l lsz r rsz lu ru)
    = "Queue (" ++ show (lsz + rsz) ++ "): " ++ show (l ++ reverse r)

showQueueWith :: Queue a -> ([a] -> String) -> String
showQueueWith (Queue l lsz r rsz lu ru) s
    = "Queue (" ++ show (lsz + rsz) ++ "): " ++ s (l ++ reverse r)

c :: Word
c = 3

toList :: Queue a -> [a]
toList (Queue l1 lsz1 r1 rsz1 _ _) = l1 ++ reverse r1

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

-- cons for the front of the queue pattern
infixr 7 :<|
pattern Front a as <- (Data.Queue.head -> Just (as, a))
pattern a :<| as <- (Data.Queue.head -> Just (as, a))

-- | Gets the heads of two queues (if they exist..) and 
-- with the modified queues...
headTuple :: 
    (Queue a, Queue a) ->
    ((Queue a, Queue a), (Maybe a, Maybe a))
headTuple (q1, q2) = ((q1', q2'), (n1, n2))
  where
    (q1', n1) = f q1
    (q2', n2) = f q2

    f q = 
        case Data.Queue.head q of
            Nothing -> (q, Nothing)
            Just (q', v) -> (q', Just v)

-- this is a pattern to get the head instructions of a tuple of queues (if
-- they exist of course...)
pattern heads :<|| tails <- 
    (headTuplePatternHelper . headTuple -> Just (heads, tails)) 

headTuplePatternHelper ((q1s,q2s), (Just q1, Just q2)) = Just ((q1,q2), (q1s, q2s))
headTuplePatternHelper _ = Nothing
    

-- snoc for the back of the queue pattern
infixl 7 :|>
pattern Rear as a <- (Data.Queue.last -> Just (as, a))
pattern a :|> as <- (Data.Queue.last -> Just (as, a))

infixl 7 |>
(|>) :: Queue a -> a -> Queue a
(|>) = append

-- pattern for empty queue
pattern Empty <- (Data.Queue.isEmpty -> True)

prepend :: a -> Queue a -> Queue a
prepend e (Queue l lsz r rsz lu ru) 
    = mkQueue (e:l) (succ lsz) r rsz (drop 1 lu) (drop 1 ru)

prepends :: [a] -> Queue a -> Queue a
prepends lst q = foldr prepend q lst

append :: Queue a -> a -> Queue a
append (Queue l lsz r rsz lu ru) e  
    = mkQueue l lsz (e:r) (succ rsz) (drop 1 lu) (drop 1 ru)

-- this litearlly just adjoints it at the end
appends :: Queue a -> [a] -> Queue a
appends q lst = foldr (flip append) q lst

fromList :: [a] -> Queue a
fromList =  appends empty

head :: Queue a -> Maybe (Queue a, a)
head (Queue [] _ [] _ _ _) = Nothing
head (Queue [] _ (r:_) _ _ _) = Just (empty, r)
{-
head (Queue (l:ls) lsz r rsz lu ru) 
    = Just (mkQueue ls' (pred lsz) r' rsz lu' ru', l)
    where
        lu' = drop 2 lu 
        ls' = lu' `seq` ls
        ru' = drop 2 ru
        r' = ru' `seq` r
    -- the seq is here to ensure that by taking the head,
    -- we prevalute...
    -}
head (Queue (l:ls) lsz r rsz lu re) 
    = Just (mkQueue ls (pred lsz) r rsz (drop 2 lu) (drop 2 re), l)

last :: Queue a -> Maybe (Queue a, a)
last (Queue [] _ [] _ _ _) = Nothing
last (Queue (l:_) _ [] _ _ _) = Just (empty, l)
{-
last (Queue l lsz (r:rs) rsz lu ru) 
    = Just (mkQueue l' lsz rs' (pred rsz) lu' ru', r)
    where
        l' = lu' `seq` l
        lu' = drop 2 lu
        rs' = ru' `seq` rs
        ru' = drop 2 ru
    -- the seq is here to ensure that by taking the head,
    -- we prevalute...
    -}
last (Queue l lsz (r:rs) rsz lu ru) 
    = Just (mkQueue l lsz rs (pred rsz) (drop 2 lu) (drop 2 ru), r)

takeHead :: Int -> Queue a -> (Queue a, [a])
takeHead n q
    | n < 0 = error ("invalid number " ++ show n ++ " for Queue.takeHead.")
    | n == 0 = (q, [])
    | otherwise = case Data.Queue.head q of
                    Nothing -> (q, [])
                    Just (q', a) -> second (a :) (takeHead (n-1) q')

takeLast :: Int -> Queue a -> (Queue a, [a])
takeLast n q
    | n < 0 = error ("invalid number " ++ show n ++ " for Queue.takeHead.")
    | n == 0 = (q, [])
    | otherwise = case Data.Queue.last q of
                    Nothing -> (q, [])
                    Just (q', a) -> second (a:) (takeLast (n-1) q')

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
    | n >= c = l : rotate1 (n - c) ls (pred lsz) (genericDrop c r) (rsz - c)
    {-
    | n >= c = l' : rotate1 (n - c) ls (pred lsz) r' (rsz - c)
    where
        r' = genericDrop c r
        l' = r' `seq` l
        -}
rotate1 n l lsz r rsz = rotate2 l lsz (genericDrop n r) (rsz - n) []
{-
rotate1 n l lsz r rsz = rotate2 l lsz r' (rsz - n) []
    where
        l' = r' `seq` l
        r' = genericDrop n r-}

rotate2 :: [a] -> Word -> [a] -> Word -> [a] -> [a]
rotate2 (l:ls) lsz r rsz acc 
    | rsz >= c = l : rotate2 ls (pred lsz) (genericDrop c r) (rsz - c) (reverse (genericTake c r) ++ acc)
    {-
    | rsz >= c = l' : rotate2 ls (pred lsz) r' (rsz - c) r'''
    where
        r' = genericDrop c r
        r'' = reverse (genericTake c r)
        r''' = r'' ++ acc
        l' = ((r'' `seq` r''') `seq` r') `seq` l
        -}
rotate2 l lsz r rsz acc = l ++ reverse r ++ acc
