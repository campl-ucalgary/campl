module Data.Stream 
    ( Stream (..)
    , unfoldr
    , Data.Stream.iterate
    , Data.Stream.head
    , Data.Stream.tail )
    where

{-
    Infinite streams.
    Functions simliar to Data.List are implemented here..
-}

data Stream a = Stream a (Stream a)
    deriving (Show, Eq)

head :: Stream a -> a
head (Stream a _) = a

tail :: Stream a -> Stream a
tail (Stream _ as) = as

unfoldr :: (b -> (a, b)) -> b -> Stream a
unfoldr f b = Stream a (unfoldr f b')
  where
    (a, b') = f b

iterate :: (a -> a) -> a -> Stream a
iterate f = unfoldr (\b -> (b, f b)) 
