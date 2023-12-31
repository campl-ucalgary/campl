module MplUtil.Data.Stream where

{- Module for defining a Stream
 - TODO: I do not think this is actually used anywhere?
 - Remove this in the future...
 -}

infixr 5 :/

data Stream a = a :/ Stream a
    deriving (Show, Eq)

instance Functor Stream where
    fmap = MplUtil.Data.Stream.map

map :: (a -> b) -> Stream a -> Stream b
map f ~(a :/ as) = (f a) :/ (MplUtil.Data.Stream.map f as)

head :: Stream a -> a
head (a :/ _) = a

take :: Int -> Stream a -> [a]
take n (a :/ as) 
    | n < 0 = error ("Stream.take negative input of " ++ show n)
    | n == 0 = []
    | otherwise = a : MplUtil.Data.Stream.take (n - 1) as

tail :: Stream a -> Stream a
tail (_ :/ as) = as

unfoldr :: (b -> (a, b)) -> b -> Stream a
unfoldr f b = a :/ (unfoldr f b')
  where
    ~(a, b') = f b

unfoldrM :: Monad m => (b -> m (a, b)) -> b -> m (Stream a)
unfoldrM f b = do
    (a, b') <- f b
    rst <- (unfoldrM f b')
    return $ a :/ rst

iterate :: (a -> a) -> a -> Stream a
iterate f = unfoldr (\b -> (b, f b)) 

toList :: Stream a -> [a]
toList ~(a :/ as) = a : toList as

fromList :: [a] -> Stream a 
fromList = foldr (:/) (error "Illegal `fromList' in MplUtil.Data.Stream")

