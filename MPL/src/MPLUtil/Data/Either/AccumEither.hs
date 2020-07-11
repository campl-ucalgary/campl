module MPLUtil.Data.Either.AccumEither where

import Control.Monad.Except
import Data.Bifunctor

newtype AccumEither e a = AccumEither { runAccumEither :: Either e a }

instance Semigroup e => Functor (AccumEither e) where
    fmap f (AccumEither n) = AccumEither $ fmap f n

instance Semigroup e => Applicative (AccumEither e) where
    pure = AccumEither . Right

    AccumEither (Left e1) <*> AccumEither (Left e2) = 
        AccumEither (Left (e1 <> e2))
    AccumEither (Left e1) <*> AccumEither (Right _) = 
        AccumEither (Left e1)
    AccumEither (Right f) <*> n = f <$> n

instance (Semigroup e, Semigroup a) => Semigroup (AccumEither e a) where
    AccumEither a <> AccumEither b = AccumEither $ case (a,b) of
        (Left a', Left b') -> Left (a' <> b')
        (Right _, Left b') -> Left b'
        (Left a', Right _) -> Left a'
        (Right a', Right b') ->  Right (a' <> b')

instance (Semigroup e, Monoid a) => Monoid (AccumEither e a) where
    mempty = liftAEither (Right mempty)

instance Bifunctor AccumEither where
    bimap f g (AccumEither e) = AccumEither $ bimap f g e

liftAEither :: Semigroup e => Either e a -> AccumEither e a
liftAEither = AccumEither

liftAccumEither :: (MonadError e m, Semigroup e) => AccumEither e a -> m a
liftAccumEither = liftEither . runAccumEither

{- $>
runAccumEither (traverse (liftAEither . Left ) [[1],[2],[3]])
<$ -}
