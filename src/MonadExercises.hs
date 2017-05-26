module MonadExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

checkNope :: IO ()
checkNope = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  where
    trigger = undefined :: Nope (Int, Int, Int)

--
newtype MIdentity a =
  MIdentity a
  deriving (Eq, Show)

instance Functor MIdentity where
  fmap f (MIdentity a) = MIdentity $ f a

instance Applicative MIdentity where
  pure = MIdentity
  (MIdentity f) <*> (MIdentity x) = MIdentity $ f x

instance Monad MIdentity where
  return = pure
  (MIdentity x) >>= f = f x

instance (Arbitrary a) => Arbitrary (MIdentity a) where
  arbitrary = do
    x <- arbitrary
    return $ MIdentity x

instance (Eq a) => EqProp (MIdentity a) where
  (=-=) = eq

checkMIdentity :: IO ()
checkMIdentity = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  where
    trigger = undefined :: MIdentity (Int, Int, Int)

--
j :: Monad m => m (m a) -> m a
j outer = do
  inner <- outer
  value <- inner
  return value

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f mx = do
  x <- mx
  return $ f x

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f mx my = do
  x <- mx
  y <- my
  return $ f x y

ap :: Monad m => m a -> m (a -> b) -> m b
ap mx mf = do
  x <- mx
  f <- mf
  return $ f x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  y <- f x
  ys <- meh xs f
  return (y:ys)

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
