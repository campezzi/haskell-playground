module MonadExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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
