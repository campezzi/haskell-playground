module Sumthing where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sumthing a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sumthing a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sumthing a) where
  pure = Second
  First x <*> _ = First x
  _ <*> First x = First x
  Second f <*> Second x = Second (f x)

instance Monad (Sumthing a) where
  return = pure
  First x >>= _ = First x
  Second x >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sumthing a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [First x, Second y]

instance (Eq a, Eq b) => EqProp (Sumthing a b) where
  (=-=) = eq

checkSumthing :: IO ()
checkSumthing = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  where
    trigger = undefined :: Sumthing Int (Int, Bool, String)
