module ListHell where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

toMyList :: [a] -> List a
toMyList = foldr Cons Nil

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x (appendList xs ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = appendList (f <$> xs) (fs <*> xs)

instance Arbitrary a =>
         Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons x xs)]

instance Eq a =>
         EqProp (List a) where
  (=-=) = eq

--
check :: IO ()
check = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  where
    trigger = undefined :: List (Int, Bool, String)
