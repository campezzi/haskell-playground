module ListHell where

import Control.Applicative
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

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x (appendList xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold appendList Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ fmap f xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = appendList (f <$> xs) (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x xs)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

--
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take' 3000 l
      ys' =
        let (ZipList' l) = ys
        in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' xs <*> ZipList' ys = ZipList' $ zip' xs ys
    where
      zip' Nil _ = Nil
      zip' _ Nil = Nil
      zip' (Cons f fs) (Cons v vs) = Cons (f v) (zip' fs vs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return $ ZipList' Nil), (2, return $ ZipList' (Cons x xs))]

--
check :: IO ()
check = do
  quickBatch $ applicative triggerList
  quickBatch $ applicative triggerZipList
  where
    triggerList = undefined :: List (Int, Bool, String)
    triggerZipList = undefined :: ZipList' (Int, Bool, String)
