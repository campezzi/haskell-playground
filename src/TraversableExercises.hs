module TraversableExercises where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- something to use as the function for traverse
maybeNum :: Int -> Maybe Int
maybeNum x =
  if odd x
    then Just x
    else Nothing

-- Identity
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

checkIdentity :: IO ()
checkIdentity = do
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: Identity (Int, Int, [Int])

-- Constant
newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    x <- arbitrary
    return $ Constant x

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

checkConstant :: IO ()
checkConstant = do
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: Constant Int (Int, Int, [Int])

-- Maybe
data Optional a
  = Nope
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nope = Nope
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nope = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nope = pure Nope
  traverse f (Yep x) = Yep <$> f x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return Nope), (2, return $ Yep x)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

checkOptional :: IO ()
checkOptional = do
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: Optional (Int, Int, [Int])

-- List
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = mappend (f x) (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x xs)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

checkList :: IO ()
checkList = do
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: List (Int, Int, [Int])
