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

-- Three
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

checkThree :: IO ()
checkThree = do
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: Three Int Int (Int, Int, [Int])

-- Three'
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = mappend (f y) (f z)

instance Traversable (Three' a) where
  traverse f (Three' x y z) = liftA2 (Three' x) (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

checkThree' :: IO ()
checkThree' = do
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: Three' Int (Int, Int, [Int])

-- S
data S n a =
  S (n a)
    a
  deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S x y) = mappend (foldMap f x) (f y)

instance (Traversable n) => Traversable (S n) where
  traverse f (S x y) = liftA2 S (traverse f x) (f y)

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ S x y

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

checkS :: IO ()
checkS = do
  quickBatch $ functor trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: S Maybe (Int, Int, [Int])
