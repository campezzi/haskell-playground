module FoldableExercises where

import Control.Monad
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . (foldMap Sum)

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . (foldMap Product)

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x t = foldr (\e a -> a || (x == e)) False t

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' t = foldr maybeMin Nothing t
  where
    maybeMin x Nothing = return x
    maybeMin x my = liftM (min x) my

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' t = foldr maybeMax Nothing t
  where
    maybeMax x Nothing = return x
    maybeMax x my = liftM (max x) my

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ a -> a + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\e a -> f e <> a) mempty

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (mappend . f) mempty

--
data FConstant a b =
  FConstant a
  deriving (Show)

instance Foldable (FConstant a) where
  foldMap _ _ = mempty

--
data FTwo a b =
  FTwo a
       b
  deriving (Show)

instance Foldable (FTwo a) where
  foldMap f (FTwo _ b) = f b

--
data FThree a b c =
  FThree a
         b
         c
  deriving (Show)

instance Foldable (FThree a b) where
  foldMap f (FThree _ _ c) = f c

--
data FThree' a b =
  FThree' a
          b
          b
  deriving (Show)

instance Foldable (FThree' a) where
  foldMap f (FThree' _ b c) = f b <> f c

--
data FFour a b =
  FFour a
        b
        b
        b
  deriving (Show)

instance Foldable (FFour a) where
  foldMap f (FFour _ b c d) = f b <> f c <> f d

--
filterF ::
     (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f t = foldMap valueOrEmpty t
  where
    valueOrEmpty x
      | f x = pure x
      | otherwise = mempty
