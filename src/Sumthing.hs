module Sumthing where

data Sumthing a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sumthing a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sumthing a) where
  pure = undefined
  (<*>) = undefined

instance Monad (Sumthing a) where
  return = pure
  (>>=) = undefined
