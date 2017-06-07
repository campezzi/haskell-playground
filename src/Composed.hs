module Composed where

import Control.Applicative (liftA2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity
  { runIdentity :: a
  }

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ (pure . pure) x
  (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose a) = (foldMap . foldMap) f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose a) = Compose <$> (traverse . traverse) f a

instance (Arbitrary (f (g a))) => Arbitrary (Compose f g a) where
  arbitrary = do
    fga <- arbitrary
    return $ Compose fga

instance (Eq (f (g a))) => EqProp (Compose f g a) where
  (=-=) = eq

checkCompose :: IO ()
checkCompose = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ traversable trigger
  where
    trigger = undefined :: Compose Maybe Maybe (Int, Int, [Int])

--
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

--
data Deux a b =
  Deux a
       b
  deriving (Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

--
data Const a b =
  Const a
  deriving (Show)

instance Bifunctor Const where
  bimap f _ (Const a) = Const $ f a

--
data Drei a b c =
  Drei a
       b
       c
  deriving (Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

--
data SuperDrei a b c =
  SuperDrei a
            b
  deriving (Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

--
data SemiDrei a b c =
  SemiDrei a
  deriving (Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = (SemiDrei a)

--
data Quadriceps a b c d =
  Quadzzz a
          b
          c
          d
  deriving (Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

--
instance Bifunctor Either where
  bimap f _ (Left a) = Left $ f a
  bimap _ g (Right b) = Right $ g b
