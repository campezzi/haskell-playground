module IdentityCrisis where

newtype Identity a = Identity
  { runIdentity :: a
  } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

newtype IdentityT f a = IdentityT
  { runIdentityT :: f a
  } deriving (Eq, Show)

instance (Functor f) => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance (Applicative f) => Applicative (IdentityT f) where
  pure = IdentityT . pure
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance (Monad f) => Monad (IdentityT f) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
