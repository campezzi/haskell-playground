{-# LANGUAGE UndecidableInstances #-}

module TransformersPreamble where

import Control.Applicative (liftA2)

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance (Show (m (Maybe a))) => Show (MaybeT m a) where
  show (MaybeT ma) = "MaybeT " ++ show ma

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure a = MaybeT $ (pure . pure) a
  (MaybeT mab) <*> (MaybeT ma) = MaybeT $ liftA2 (<*>) mab ma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just x -> runMaybeT (f x)
