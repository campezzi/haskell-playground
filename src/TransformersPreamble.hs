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

--
newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  }

instance (Show (m (Either e a))) => Show (EitherT e m a) where
  show (EitherT mea) = "EitherT " ++ show mea

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
  pure a = EitherT $ (pure . pure) a
  (EitherT meab) <*> (EitherT mea) = EitherT $ liftA2 (<*>) meab mea

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f =
    EitherT $ do
      v <- mea
      case v of
        Left a -> return $ Left a
        Right b -> runEitherT (f b)

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: (Monad m) => (e -> m c) -> (a -> m c) -> EitherT e m a -> m c
eitherT femc famc (EitherT mea) = do
  v <- mea
  case v of
    Left e -> femc e
    Right a -> famc a
