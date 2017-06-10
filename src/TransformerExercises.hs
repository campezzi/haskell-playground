{-# LANGUAGE UndecidableInstances #-}

module TransformerExercises where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

--
newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  }

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

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

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

--
maybeInt :: Int -> Maybe Int
maybeInt x
  | odd x = Nothing
  | otherwise = Just x

readSix :: Int -> ReaderT Int Maybe String
readSix x =
  ReaderT $ \_ ->
    if x == 6
      then Just "Six"
      else Nothing

newtype ReaderT r m a = ReaderT
  { runReaderT :: r -> m a
  }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ (pure . pure) a
  (ReaderT ramb) <*> (ReaderT rma) = ReaderT $ liftA2 (<*>) ramb rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

--
newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s)
  }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT smas) =
    StateT $ \s ->
      let mas = smas s
      in fmap (\(a, s') -> (f a, s')) mas

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT smfabs) <*> (StateT smas) =
    StateT $ \s -> do
      (fab, s') <- smfabs s
      (a, s'') <- smas s'
      return (fab a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT smas) >>= f =
    StateT $ \s -> do
      (a, s') <- smas s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma =
    StateT $ \s -> do
      a <- ma
      return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
