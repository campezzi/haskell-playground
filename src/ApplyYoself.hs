module ApplyYoself where

newtype AIdentity a =
  AIdentity a
  deriving (Eq, Show)

instance Functor AIdentity where
  fmap f (AIdentity a) = AIdentity (f a)

instance Applicative AIdentity where
  pure = AIdentity
  (<*>) (AIdentity f) (AIdentity x) = AIdentity (f x)

--
newtype AConstant a b = AConstant
  { getConstant :: a
  } deriving (Eq, Show)

instance Functor (AConstant a) where
  fmap _ (AConstant a) = AConstant a

instance Monoid a =>
         Applicative (AConstant a) where
  pure _ = AConstant mempty
  (<*>) (AConstant a) (AConstant b) = AConstant (mappend a b)
