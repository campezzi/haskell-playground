module ApplyYoself where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers

newtype AIdentity a =
  AIdentity a
  deriving (Eq, Show)

instance Functor AIdentity where
  fmap f (AIdentity a) = AIdentity (f a)

instance Applicative AIdentity where
  pure = AIdentity
  (<*>) (AIdentity f) (AIdentity x) = AIdentity (f x)

instance Arbitrary a =>
         Arbitrary (AIdentity a) where
  arbitrary = do
    x <- arbitrary
    return (AIdentity x)

instance Eq a =>
         EqProp (AIdentity a) where
  (=-=) (AIdentity a) (AIdentity b) = property (a == b)

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

instance (Arbitrary a) =>
         Arbitrary (AConstant a b) where
  arbitrary = do
    a <- arbitrary
    return (AConstant a)

instance Eq a =>
         EqProp (AConstant a b) where
  (=-=) (AConstant a) (AConstant b) = property (a == b)

--
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
    then Nothing
    else Just s

newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name
         Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson name address = Person <$> mkName name <*> mkAddress address

mkPerson' :: String -> String -> Maybe Person
mkPerson' name address = liftA2 Person (mkName name) (mkAddress address)
