module ReaderExercises where

import Control.Applicative (liftA2)
import Data.Char

cap :: [Char] -> [Char]
cap = fmap toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = reverse . cap

fmapped :: [Char] -> [Char]
fmapped = reverse <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  x <- cap
  y <- rev
  return (x, y)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= (\x -> rev >>= (\y -> return (x, y)))

--
newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

thiago :: Person
thiago = Person (HumanName "Thiago") (DogName "Pepe") (Address "St Kilda Rd")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

--
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

--
newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader r) where
  fmap f (Reader a) = Reader $ f . a

instance Applicative (Reader r) where
  pure a = Reader $ \_ -> a
  (Reader fab) <*> (Reader fa) = Reader $ \r -> fab r (fa r)
