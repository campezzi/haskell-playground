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
