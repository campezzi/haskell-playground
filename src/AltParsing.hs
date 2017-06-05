{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr =
  [r|
123
abc
456
def|]

a :: String
a = "blah"

b :: String
b = "123"

c :: String
c = "123blah789"

parseNoS :: Parser NumberOrString
parseNoS =
  skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter)

testAlt :: IO ()
testAlt = do
  print $ parseString (some parseNoS) mempty eitherOr

--
data DecimalOrFraction
  = Decimal Double
  | Fraction Rational
  deriving (Eq, Show)

parseDecimal :: Parser Double
parseDecimal = do
  integerPart <- integer
  char '.'
  decimalPart <- integer
  return $ toDouble integerPart decimalPart

toDouble :: Integer -> Integer -> Double
toDouble i d = (fromInteger ((i * multiplier) + d)) / (fromInteger multiplier)
  where
    multiplier = (10 ^) . length . show $ d

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimalOrFraction :: Parser DecimalOrFraction
parseDecimalOrFraction =
  try (Decimal <$> parseDecimal) <|> (Fraction <$> parseFraction)
