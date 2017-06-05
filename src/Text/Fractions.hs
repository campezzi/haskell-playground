{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Data.Ratio ((%))
import Text.Trifecta

badFraction :: String
badFraction = "1/0"

alsoBad :: String
alsoBad = "10"

shouldWork :: String
shouldWork = "1/2"

shouldAlsoWork :: String
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

intParser :: Parser Integer
intParser = do
  n <- integer
  eof
  return n

testFractions :: IO ()
testFractions = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction
