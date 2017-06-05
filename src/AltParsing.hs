module AltParsing where

import Control.Applicative
import Text.Trifecta

type NumberOrString = Either Integer String

a :: String
a = "blah"

b :: String
b = "123"

c :: String
c = "123blah789"

parseNoS :: Parser NumberOrString
parseNoS = (Left <$> integer) <|> (Right <$> some letter)

testAlt :: IO ()
testAlt = do
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b
  print $ parseString parseNoS mempty a
  print $ parseString parseNoS mempty b
  print $ parseString parseNoS mempty c
  print $ parseString (many parseNoS) mempty c
  print $ parseString (some parseNoS) mempty c
