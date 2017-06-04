module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

oneTwoThree :: Parser String
oneTwoThree = string "123"

oneTwoThreeS :: Parser String
oneTwoThreeS = choice [string "123", string "12", string "1", stop]

oneTwoThreeC :: Parser Char
oneTwoThreeC = char '1' >> char '2' >> char '3'

--
testParse :: Show a => Parser a -> String -> IO ()
testParse p s = print $ parseString p mempty s

main :: IO ()
main = do
  testParse oneTwoThreeS "1"
  testParse oneTwoThreeS "12"
  testParse oneTwoThreeS "123"
  testParse oneTwoThreeC "123"
