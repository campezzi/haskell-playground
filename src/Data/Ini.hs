{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.RawString.QQ
import Text.Trifecta

--
headerEx :: ByteString
headerEx = "[MyHeader]"

newtype Header =
  Header String
  deriving (Eq, Show, Ord)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

--
assignmentEx :: ByteString
assignmentEx = "setting=1"

type Key = String

type Value = String

type Assignments = Map Key Value

parseAssigment :: Parser (Key, Value)
parseAssigment = do
  key <- some letter
  char '='
  val <- some (noneOf "\n")
  skipEOL
  return (key, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

--
commentEx :: ByteString
commentEx =
  "; last modified 1 April\
\ 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n \n#hah"

skipComments :: Parser ()
skipComments = skipMany comments
  where
    comments = do
      char ';' <|> char '#'
      skipMany (noneOf "\n")
      skipEOL

--
sectionEx :: ByteString
sectionEx =
  [r|
; ignore me
[states]
Thiago=Victoria
|]

sectionEx' :: ByteString
sectionEx' =
  [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothhandclaw
|]

data Section =
  Section Header
          Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssigment
  return $ Section h (M.fromList assignments)

--
rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  return $ Config $ foldr rollup M.empty sections
