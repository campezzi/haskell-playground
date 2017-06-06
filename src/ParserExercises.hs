module ParserExercises where

import Control.Applicative
import Text.Trifecta

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT
  compare (NOSS x) (NOSS y) = compare x y
  compare (NOSI x) (NOSI y) = compare x y

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer =
  SemVer Major
         Minor
         Patch
         Release
         Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer ma ia pa ra _) (SemVer mb ib pb rb _)
    | cMajor /= EQ = cMajor
    | cMinor /= EQ = cMinor
    | cPatch /= EQ = cPatch
    | otherwise = compareRelease ra rb
    where
      cMajor = compare ma mb
      cMinor = compare ia ib
      cPatch = compare pa pb

compareRelease :: Release -> Release -> Ordering
compareRelease [] [] = EQ
compareRelease [] (_:_) = GT
compareRelease (_:_) [] = LT
compareRelease x y = go x y
  where
    go [] [] = EQ
    go (_:_) [] = GT
    go [] (_:_) = LT
    go (a:as) (b:bs)
      | a == b = go as bs
      | otherwise = compare a b

--
parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  rel <- option [] parseRelease
  meta <- option [] parseMetadata
  return $ SemVer major minor patch rel meta

parseNoS :: Parser NumberOrString
parseNoS = try (NOSS <$> some letter) <|> (NOSI <$> integer)

parseRelease :: Parser Release
parseRelease = char '-' >> parseNoS `sepBy` (char '.')

parseMetadata :: Parser Metadata
parseMetadata = char '+' >> parseNoS `sepBy` (char '.')
