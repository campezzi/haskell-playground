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
  compare x y
    | cMajor /= EQ = cMajor
    | cMinor /= EQ = cMinor
    | cPatch /= EQ = cPatch
    | cRelease /= EQ = cRelease
    | otherwise = EQ
    where
      cMajor = compareMajor x y
      cMinor = compareMinor x y
      cPatch = comparePatch x y
      cRelease = compareRelease x y

compareMajor :: SemVer -> SemVer -> Ordering
compareMajor (SemVer x _ _ _ _) (SemVer y _ _ _ _) = compare x y

compareMinor :: SemVer -> SemVer -> Ordering
compareMinor (SemVer _ x _ _ _) (SemVer _ y _ _ _) = compare x y

comparePatch :: SemVer -> SemVer -> Ordering
comparePatch (SemVer _ _ x _ _) (SemVer _ _ y _ _) = compare x y

compareRelease :: SemVer -> SemVer -> Ordering
compareRelease (SemVer _ _ _ x _) (SemVer _ _ _ y _)
  | x == [] && y == [] = EQ
  | x == [] && y /= [] = GT
  | x /= [] && y == [] = LT
  | otherwise = go x y
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
