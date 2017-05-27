module TraversablePreamble where

maybeNum :: Int -> Maybe Int
maybeNum a
  | even a = Just a
  | otherwise = Nothing

data MyThing =
  MyThing
  deriving (Show)

data MyError =
  MyError
  deriving (Show)

ioStrings :: IO [String]
ioStrings = return ["hello", "world"]

parse :: String -> Either MyError MyThing
parse x
  | length x == 5 = Right MyThing
  | otherwise = Left MyError

pipeline :: IO (Either MyError [MyThing])
pipeline = do
  strings <- ioStrings
  return $ traverse parse strings
