module RockPaperScissors where

import System.Random

data Choice
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show, Ord)

instance Random Choice where
  random g = (randomChoice, nextGen)
    where
      (randomValue, nextGen) = randomR (0, 2) g
      randomChoice = toChoice randomValue
  randomR _ = random
  randomIO = getStdRandom random

data Outcome
  = Win
  | Draw
  | Lose
  deriving (Eq, Show)

data Game = Game
  { myChoice :: Maybe Choice
  , opponentChoice :: Maybe Choice
  , outcome :: Maybe Outcome
  }

toChoice :: Int -> Choice
toChoice a =
  case a of
    0 -> Rock
    1 -> Paper
    2 -> Scissors

defeatedBy :: Choice -> Choice
defeatedBy Rock = Scissors
defeatedBy Paper = Rock
defeatedBy Scissors = Paper

calculateOutcome :: Choice -> Choice -> Outcome
calculateOutcome opponent player
  | opponent == player = Draw
  | opponent == defeatedBy player = Win
  | otherwise = Lose

play :: IO ()
play = undefined
-- generate opponent choice
-- ask player for his choice
-- calculate outcome
