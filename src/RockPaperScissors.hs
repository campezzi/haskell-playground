module RockPaperScissors where

import Control.Monad (forever, replicateM)
import Control.Monad.Trans.State
import System.Random
import Text.Read (readMaybe)

data Choice
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show, Ord, Read)

instance Random Choice where
  random g = (randomChoice, nextGen)
    where
      (randomValue, nextGen) = randomR (0, 2) g
      randomChoice = toChoice randomValue
  randomR _ = random
  randomIO = getStdRandom random

data Outcome
  = Win
  | Lose
  | Draw
  deriving (Eq, Show)

data Scoreboard = Scoreboard
  { playerScore :: Int
  , opponentScore :: Int
  }

instance Show Scoreboard where
  show (Scoreboard p o) = "Player " ++ (show p) ++ ", Opponent " ++ (show o)

newScoreboard :: Scoreboard
newScoreboard = Scoreboard 0 0

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

updateScoreboard :: Scoreboard -> Outcome -> Scoreboard
updateScoreboard (Scoreboard player opponent) outcome =
  case outcome of
    Win -> Scoreboard (player + 1) opponent
    Lose -> Scoreboard player (opponent + 1)
    _ -> Scoreboard player opponent

friendly :: Outcome -> String
friendly o =
  case o of
    Win -> "You win!"
    Lose -> "You lose!"
    Draw -> "It's a draw!"

playRound :: StateT Scoreboard IO (Maybe Outcome)
playRound =
  StateT $ \s -> do
    opponent <- randomIO
    putStrLn "Type your choice!"
    maybeChoice <- fmap readMaybe getLine
    case maybeChoice of
      Just player -> do
        let outcome = calculateOutcome opponent player
        putStrLn $
          "Your opponent picked " ++
          (show opponent) ++ ". " ++ (friendly outcome)
        return (Just outcome, updateScoreboard s outcome)
      Nothing -> do
        putStrLn "Invalid choice. Try Rock, Paper or Scissors"
        return (Nothing, s)

playForever :: IO ([Maybe Outcome], Scoreboard)
playForever = runStateT (forever playRound) newScoreboard

newGame :: Int -> IO ()
newGame rounds = do
  (_, s) <- runStateT (replicateM rounds playRound) newScoreboard
  putStrLn $ "Game over! Final score: " ++ show s
