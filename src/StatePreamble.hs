module StatePreamble where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie =
  state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  (intToDie d1, intToDie d2, intToDie d3)
  where
    s = mkStdGen 0
    (d1, s1) = randomR (1, 6) s
    (d2, s2) = randomR (1, 6) s1
    (d3, _) = randomR (1, 6) s2

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

rollsToGetToTwenty :: StdGen -> Int
rollsToGetToTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go total count gen
      | total >= 20 = count
      | otherwise = go (total + die) (count + 1) nextG
      where
        (die, nextG) = randomR (1, 6) gen

-- TODO: refactor to use State?
rollsToGetToN :: Int -> StdGen -> (Int, [Die])
rollsToGetToN target g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go total count dice gen
      | total >= target = (count, dice)
      | otherwise = go (total + die) (count + 1) allDice nextG
      where
        (die, nextG) = randomR (1, 6) gen
        allDice = (:) (intToDie die) dice
