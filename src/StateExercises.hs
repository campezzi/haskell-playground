module StateExercises where

import Control.Applicative (liftA3)
import System.Random

newtype MyState s a = MyState
  { runMyState :: s -> (a, s)
  }

-- (a -> b) -> MyState s a -> MyState s b
-- (a -> b) -> MyState (s -> (a, s)) -> MyState (s -> (b, s))
instance Functor (MyState s) where
  fmap f (MyState fs) =
    MyState $ \s ->
      let (a, s') = fs s
      in (f a, s')

-- MyState s (a -> b) -> MyState s a -> MyState s b
-- MyState (s -> (a -> b, s)) -> MyState (s -> (a, s)) -> MyState (s -> (b, s))
instance Applicative (MyState s) where
  pure a = MyState $ \s -> (a, s)
  MyState fsa <*> MyState fs =
    MyState $ \s ->
      let (fa, s') = fsa s
          (a, s'') = fs s'
      in (fa a, s'')

-- MyState s (a -> b) -> (a -> MyState s b) -> MyState s b
-- MyState (s -> (a, s)) -> (a -> MyState (s -> (b, s))) -> MyState (s -> (b, s))
instance Monad (MyState s) where
  return = pure
  MyState fs >>= f =
    MyState $ \s ->
      let (a, s') = fs s
      in runMyState (f a) s'

get :: MyState s s
get = MyState $ \s -> (s, s)

put :: s -> MyState s ()
put s = MyState $ \_ -> ((), s)

exec :: MyState s a -> s -> s
exec (MyState f) = snd . f

eval :: MyState s a -> s -> a
eval (MyState f) = fst . f

modify :: (s -> s) -> MyState s ()
modify f = MyState $ \s -> ((), f s)

--
data RollHistory = RollHistory
  { dice :: [Int]
  , total :: Int
  , gen :: StdGen
  } deriving (Show)

emptyHistory :: Int -> RollHistory
emptyHistory seed = RollHistory [] 0 (mkStdGen seed)

roll :: MyState RollHistory Int
roll =
  MyState $ \s ->
    let (value, nextGen) = randomR (1, 6) (gen s)
        s' = RollHistory (value : (dice s)) (value + (total s)) nextGen
    in (value, s')

rollsToGetTo :: Int -> MyState RollHistory Int
rollsToGetTo target =
  MyState $ \s ->
    let s' = go s
        go history
          | total history >= target = history
          | otherwise = go $ exec roll history
    in (length $ dice s', s')

chainedRolls :: MyState RollHistory (Int, Int, Int)
chainedRolls = do
  thirty <- rollsToGetTo 30
  fifty <- rollsToGetTo 50
  hundred <- rollsToGetTo 100
  return (thirty, fifty, hundred)

chainedRolls' :: MyState RollHistory (Int, Int, Int)
chainedRolls' =
  liftA3 (,,) (rollsToGetTo 30) (rollsToGetTo 50) (rollsToGetTo 100)
