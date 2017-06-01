module StateExercises where

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
