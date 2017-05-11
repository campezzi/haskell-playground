module Funky where

-- functions to prove functor laws
verifyIdentity
  :: (Functor f, Eq (f a))
  => f a -> (Bool, f a)
verifyIdentity f = (valid, result)
  where
    result = fmap id f
    valid = f == result

verifyComposition
  :: (Num a, Functor f, Eq (f a))
  => f a -> (Bool, f a, f a)
verifyComposition x = (valid, separate, fused)
  where
    f = (+ 1)
    g = (* 2)
    separate = (fmap g) . (fmap f) $ x
    fused = fmap (g . f) x
    valid = separate == fused

-- Identity functor
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- Pair functor
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- Two functor
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- Three functor
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- Three' functor
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

-- Four functor
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- Four' functor
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

-- Possibly functor
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Sum functor
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- Wrap functor
data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f =>
         Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- Parappa functor
data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- output
verifyAll :: IO ()
verifyAll = do
  verifyFunctor (Identity 1 :: Identity Integer) "Identity"
  verifyFunctor (Pair 1 1 :: Pair Integer) "Pair"
  verifyFunctor (Two 0 1 :: Two Integer Integer) "Two"
  verifyFunctor (Three 0 0 1 :: Three Integer Integer Integer) "Three"
  verifyFunctor (Three' 0 1 1 :: Three' Integer Integer) "Three'"
  verifyFunctor (Four 0 0 0 1 :: Four Integer Integer Integer Integer) "Four"
  verifyFunctor (Four' 0 0 0 1 :: Four' Integer Integer) "Four"
  verifyFunctor (Yeppers 1 :: Possibly Integer) "Possibly (with value)"
  verifyFunctor (LolNope :: Possibly Integer) "Possibly (without value)"
  verifyFunctor (First 1 :: Sum Integer Integer) "Sum (with left value)"
  verifyFunctor (Second 1 :: Sum Integer Integer) "Sum (with right value)"
  verifyFunctor (Wrap (Identity 1) :: Wrap Identity Integer) "Wrap"
  verifyFunctor
    (DaWrappa (Identity 1) (Identity 1) :: Parappa Identity Identity Integer)
    "Parappa"

verifyFunctor
  :: (Functor f, Num a, Show (f a), Eq (f a))
  => f a -> String -> IO ()
verifyFunctor f name = do
  putStrLn $ "--- " ++ name ++ " ---"
  putStrLn $ "identity: " ++ (show $ verifyIdentity f)
  putStrLn $ "composition: " ++ (show $ verifyComposition f)
  putStrLn ""
