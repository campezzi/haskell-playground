module MonadPreamble where

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' = putStrLn "blah" >> putStrLn "another thing"

binding :: IO ()
binding = do
  name' <- getLine
  putStrLn name'

binding' :: IO ()
binding' = getLine >>= putStrLn

bas :: IO ()
bas = do
  putStrLn "name:"
  name' <- getLine
  putStrLn ("hello " ++ name')

bas' :: IO ()
bas' = putStrLn "name:" >> getLine >>= \name' -> putStrLn ("hello" ++ name')

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

--
data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
       then Nothing
       else Just c

mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name' age' weight' = do
  n <- noEmpty name'
  a <- noNegative age'
  w <- noNegative weight'
  weightCheck (Cow n a w)
