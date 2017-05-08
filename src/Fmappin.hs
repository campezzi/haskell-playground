module Fmappin where

a :: [Int]
a = fmap (+ 1) $ read "[1]" :: [Int]

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Int -> Int
c = fmap (* 2) (\x -> x - 2)

d :: Int -> String
d = fmap ((return '1' ++) . show) (\x -> [x,1 .. 3])

e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      shown = fmap show ioi -- IO String
      changed = fmap (read . ("123" ++)) shown
  in fmap (* 3) changed

fmappinDemo :: IO ()
fmappinDemo = do
  putStrLn $ show a
  putStrLn $ show b
  putStrLn $ show (c 1)
  putStrLn $ show (d 0)
  e' <- fmap show e
  putStrLn e'
