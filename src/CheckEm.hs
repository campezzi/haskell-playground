module CheckEm where

import Test.QuickCheck

half :: Double -> Double
half = (/ 2)

halfIdentity :: Double -> Bool
halfIdentity x = (half x) * 2 == x

checkEm :: IO ()
checkEm = do
  putStrLn "Double of half a number equals the number"
  quickCheck halfIdentity
