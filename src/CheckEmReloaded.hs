module CheckEmReloaded where

import ApplyYoself
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

checkEmReloaded :: IO ()
checkEmReloaded = do
  putStrLn "Checking AIdentity..."
  quickBatch $ applicative identityTrigger
  putStrLn "Checking AConstant..."
  quickBatch $ applicative constantTrigger
  where
    identityTrigger = undefined :: AIdentity (String, String, Int)
    constantTrigger =
      undefined :: AConstant (String, String, [Int]) (String, String, [Int])
