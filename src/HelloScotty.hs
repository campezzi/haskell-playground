{-# LANGUAGE OverloadedStrings #-}

module HelloScotty where

import Control.Monad.Trans.Class
import Data.Monoid (mconcat)
import Web.Scotty

main =
  scotty 3000 $ do
    get "/:name" $ do
      name <- param "name"
      lift $ putStrLn "Hello!"
      html $ mconcat ["hello, ", name, "!"]
