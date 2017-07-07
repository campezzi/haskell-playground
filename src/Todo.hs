{-# LANGUAGE DeriveFunctor #-}

module Todo where

import Control.Monad.Free
import Control.Monad.Trans.State
import Data.Map (Map, delete, empty, insert, lookup)
import Prelude hiding (lookup)

-- define a data type to describe possible domain actions
data TodoActionF a
  = Add String
        a
  | Query String
          (Bool -> a)
  | Complete String
             a
  | Remove String
           a
  deriving (Functor) -- free monads can only be made from functors

-- make a free monad from domain actions
type TodoAction = Free TodoActionF

-- dsl to create composable domain actions
add :: String -> TodoAction ()
add todo = liftF $ Add todo ()

query :: String -> TodoAction Bool
query todo = liftF $ Query todo id

complete :: String -> TodoAction ()
complete todo = liftF $ Complete todo ()

remove :: String -> TodoAction ()
remove todo = liftF $ Remove todo ()

-- type that will be used to hold state
type TodoList = Map String Bool

-- interpreter from TodoAction to State TodoList
toTodoList :: TodoAction a -> State TodoList a
toTodoList action =
  case action of
    Free (Add title next) ->
      (state $ \s -> ((), insert title False s)) >> toTodoList next
    Free (Query title next) -> do
      isDone <- state $ \s -> (maybe False id (lookup title s), s)
      toTodoList (next isDone)
    Free (Complete title next) ->
      (state $ \s -> ((), insert title True s)) >> toTodoList next
    Free (Remove title next) ->
      (state $ \s -> ((), delete title s)) >> toTodoList next
    Pure a -> return a

toIO :: TodoAction a -> IO ()
toIO action =
  case action of
    Free (Add title next) -> putStrLn ("added " ++ title) >> toIO next
    Free (Query title next) ->
      putStrLn ("queried " ++ title) >> toIO (next False)
    Free (Complete title next) -> putStrLn ("completed " ++ title) >> toIO next
    Free (Remove title next) -> putStrLn ("removed " ++ title) >> toIO next
    Pure _ -> putStrLn "all done"

-- test data
addSomeTodos :: TodoAction ()
addSomeTodos = do
  add "Do something"
  add "Other thing"
  add "So many things to do"
  complete "Other thing"
  remove "Do something"

testWithState :: ((), TodoList)
testWithState = runState (toTodoList addSomeTodos) empty

testWithIO :: IO ()
testWithIO = toIO addSomeTodos
