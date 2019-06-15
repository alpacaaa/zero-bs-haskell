module TodoMVC.Core where

-- import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map


data Todo
  = Todo
      { title     :: String
      , completed :: Bool
      , url       :: String
      , todoId    :: String
      , order     :: Maybe Int
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data State
  = State
      { todos :: Map String Todo
      , nextIndex :: Int
      }
  deriving (Eq, Show)

initialState :: State
initialState
  = State
      { todos = Map.empty
      , nextIndex = 1
      }

createTodo :: State -> String -> Maybe Int -> (State, Todo)
createTodo state todoTitle todoOrder
  = (newState, newTodo)
  where
    index
      = nextIndex state
    tId
      = show index
    newTodo
      = Todo
          { title = todoTitle
          , completed = False
          , url = "http://localhost:7879/api/" ++ tId
          , todoId = tId
          , order = todoOrder
          }
    newState
      = State
          { todos = Map.insert tId newTodo (todos state)
          , nextIndex = index + 1
          }

updateTodo :: State -> Todo -> Maybe String -> Maybe Bool -> Maybe Int -> (State, Todo)
updateTodo state existing todoTitle todoCompleted todoOrder
  = (newState, updated)
  where
    newState
      = state
          { todos = Map.insert (todoId existing) updated (todos state) }

    updated
      = existing
          { title = fromMaybe (title existing) todoTitle
          , completed = fromMaybe (completed existing) todoCompleted
          , order = todoOrder
          }

deleteTodo :: State -> Todo -> State
deleteTodo state todo
  = state { todos = Map.delete (todoId todo) (todos state) }

findTodo :: State -> String -> Maybe Todo
findTodo state tId
  = Map.lookup tId (todos state)

stateToList :: State -> [Todo]
stateToList state
  = snd <$> Map.toList (todos state)
