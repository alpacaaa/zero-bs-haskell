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
createTodo state todoTitle todoOrder =
  let index = nextIndex state
      tId = show index
      newTodo = Todo
        { title = todoTitle
        , completed = False
        , url = "http://localhost:7879/api/" ++ tId
        , todoId = tId
        , order = todoOrder
        }
      newState = State
        { todos = Map.insert tId newTodo (todos state)
        , nextIndex = index + 1
        }
  in (newState, newTodo)

updateTodo
  :: State
  -> Todo
  -> Maybe String
  -> Maybe Bool
  -> Maybe Int
  -> (State, Todo)
updateTodo state existing newTitle newCompleted newOrder =
  let
    updated
      = existing
          { title = withDefault (title existing) newTitle
          , completed = withDefault (completed existing) newCompleted
          , order = newOrder
          }

    newState
      = state
          { todos = Map.insert (todoId existing) updated (todos state) }

  in (newState, updated)

deleteTodo :: State -> Todo -> State
deleteTodo state todo
  = state { todos = Map.delete (todoId todo) (todos state) }

findTodo :: State -> String -> Maybe Todo
findTodo state tId
  = Map.lookup tId (todos state)

stateToList :: State -> [Todo]
stateToList state
  = snd <$> Map.toList (todos state)

-- Forgive me Padre
withDefault :: a -> Maybe a -> a
withDefault = fromMaybe
