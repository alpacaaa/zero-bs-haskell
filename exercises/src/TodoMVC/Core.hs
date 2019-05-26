module TodoMVC.Core where

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)

import qualified TodoMVC.Input as Input

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
      { todos     :: Map String Todo
      , nextIndex :: Int
      }
  deriving (Eq, Show)

initialState :: State
initialState
  = State
      { todos     = Map.empty
      , nextIndex = 1
      }

create :: String -> Input.InputTodo -> Todo
create newId input
  = Todo
      { title     = fromMaybe "" $ Input.title input
      , completed = False
      , url       = "http://localhost:7879/api/" ++ newId
      , todoId    = newId
      , order     = Input.order input
      }

addTodo :: State -> Input.InputTodo -> (State, Todo)
addTodo state input
  = (newState, newTodo)
  where
    index
      = nextIndex state
    tId
      = show index
    newTodo
      = create tId input
    newState
      = State
          { todos = Map.insert tId newTodo (todos state)
          , nextIndex = index + 1
          }

updateTodo :: State -> Todo -> Input.InputTodo -> (State, Todo)
updateTodo state existing input
  = (newState, updated)
  where
    updated
      = existing
          { title = fromMaybe (title existing) $ Input.title input
          , completed = fromMaybe (completed existing) $ Input.completed input
          , order = Input.order input
          }

    newState
      = state
          { todos = Map.insert (todoId existing) updated (todos state) }

deleteTodo :: State -> Todo -> State
deleteTodo state todo
  = state { todos = Map.delete (todoId todo) (todos state) }

findTodo :: State -> String -> Maybe Todo
findTodo state tId
  = Map.lookup tId (todos state)

stateToList :: State -> [Todo]
stateToList state
  = snd <$> Map.toList (todos state)
