-- | Here we define the business logic for our application.
-- We model our domain by defining a `Todo` and `State` types.
-- Functions are completely self contained and super easy to test, there is
-- no mention of an HTTP api or databases. Everything in this module is highly
-- testable and represents the core of the application.
module TodoMVC.Core where

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)

import qualified TodoMVC.Partial as Partial

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map

-- | Note how we only define a `ToJSON` instance here, to make it explicit that
--   values of this type cannot be read directly from a request. The reverse is
--   true for the `PartialTodo` type, that should only be used to get a partial
--   structure out of an HTTP request. A `PartialTodo` can then be used to
--   update an existing `Todo`, by updating only the fields that are non-null.
data Todo
  = Todo
      { title     :: String
      , completed :: Bool
      , url       :: String
      , todoId    :: String
      , order     :: Maybe Int
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

-- | Todos are stored in a map indexed by Todo id.
--   The next Todo id is tracked by `nextIndex` and incremented everytime
--   a new Todo is added to the `todos` map.
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

todoFromPartial :: String -> Partial.PartialTodo -> Todo
todoFromPartial tId input
  = Todo
      { title     = fromMaybe "" (Partial.title input)
      , completed = False
      , url       = "http://localhost:7879/api/" ++ tId
      , todoId    = tId
      , order     = Partial.order input
      }

createTodo :: State -> Partial.PartialTodo -> (State, Todo)
createTodo state input
  = (newState, newTodo)
  where
    index
      = nextIndex state
    tId
      = show index
    newTodo
      = todoFromPartial tId input
    newState
      = State
          { todos = Map.insert tId newTodo (todos state)
          , nextIndex = index + 1
          }

updateTodo :: State -> Todo -> Partial.PartialTodo -> (State, Todo)
updateTodo state existing input
  = (newState, updated)
  where
    newState
      = state
          { todos = Map.insert (todoId existing) updated (todos state) }

    updated
      = existing
          { title = fromMaybe (title existing) (Partial.title input)
          , completed = fromMaybe (completed existing) (Partial.completed input)
          , order = Partial.order input
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
