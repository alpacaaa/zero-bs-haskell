module TodoMVC.Backend where

import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import qualified TodoMVC.Input as Input

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Zero.Server as Server

data Todo
  = Todo
      { title :: String
      , completed :: Bool
      , url :: String
      , todoId :: String
      , order :: Maybe Int
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data TodoState
  = TodoState
      { todos :: Map String Todo
      , nextIndex :: Int
      }
  deriving (Eq, Show)

initialState :: TodoState
initialState
  = TodoState
      { todos = Map.empty
      , nextIndex = 1
      }

main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState initialState
          [ Server.statefulHandler Server.GET "/api" getTodos
          , Server.statefulHandler Server.POST "/api" postTodo
          , Server.statefulHandler Server.DELETE "/api" deleteAll
          , Server.statefulHandler Server.GET "/api/:id" getSingle
          , Server.statefulHandler Server.PATCH "/api/:id" patchTodo
          , Server.statefulHandler Server.DELETE "/api/:id" deleteTodo
          ]
      ]

  where
    getTodos state _
      = (state, Server.jsonResponse $ stateToList state)

    create newId input
      = Todo
          { title = fromMaybe "" $ Input.title input
          , completed = False
          , url = "http://localhost:7879/api/" ++ newId
          , todoId = newId
          , order = Input.order input
          }

    postTodo state req
      = case Server.decodeJson (Server.requestBody req) of
          Right input ->
            let (newState, newTodo) = addTodo state input
            in (newState, Server.jsonResponse newTodo)
          Left err ->
            (state, Server.failureResponse $ show err)

    deleteAll _ _
      = (initialState, Server.stringResponse "ok")

    addTodo state input
      = let
          index = nextIndex state
          tId = show index
          newTodo = create tId input
          newState
            = TodoState
                { todos = Map.insert tId newTodo (todos state)
                , nextIndex = index + 1
                }
        in (newState, newTodo)

    getSingle state req
      = case findTodo "id" state req of
          Right todo -> (state, Server.jsonResponse todo)
          Left err -> (state, Server.failureResponse err)

    patchTodo state req
      = case updateTodo state req of
          Right (newState, todo) -> (newState, Server.jsonResponse todo)
          Left err -> (state, Server.failureResponse err)

    updateTodo state req = do
      input <- Server.decodeJson (Server.requestBody req)
      existing <- findTodo "id" state req
      let updated
            = existing
                { title = fromMaybe (title existing) $ Input.title input
                , completed = fromMaybe (completed existing) $ Input.completed input
                , order = Input.order input
                }

      pure
        ( state { todos = Map.insert (todoId existing) updated (todos state) }
        , updated
        )

    deleteTodo state req
      = let update todo
              = state { todos = Map.delete (todoId todo) (todos state) }
        in case findTodo "id" state req of
            Right todo -> (update todo, Server.stringResponse "ok")
            Left err -> (state, Server.failureResponse err)

findTodo :: String -> TodoState -> Server.Request -> Either String Todo
findTodo urlVar state req
  = case maybeUrlId of
      Nothing ->
        Left "No ID found in URL"
      Just (_, tId) ->
        case findTodoInState state tId of
          Nothing -> Left "Todo not found"
          Just todo -> Right todo
  where
    maybeUrlId
      = List.find
          (\(k, _) -> k == urlVar)
          (Server.requestParams req)

findTodoInState :: TodoState -> String -> Maybe Todo
findTodoInState state tId
  = List.find
      (\todo -> todoId todo == tId)
      (todos state)

stateToList :: TodoState -> [Todo]
stateToList state
  = snd <$> Map.toList (todos state)
