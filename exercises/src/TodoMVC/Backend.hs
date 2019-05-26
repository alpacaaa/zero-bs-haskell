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
      { title     :: String
      , completed :: Bool
      , url       :: String
      , todoId    :: String
      , order     :: Maybe Int
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data TodoState
  = TodoState
      { todos     :: Map String Todo
      , nextIndex :: Int
      }
  deriving (Eq, Show)

initialState :: TodoState
initialState
  = TodoState
      { todos     = Map.empty
      , nextIndex = 1
      }

main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState initialState
          [ Server.statefulHandler Server.GET    "/api" getTodos
          , Server.statefulHandler Server.POST   "/api" postTodo
          , Server.statefulHandler Server.DELETE "/api" deleteAll
          , Server.statefulHandler Server.GET    "/api/:id" getSingle
          , Server.statefulHandler Server.PATCH  "/api/:id" patchTodo
          , Server.statefulHandler Server.DELETE "/api/:id" deleteTodo
          ]
      ]

  where
    getTodos state _
      = (state, Server.jsonResponse $ stateToList state)

    create newId input
      = Todo
          { title     = fromMaybe "" $ Input.title input
          , completed = False
          , url       = "http://localhost:7879/api/" ++ newId
          , todoId    = newId
          , order     = Input.order input
          }

    postTodo state req
      = decodeInputOrFail state req $ \input ->
          let (newState, newTodo) = addTodo state input
          in (newState, Server.jsonResponse newTodo)

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
      = findTodoOrFail state req $ \todo ->
          (state, Server.jsonResponse todo)

    patchTodo state req
      = findTodoOrFail state req $ \existing ->
          decodeInputOrFail state req $ \input ->
            let updated
                  = existing
                      { title = fromMaybe (title existing) $ Input.title input
                      , completed = fromMaybe (completed existing) $ Input.completed input
                      , order = Input.order input
                      }

            in
              ( state { todos = Map.insert (todoId existing) updated (todos state) }
              , Server.jsonResponse updated
              )

    deleteTodo state req
      = findTodoOrFail state req $ \todo ->
          let updated
                = state { todos = Map.delete (todoId todo) (todos state) }
          in (updated, Server.stringResponse "ok")

findTodoOrFail
  :: TodoState
  -> Server.Request
  -> (Todo -> (TodoState, Server.Response))
  -> (TodoState, Server.Response)
findTodoOrFail state req cb
  = case maybeUrlId of
      Nothing ->
        (state, Server.failureResponse "No ID found in URL")
      Just (_, tId) ->
        case findTodoInState state tId of
          Nothing ->
            (state, Server.failureResponse "Todo not found")
          Just todo ->
            cb todo
  where
    maybeUrlId
      = List.find
          (\(k, _) -> k == "id")
          (Server.requestParams req)

findTodoInState :: TodoState -> String -> Maybe Todo
findTodoInState state tId
  = Map.lookup tId (todos state)

stateToList :: TodoState -> [Todo]
stateToList state
  = snd <$> Map.toList (todos state)

decodeInputOrFail
  :: TodoState
  -> Server.Request
  -> (Input.InputTodo -> (TodoState, Server.Response))
  -> (TodoState, Server.Response)
decodeInputOrFail state req cb
  = case Server.decodeJson (Server.requestBody req) of
      Left err ->
        (state, Server.failureResponse $ show err)
      Right input ->
        cb input
