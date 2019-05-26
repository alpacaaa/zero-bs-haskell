{-# LANGUAGE DuplicateRecordFields #-}
module TodoMVC.Backend where

import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.List as List
-- import qualified Data.Map.Strict as Map
import qualified Zero.Server as Server

data InputTodo
  = InputTodo
      { title :: String
      }
  deriving (Eq, Show, Generic, Aeson.FromJSON)

data Todo
  = Todo
      { title :: String
      , completed :: Bool
      , url :: String
      , todoId :: String
      }
  deriving (Eq, Show, Generic, Aeson.ToJSON)

data TodoState
  = TodoState
      { todos :: [Todo]
      , nextIndex :: Int
      }
  deriving (Eq, Show)

initialState :: TodoState
initialState
  = TodoState
      { todos = []
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
          ]
      ]

  where
    getTodos state _
      = (state, Server.jsonResponse $ todos state)

    create newId input
      = Todo
          { title = title (input :: InputTodo)
          , completed = False
          , url = "http://localhost:7879/api/" ++ newId
          , todoId = newId
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
          newTodo = create (show index) input
          newState
            = TodoState
                { todos = (todos state) ++ [newTodo]
                , nextIndex = index + 1
                }
        in (newState, newTodo)

    getSingle state req
      = case findTodo "id" state req of
          Right todo -> (state, Server.jsonResponse todo)
          Left err -> (state, Server.failureResponse err)

    -- TODO update state blah blah
    patchTodo state _
      = (state, Server.stringResponse "wtf")

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
