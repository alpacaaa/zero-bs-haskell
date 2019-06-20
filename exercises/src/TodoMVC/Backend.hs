{-
  Integration tests:
  https://www.todobackend.com/specs/?http://localhost:7879/api
-}
module TodoMVC.Backend where

import TodoMVC.Input

import qualified TodoMVC.Core as Core
import qualified TodoMVC.HTTP as HTTP
import qualified Zero.Server as Server

type Handler = Core.State -> Server.Request -> (Core.State, Server.Response)

main :: IO ()
main
  = HTTP.startServer Core.initialState
      [ HTTP.get    "/api" getAll
      , HTTP.post   "/api" postTodo
      , HTTP.delete "/api" deleteAll
      , HTTP.get    "/api/:id" getTodo
      , HTTP.patch  "/api/:id" patchTodo
      , HTTP.delete "/api/:id" deleteTodo
      ]

getAll :: Handler
getAll state _
  = (state, Server.jsonResponse $ Core.stateToList state)

postTodo :: Handler
postTodo state req
  = decodeInputOrFail state req $ \input ->
      case title input of
        Nothing ->
          (state, Server.failureResponse "Empty title")
        Just todoTitle ->
          let (newState, newTodo)
                = Core.createTodo
                    state
                    todoTitle
                    (order input)
          in (newState, Server.jsonResponse newTodo)

deleteAll :: Handler
deleteAll _ _
  = (Core.initialState, Server.stringResponse "ok")

getTodo :: Handler
getTodo state req
  = findTodoOrFail state req $ \todo ->
      (state, Server.jsonResponse todo)

patchTodo :: Handler
patchTodo state req
  = findTodoOrFail state req $ \existing ->
      decodeInputOrFail state req $ \input ->
        let (newState, updated) = Core.updateTodo
              state
              existing
              (title input)
              (completed input)
              (order input)
        in (newState, Server.jsonResponse updated)

deleteTodo:: Handler
deleteTodo state req
  = findTodoOrFail state req $ \todo ->
      (Core.deleteTodo state todo, Server.stringResponse "ok")

findTodoOrFail
  :: Core.State
  -> Server.Request
  -> (Core.Todo -> (Core.State, Server.Response))
  -> (Core.State, Server.Response)
findTodoOrFail state req cb
  = case Server.requestParameter "id" req of
      Nothing ->
        (state, Server.failureResponse "No :id found in URL")
      Just tId ->
        case Core.findTodo state tId of
          Nothing ->
            (state, Server.failureResponse "Todo not found")
          Just todo ->
            cb todo

decodeInputOrFail
  :: Core.State
  -> Server.Request
  -> (Input -> (Core.State, Server.Response))
  -> (Core.State, Server.Response)
decodeInputOrFail state req cb
  = case Server.decodeJson (Server.requestBody req) of
      Left err ->
        (state, Server.failureResponse err)
      Right input ->
        cb input
