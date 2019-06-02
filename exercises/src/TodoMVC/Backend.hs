{-
  Integration tests:
  https://www.todobackend.com/specs/?http://localhost:7879/api
-}
module TodoMVC.Backend where

import qualified TodoMVC.Core as Core
import qualified TodoMVC.Partial as Partial

import qualified Data.List as List
import qualified Zero.Server as Server

main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState Core.initialState
          [ Server.statefulHandler Server.GET    "/api" getAll
          , Server.statefulHandler Server.POST   "/api" postTodo
          , Server.statefulHandler Server.DELETE "/api" deleteAll
          , Server.statefulHandler Server.GET    "/api/:id" getTodo
          , Server.statefulHandler Server.PATCH  "/api/:id" patchTodo
          , Server.statefulHandler Server.DELETE "/api/:id" deleteTodo
          ]
      ]

  where
    getAll state _
      = (state, Server.jsonResponse $ Core.stateToList state)

    postTodo state req
      = decodeInputOrFail state req $ \input ->
          let (newState, newTodo) = Core.createTodo state input
          in (newState, Server.jsonResponse newTodo)

    deleteAll _ _
      = (Core.initialState, Server.stringResponse "ok")

    getTodo state req
      = findTodoOrFail state req $ \todo ->
          (state, Server.jsonResponse todo)

    patchTodo state req
      = findTodoOrFail state req $ \existing ->
          decodeInputOrFail state req $ \input ->
            let (newState, updated) = Core.updateTodo state existing input
            in (newState, Server.jsonResponse updated)

    deleteTodo state req
      = findTodoOrFail state req $ \todo ->
          (Core.deleteTodo state todo, Server.stringResponse "ok")

findTodoOrFail
  :: Core.State
  -> Server.Request
  -> (Core.Todo -> (Core.State, Server.Response))
  -> (Core.State, Server.Response)
findTodoOrFail state req cb
  = case maybeUrlId of
      Nothing ->
        (state, Server.failureResponse "No :id found in URL")
      Just (_, tId) ->
        case Core.findTodo state tId of
          Nothing ->
            (state, Server.failureResponse "Todo not found")
          Just todo ->
            cb todo
  where
    maybeUrlId
      = List.find
          (\(k, _) -> k == "id")
          (Server.requestParams req)

decodeInputOrFail
  :: Core.State
  -> Server.Request
  -> (Partial.PartialTodo -> (Core.State, Server.Response))
  -> (Core.State, Server.Response)
decodeInputOrFail state req cb
  = case Server.decodeJson (Server.requestBody req) of
      Left err ->
        (state, Server.failureResponse err)
      Right input ->
        cb input
