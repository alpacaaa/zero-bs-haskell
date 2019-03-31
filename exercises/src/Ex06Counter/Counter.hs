module Ex06Counter.Counter where

import qualified Zero.Server as Server

data Counter
  = Counter Int
  deriving (Eq, Show)


increaseCounterHandler :: Counter -> Server.Request -> (Counter, Server.Response)
increaseCounterHandler (Counter current) _
  = (newState, response)
  where
    newState
      = Counter (current + 1)

    response
      = Server.stringResponse "ok"

currentCountHandler :: Counter -> Server.Request -> (Counter, Server.Response)
currentCountHandler state _
  = (state, response)
  where
    Counter current
      = state

    response
      = Server.stringResponse (show current)

initialState :: Counter
initialState = Counter 0


main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState initialState
          [ Server.statefulHandler Server.POST "/increase" increaseCounterHandler
          , Server.statefulHandler Server.GET  "/current-count" currentCountHandler
          ]
      ]
