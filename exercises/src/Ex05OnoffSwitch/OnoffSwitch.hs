module Ex05OnoffSwitch.OnoffSwitch where

import qualified Zero.Server as Server

data State
  = On
  | Off
  deriving (Eq, Show)


switchHandler :: State -> Server.Request -> (State, Server.Response)
switchHandler currentState _
  = (newState, response)

  where
    newState
      = case currentState of
          On  -> Off
          Off -> On

    response
      = Server.stringResponse (show newState)


initialState :: State
initialState = Off


main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState initialState
          [ Server.statefulHandler Server.POST "/onoff-switch" switchHandler
          ]
      ]
