module Ex02Echo.Echo where

import qualified Zero.Server as Server

-- curl http://localhost:7879/echo -H "Content-Type: application/json" --data "[]"


echoHandler :: Server.Request -> Server.Response
echoHandler req
  = Server.stringResponse (Server.requestBody req)

main :: IO ()
main
  = Server.startServer
      [ Server.simpleHandler Server.POST "/echo" echoHandler
      ]
