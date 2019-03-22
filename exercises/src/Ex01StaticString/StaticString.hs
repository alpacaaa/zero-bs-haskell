module Ex01StaticString.StaticString where

import qualified Zero.Server as Server


helloHandler :: Server.Request -> Server.Response
helloHandler _
  = Server.stringResponse "hello"

main :: IO ()
main
  = Server.startServer
      [ Server.simpleHandler Server.GET "/hello" helloHandler
      ]
