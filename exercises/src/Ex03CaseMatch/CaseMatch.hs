module Ex03CaseMatch.CaseMatch where

import qualified Zero.Server as Server


caseHandler :: Server.Request -> Server.Response
caseHandler req
  = Server.stringResponse res
  where
    res
      = case Server.requestBody req of
          "1" -> "one"
          "2" -> "two"
          "3" -> "three"
          _   -> "What am I, a mathematician?"

main :: IO ()
main
  = Server.startServer
      [ Server.simpleHandler Server.POST "/case" caseHandler
      ]
