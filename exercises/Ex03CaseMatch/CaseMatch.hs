module Ex03CaseMatch.CaseMatch where

import qualified Lib


caseHandler :: Lib.Request -> Lib.Response
caseHandler req
  = Lib.stringResponse res
  where
    res
      = case Lib.requestBody req of
          "1" -> "one"
          "2" -> "two"
          "3" -> "three"
          _   -> "What am I, a mathematician?"

main :: IO ()
main
  = Lib.startServer
      [ Lib.simpleHandler Lib.MethodPOST "/case" caseHandler
      ]
