module Ex02Echo.Echo where

import qualified Lib

-- curl http://localhost:7879/echo -H "Content-Type: application/json" --data "[]"


echoHandler :: Lib.Request -> Lib.Response
echoHandler req
  = Lib.okResponse (Lib.requestBody req)

main :: IO ()
main
  = Lib.startServer
      [ Lib.simpleHandler Lib.MethodPOST "/echo" echoHandler
      ]
