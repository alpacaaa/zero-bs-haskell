module Ex01StaticString.StaticString where

import qualified Lib


helloHandler :: Lib.Request -> Lib.Response
helloHandler _
  = Lib.stringResponse "hello"

main :: IO ()
main
  = Lib.startServer
      [ Lib.simpleHandler Lib.MethodGET "/hello" helloHandler
      ]
