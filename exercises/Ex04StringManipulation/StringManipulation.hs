module Ex04StringManipulation.StringManipulation where

import qualified Data.List as List
import qualified Zero.Server as Server


stringManipulationHandler :: Server.Request -> Server.Response
stringManipulationHandler req
  = Server.stringResponse res

  where
    search
      = "I'm positive"

    len
      = List.length search

    body
      = Server.requestBody req

    res
      = if List.take len body == search
          then "I think" <> List.drop len body
          else body

main :: IO ()
main
  = Server.startServer
      [ Server.simpleHandler Server.POST "/string-manipulation" stringManipulationHandler
      ]
