module TodoMVC.HTTP where

import qualified Zero.Server as Server

type HttpHandler state
  =  String
  -> (state -> Server.Request -> (state, Server.Response))
  -> Server.StatefulHandler state

get :: HttpHandler state
get = Server.statefulHandler Server.GET

post :: HttpHandler state
post = Server.statefulHandler Server.POST

patch :: HttpHandler state
patch = Server.statefulHandler Server.PATCH

delete :: HttpHandler state
delete = Server.statefulHandler Server.DELETE

startServer :: state -> [Server.StatefulHandler state] -> IO ()
startServer initialState handlers
  = Server.startServer
      [ Server.handlersWithState initialState handlers
      ]
