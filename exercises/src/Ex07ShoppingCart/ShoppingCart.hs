module Ex07ShoppingCart.ShoppingCart where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Zero.Server as Server

data Item
  = Item { model :: String, quantity :: Int }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Cart
  = Cart [Item]
  deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)


addToCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
addToCartHandler (Cart cart) req
  = case Server.decodeJson body of
      Right item ->
        (appendToCart item, Server.stringResponse "ok")
      Left err ->
        (Cart cart, Server.failureResponse $ "Got an error! " ++ show err)
  where
    body
      = Server.requestBody req

    appendToCart item
      = Cart (cart ++ [item]) -- append item at the end of the list

currentCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
currentCartHandler state _
  = (state, response)
  where
    -- Cart current
    --   = state

    response
      = Server.jsonResponse (state)

initialState :: Cart
initialState = Cart []


main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState initialState
          [ Server.statefulHandler Server.POST "/cart" addToCartHandler
          , Server.statefulHandler Server.GET  "/cart" currentCartHandler
          ]
      ]
