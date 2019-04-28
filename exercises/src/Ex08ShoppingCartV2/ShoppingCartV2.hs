module Ex08ShoppingCartV2.ShoppingCartV2 where

import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Zero.Server as Server


data Item
  = Item { model :: String, quantity :: Int }
  deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Cart
  = Cart (Map String Item)
  deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)


addToCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
addToCartHandler cart req
  = case Server.decodeJson body of
      Right item ->
        (appendToCart item cart, Server.stringResponse "ok")
      Left err ->
        (cart, Server.failureResponse $ "Got an error! " ++ show err)
  where
    body
      = Server.requestBody req

appendToCart :: Item -> Cart -> Cart
appendToCart item (Cart cart)
  = Cart newCart
  where
    newCart
      = case Map.lookup key cart of
          Just existing ->
            let
              newItem
                = existing { quantity = (quantity existing) + (quantity item) }
            in
            Map.insert key newItem cart

          Nothing ->
            Map.insert key item cart
    key
      = model item

currentCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
currentCartHandler state@(Cart cart) _
  = (state, response)
  where
    response
      = Server.jsonResponse $ sortOnQuantity (Map.elems cart)

    sortOnQuantity items
      -- `sortOn` orders elements from lowest to highest, so we need to `reverse`
      = List.reverse (List.sortOn quantity items)

initialState :: Cart
initialState = Cart Map.empty


main :: IO ()
main
  = Server.startServer
      [ Server.handlersWithState initialState
          [ Server.statefulHandler Server.POST "/cart" addToCartHandler
          , Server.statefulHandler Server.GET  "/cart" currentCartHandler
          ]
      ]
