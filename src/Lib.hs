{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import Control.Arrow (left)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.Wai as Wai
import qualified Web.Scotty as Scotty

import qualified Debug.Trace as Debug

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Method
  = MethodGET
  | MethodPOST
  | MethodOTHER
  deriving (Eq, Show)

data Request
  = Request
      { path   :: Text
      , method :: Method
      , requestBody :: Text
      }
  deriving (Eq, Show)

-- data Response
  -- = Response
      -- { statusCode   :: Int
      -- , responseBody :: Text
      -- }
  -- deriving (Eq, Show)

data Response where
  OkResponse :: Aeson.ToJSON a => a -> Response
  FailureResponse :: Text -> Response

createRequest :: Scotty.ActionM Request
createRequest = do
  req <- Scotty.request

  let method'
        = case Wai.requestMethod req of
            "GET"  -> MethodGET
            "POST" -> MethodPOST
            _      -> MethodOTHER

  body <- Text.Encoding.decodeUtf8 . ByteString.Lazy.toStrict <$> Scotty.body

  pure Request
    { path   = "/" <> Text.intercalate "/" (Wai.pathInfo req)
    , method = method'
    , requestBody = body
    }

simpleServer :: Int -> (Request -> Response) -> IO ()
simpleServer port toResponse
  = Scotty.scotty port $ do
      Scotty.notFound $ do
        req <- createRequest
        Debug.traceShowM req

        case toResponse req of
          OkResponse body ->
            Scotty.json body
          FailureResponse err ->
            Scotty.raise $ Text.Lazy.fromStrict err

decodeJson :: Aeson.FromJSON a => Text -> Either Text a
decodeJson input
  = left Text.pack $ Aeson.eitherDecode' (toLazy input)
  where
    toLazy = ByteString.Lazy.fromStrict . Text.Encoding.encodeUtf8

okResponse :: Aeson.ToJSON a => a -> Response
okResponse body
  = OkResponse body

failureResponse :: Text -> Response
failureResponse err
  = FailureResponse err

data Add = Add { a :: Int, b :: Int }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON Add

testResponse :: Request -> Response
testResponse req
  = case path req of
      "/math/add" ->
        let
          parsed = decodeJson @Add (requestBody req)
        in
        case parsed of
          Right (Add x y) -> okResponse (x + y)
          Left err        -> failureResponse err
      _ ->
        failureResponse "Invalid path"
