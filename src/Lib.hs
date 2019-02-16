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
import qualified System.Random as Random
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
      { requestPath   :: Text
      , requestMethod :: Method
      , requestBody   :: Text
      }
  deriving (Eq, Show)

data Response where
  OkResponse      :: Aeson.ToJSON a => a -> Response
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
    { requestPath   = "/" <> Text.intercalate "/" (Wai.pathInfo req)
    , requestMethod = method'
    , requestBody   = body
    }

simpleServer :: Int -> (Request -> Response) -> IO ()
simpleServer port toResponse
  = Scotty.scotty port $ do
      Scotty.notFound $ do
        req <- createRequest
        Debug.traceShowM req

        handleResponse (toResponse req)

handleResponse :: Response -> Scotty.ActionM ()
handleResponse res
  = case res of
      OkResponse body ->
        Scotty.json body
      FailureResponse err ->
        Scotty.raise $ Text.Lazy.fromStrict err

effectfulServer :: Int -> (Request -> IO Response) -> IO ()
effectfulServer port toResponse
  = Scotty.scotty port $ do
      Scotty.notFound $ do
        req <- createRequest
        Debug.traceShowM req

        res <- Scotty.liftAndCatchIO $ toResponse req
        handleResponse res

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
  = case requestPath req of
      "/math/add" ->
        let
          parsed = decodeJson @Add (requestBody req)
        in
        case parsed of
          Right (Add x y) -> okResponse (x + y)
          Left err        -> failureResponse err

      _ ->
        failureResponse "Invalid path"

testEffectResponse :: Request -> IO Response
testEffectResponse req
  = case requestPath req of
      "/math/random" -> do
        n <- Random.randomRIO @Int (1, 100)
        pure (okResponse n)

      _ ->
        pure (testResponse req)
