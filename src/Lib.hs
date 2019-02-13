{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Web.Scotty as Scotty
import qualified Network.Wai as Wai

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
      { path    :: Text
      , method  :: Method
      , headers :: Map Text Text
      , params  :: Map Text Text
      , body    :: ByteString
      }
  deriving (Eq, Show)

data Response
  = Response
  deriving (Eq, Show)

createRequest :: Scotty.ActionM Request
createRequest = do
  req <- Scotty.request

  let method'
        = case Wai.requestMethod req of
            "GET"  -> MethodGET
            "POST" -> MethodPOST
            _      -> MethodOTHER

  headers' <- makeStrictTuple <$> Scotty.headers
  params' <- makeStrictTuple <$> Scotty.params
  body' <- ByteString.Lazy.toStrict <$> Scotty.body

  pure Request
    { path    = Text.intercalate "/" (Wai.pathInfo req)
    , method  = method'
    , headers = Map.fromList headers'
    , params  = Map.fromList params'
    , body    = body'
    }

simpleServer :: Int -> (Request -> Response) -> IO ()
simpleServer port toResponse
  = Scotty.scotty port $ do
      Scotty.notFound $ do
        req <- createRequest
        let res = toResponse req
        Debug.traceShowM req
        Scotty.json ("ciao" :: String)

makeStrictTuple :: [(Text.Lazy.Text, Text.Lazy.Text)] -> [(Text, Text)]
makeStrictTuple xs
  = go <$> xs
  where
    go (a, b) = (Text.Lazy.toStrict a, Text.Lazy.toStrict b)

