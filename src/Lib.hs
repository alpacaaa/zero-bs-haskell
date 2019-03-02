{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( Method (..)
  , Request (..)
  , Response
  , StatefulHandlerFn (..)
  , okResponse
  , failureResponse
  , decodeJson
  , simpleHandler
  , effectfulHandler
  , statefulHandler
  , startServer
  ) where

import Control.Arrow (left)
import Control.Monad (forM, forM_)
import Data.Text (Text)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Web.Scotty as Scotty

-- import qualified Debug.Trace as Debug

data Method
  = MethodGET
  | MethodPOST
  deriving (Eq, Show)

data Request
  = Request
      { requestBody :: Text
      }
  deriving (Eq, Show)

data ResponseType
  = OkResponse
  | FailureResponse

data Response
  = Response
      { responseType :: ResponseType
      , responseBody :: ByteString.Lazy.ByteString
      }

createRequest :: Scotty.ActionM Request
createRequest = do
  body <- Text.Encoding.decodeUtf8 . ByteString.Lazy.toStrict <$> Scotty.body
  pure Request
    { requestBody = body
    }

handleResponse :: String -> Request -> Response -> Scotty.ActionM ()
handleResponse path req res = do
  Scotty.setHeader "Content-Type" "application/json; charset=utf-8"
  logInfo $ Text.pack path <> " " <> requestBody req

  case responseType res of
    OkResponse -> Scotty.status HTTP.Types.status200
    FailureResponse -> Scotty.status HTTP.Types.status400

  Scotty.raw (responseBody res)

decodeJson :: Aeson.FromJSON a => Text -> Either Text a
decodeJson input
  = left Text.pack $ Aeson.eitherDecode' (toLazy input)

toLazy :: Text -> ByteString.Lazy.ByteString
toLazy
  = ByteString.Lazy.fromStrict . Text.Encoding.encodeUtf8

logInfo :: Text -> Scotty.ActionM ()
logInfo
  = Scotty.liftAndCatchIO . putStrLn . Text.unpack

okResponse :: Aeson.ToJSON a => a -> Response
okResponse body
  = Response OkResponse (Aeson.encode body)

failureResponse :: Text -> Response
failureResponse err
  = Response FailureResponse (toLazy err)

data Handler
  = SimpleHandler    StatelessHandler
  | EffectfulHandler (IO [StatelessHandler])

data StatelessHandler
  = StatelessHandler
      { handlerMethod :: Method
      , handlerPath   :: String
      , handlerFn     :: Scotty.ActionM ()
      }

-- Refactor to helper function (?)
data StatefulHandlerFn state
  = StatefulHandlerFn
      Method
      String
      (state -> Request -> (state, Response))

simpleHandler :: Method -> String -> (Request -> Response) -> Handler
simpleHandler method path toResponse
  = SimpleHandler
  $ StatelessHandler method path $ do
      req <- createRequest
      handleResponse path req (toResponse req)

effectfulHandler :: Method -> String -> (Request -> IO Response) -> Handler
effectfulHandler method path toResponse
  = SimpleHandler
  $ StatelessHandler method path $ do
      req <- createRequest
      res <- Scotty.liftAndCatchIO $ toResponse req
      handleResponse path req res

statefulHandler
  :: state
  -> [StatefulHandlerFn state]
  -> Handler
statefulHandler initialState handlers
  = EffectfulHandler $ do
      stateVar <- TVar.newTVarIO initialState
      forM handlers $ \(StatefulHandlerFn method path toResponse) ->
        pure $ StatelessHandler method path $ do
            req <- createRequest

            res <- Scotty.liftAndCatchIO $
              STM.atomically $ do
                state <- TVar.readTVar stateVar
                let (newState, res) = toResponse state req
                TVar.writeTVar stateVar newState
                pure res

            handleResponse path req res

startServer :: Int -> [Handler] -> IO ()
startServer port serverDef = do
  handlers <- concat <$> traverse processHandler serverDef
  -- TODO show handlers

  Scotty.scotty port $ do
    Scotty.middleware corsMiddleware

    forM_ handlers $ \h -> do
      let (method, route, routeHandler) = makeRoute h
      Scotty.addroute method route routeHandler

  where
    processHandler = \case
      SimpleHandler h -> pure [h]
      EffectfulHandler makeHandlers -> makeHandlers

    makeRoute h
      = (method, route, handlerFn h)
      where
        route
          = Scotty.capture (handlerPath h)
        method
          = case handlerMethod h of
              MethodGET  -> HTTP.Types.GET
              MethodPOST -> HTTP.Types.POST

    corsMiddleware
      = Cors.cors
          ( const $ Just
            (Cors.simpleCorsResourcePolicy
              { Cors.corsRequestHeaders = ["Content-Type"] })
          )
