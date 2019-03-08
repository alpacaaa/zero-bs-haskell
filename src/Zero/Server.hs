{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Zero.Server
  ( Method (..)
  , Handler

  -- Request
  , Request
  , requestBody
  , decodeJson

  -- Response
  , Response
  , stringResponse
  , jsonResponse
  , failureResponse

  -- Handlers
  , simpleHandler
  , effectfulHandler

  , StatefulHandler
  , handlersWithState
  , statefulHandler

  -- Server
  , startServer
  , startServerOnPort
  ) where

import Control.Arrow (left)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
  = GET
  | POST
  deriving (Eq)

instance Show Method where
  show GET  = "GET"
  show POST = "POST"

data Request
  = Request String
  deriving (Eq, Show)

requestBody :: Request -> String
requestBody (Request body) = body

data ResponseType
  = StringResponse
  | JsonResponse
  | FailureResponse

data Response
  = Response
      { responseType :: ResponseType
      , responseBody :: ByteString.Lazy.ByteString
      }

createRequest :: Scotty.ActionM Request
createRequest = do
  body <- Text.Encoding.decodeUtf8 . ByteString.Lazy.toStrict <$> Scotty.body
  pure $ Request (Text.unpack body)

handleResponse :: String -> Request -> Response -> Scotty.ActionM ()
handleResponse path req res = do
  logInfo $ path <> " " <> requestBody req

  case responseType res of
    JsonResponse -> json
    StringResponse -> pure ()
    FailureResponse -> json *> Scotty.status HTTP.Types.status400

  Scotty.raw (responseBody res)

  where
    json
      = Scotty.setHeader "Content-Type" "application/json; charset=utf-8"

decodeJson :: Aeson.FromJSON a => String -> Either Text a
decodeJson input
  = left Text.pack $ Aeson.eitherDecode' (toLazy $ Text.pack input)

toLazy :: Text -> ByteString.Lazy.ByteString
toLazy
  = ByteString.Lazy.fromStrict . Text.Encoding.encodeUtf8

logInfo :: MonadIO m => String -> m ()
logInfo
  = liftIO . putStrLn

jsonResponse :: Aeson.ToJSON a => a -> Response
jsonResponse body
  = Response JsonResponse (Aeson.encode body)

stringResponse :: String -> Response
stringResponse str
  = Response StringResponse $ toLazy (Text.pack str)

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

data StatefulHandler state
  = StatefulHandler
      Method
      String
      (state -> Request -> (state, Response))

statefulHandler
  :: Method
  -> String
  -> (state -> Request -> (state, Response))
  -> StatefulHandler state
statefulHandler
  = StatefulHandler

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

handlersWithState
  :: state
  -> [StatefulHandler state]
  -> Handler
handlersWithState initialState handlers
  = EffectfulHandler $ do
      stateVar <- TVar.newTVarIO initialState
      forM handlers $ \(StatefulHandler method path toResponse) ->
        pure $ StatelessHandler method path $ do
            req <- createRequest

            res <- Scotty.liftAndCatchIO $
              STM.atomically $ do
                state <- TVar.readTVar stateVar
                let (newState, res) = toResponse state req
                TVar.writeTVar stateVar newState
                pure res

            handleResponse path req res

startServerOnPort :: Int -> [Handler] -> IO ()
startServerOnPort port serverDef = do
  logInfo ""
  logInfo "Zero Bullshit Haskell server"
  logInfo "Ready to smash"
  logInfo ""

  handlers <- concat <$> traverse processHandler serverDef
  forM_ handlers logHandler

  logInfo ""

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
              GET  -> HTTP.Types.GET
              POST -> HTTP.Types.POST

    corsMiddleware
      = Cors.cors
          ( const $ Just
            (Cors.simpleCorsResourcePolicy
              { Cors.corsRequestHeaders = ["Content-Type"] })
          )

    logHandler h
      = logInfo
      $ padMethod h <> " " <> handlerPath h

    padMethod h
      = Text.unpack
      $ Text.justifyLeft 5 ' '
      $ Text.pack
      $ show (handlerMethod h)

startServer :: [Handler] -> IO ()
startServer
  = startServerOnPort 7879
