{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Zero.Server
  ( Method (..)

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
  , Handler
  , simpleHandler
  , effectfulHandler

  , StatefulHandler
  , statefulHandler
  , handlersWithState

  -- Server
  , startServer
  , startServerOnPort
  ) where

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

-- | HTTP Method
-- Can be: `GET` or `POST`.
data Method
  = GET
  | POST
  deriving (Eq, Show)

-- | HTTP Request. Note that you can't pattern match on this directly.
-- You'll need something like `requestBody`.
data Request
  = Request String
  deriving (Eq, Show)

-- | Extract the request body as a `String`.
-- This is the raw request body, no parsing happens at this stage.
requestBody :: Request -> String
requestBody (Request body) = body

data ResponseType
  = StringResponse
  | JsonResponse
  | FailureResponse

-- | HTTP Response. Note you can't create values of this type directly.
-- You'll need something like `stringResponse`, `jsonResponse` or `failureResponse`.
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

-- | Given a `String`, either succesfully parse it to a type `a`
-- or return an error (as a `String`).
-- It's very important for the compiler to know what the `a` type is.
-- If you're having problem with `Ambiguous occurrence...`, read this article (TODO).
decodeJson :: Aeson.FromJSON a => String -> Either String a
decodeJson input
  = Aeson.eitherDecode' (toLazy $ Text.pack input)

toLazy :: Text -> ByteString.Lazy.ByteString
toLazy
  = ByteString.Lazy.fromStrict . Text.Encoding.encodeUtf8

logInfo :: MonadIO m => String -> m ()
logInfo
  = liftIO . putStrLn

-- | Create a `Response` with some JSON value.
-- It helps to read this signature as:
-- > If you give me something that can be serialized to JSON,
-- > I'll give you back a response with a JSON serialized body.
jsonResponse :: Aeson.ToJSON a => a -> Response
jsonResponse body
  = Response JsonResponse (Aeson.encode body)

-- | Create a `Response` with some raw value (just a plain `String`).
stringResponse :: String -> Response
stringResponse str
  = Response StringResponse $ toLazy (Text.pack str)

-- | Create a `Response` with an error and set the status code to `400`.
failureResponse :: String -> Response
failureResponse err
  = Response FailureResponse $ toLazy (Text.pack err)

-- | An `Handler` is something that can handle HTTP requests.
-- | You can create handlers with these functions:
--
--     * `simpleHandler`
--
--     * `effectfulHandler`
--
--     * `statefulHandler`
data Handler
  = SimpleHandler    StatelessHandler
  | EffectfulHandler (IO [StatelessHandler])

data StatelessHandler
  = StatelessHandler
      { handlerMethod :: Method
      , handlerPath   :: String
      , handlerFn     :: Scotty.ActionM ()
      }

-- | A data type to describe stateful handlers.
-- TODO decide if best to keep `statefulHandler` or just stick with the type.
data StatefulHandler state
  = StatefulHandler
      Method
      String
      (state -> Request -> (state, Response))

-- | Most basic HTTP handler.
-- With a `simpleHandler` you can turn a `Request` into a `Response`,
-- but you're not allowed to use any side effects or maintain any state
-- across requests.
simpleHandler :: Method -> String -> (Request -> Response) -> Handler
simpleHandler method path toResponse
  = SimpleHandler
  $ StatelessHandler method path $ do
      req <- createRequest
      handleResponse path req (toResponse req)

-- | An handler that allows side effects (note the `IO` in `IO Response`).
-- Unlike a `simpleHandler`, you can now have `IO` operations executed in
-- order to generate a `Response`.
--
-- For example, you might want to query a database or make an HTTP request
-- to some webservice and use the result in the `Response` body.
effectfulHandler :: Method -> String -> (Request -> IO Response) -> Handler
effectfulHandler method path toResponse
  = SimpleHandler
  $ StatelessHandler method path $ do
      req <- createRequest
      res <- Scotty.liftAndCatchIO $ toResponse req
      handleResponse path req res

-- | A `StatefulHandler` allows you to keep some state around across requests.
-- For example, if you want to implement a counter, you could keep the current
-- tally as state, and increase it everytime a `Request` comes in.
--
-- The tricky bit is understanding this callback `(state -> Request -> (state, Response))`.
-- Compare it with the simpler `Request -> Response`. The difference is that you get
-- the current state as a parameter, and you no longer return *just* the `Response`,
-- but an updated version of the state as well. For a more in depth explanation,
-- read this article (TODO).
statefulHandler
  :: Method
  -> String
  -> (state -> Request -> (state, Response))
  -> StatefulHandler state
statefulHandler
  = StatefulHandler

-- | Once you have some `StatefulHandler`s that share the same state (that's important!),
-- you can create a proper `Handler` that you can use in your server definition.
--
-- In fact, you cannot use `StatefulHandler` directly in `startServer`, as it only
-- accepts values of type `Handler`.
--
-- What's the first parameter `state` you ask? Well, it's the initial state!
-- The server needs an initial value to pass along the first `Request`, how
-- else would it be able to come up with some state (especially given that it
-- knows nothing about what `state` _is_, it could be anything! Yay, polymorphysm).
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

-- | Exactly like `startServer`, but allows you to specify a different port.
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

-- | Start the server with the given `Handler`s.
-- Click on `Handler` to know how to make one.
--
-- The server will listen on port `7879`. If you're following along with the
-- exercises, they expect to find a server running on that port. In other words,
-- you are good to go!
startServer :: [Handler] -> IO ()
startServer
  = startServerOnPort 7879
