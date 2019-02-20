{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import Control.Arrow (left)
import Control.Monad (forM_)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Encoding as Text.Encoding
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai.Middleware.Cors as Cors
import qualified System.Random as Random
import qualified Web.Scotty as Scotty

import qualified Debug.Trace as Debug

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Method
  = MethodGET
  | MethodPOST
  deriving (Eq, Show)

data Request
  = Request
      { requestBody   :: Text
      }
  deriving (Eq, Show)

data Response where
  OkResponse      :: Aeson.ToJSON a => a -> Response
  FailureResponse :: Text -> Response

createRequest :: Scotty.ActionM Request
createRequest = do
  body <- Text.Encoding.decodeUtf8 . ByteString.Lazy.toStrict <$> Scotty.body
  pure Request
    { requestBody = body
    }

handleResponse :: Response -> Scotty.ActionM ()
handleResponse res
  = case res of
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
  = let
      parsed = decodeJson @Add (requestBody req)
    in
    case parsed of
      Right (Add x y) -> okResponse (x + y)
      Left err        -> failureResponse err

testEffectResponse :: Request -> IO Response
testEffectResponse _ = do
  n <- Random.randomRIO @Int (1, 100)
  pure (okResponse n)

testStateResponse :: Int -> Request -> (Int, Response)
testStateResponse state _
  = let newState = state + 1
    in (newState, okResponse newState)


data GuessState
  = GuessState
      { guess  :: String
      , actual :: String
      }

data Guess
  = Guess { letter :: Char }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON Guess

guessInitialState :: GuessState
guessInitialState = GuessState "" "merda"

testStateGuess :: GuessState -> Request -> (GuessState, Response)
testStateGuess state req
  = let
      parsed = decodeJson @Guess (requestBody req)
    in
    case parsed of
      Right (Guess c) -> updateState c
      Left err        -> (state, failureResponse err)
  where
    updateState c
      = (state { guess = guess state <> [c] }, okResponse @Int 1)


type Server
  = [ Handler ]

data Handler
  = Handler
      { handlerMethod :: Method
      , handlerPath   :: String
      , handlerFn     :: IO (Scotty.ActionM ())
      }

simpleHandler :: Method -> String -> (Request -> Response) -> Handler
simpleHandler method path toResponse
  = Handler method path $ pure $ do
      req <- createRequest
      Debug.traceShowM req
      handleResponse (toResponse req)

effectfulHandler :: Method -> String -> (Request -> IO Response) -> Handler
effectfulHandler method path toResponse
  = Handler method path $ pure $ do
      req <- createRequest
      Debug.traceShowM req
      res <- Scotty.liftAndCatchIO $ toResponse req
      handleResponse res

statefulHandler :: Method -> String -> state -> (state -> Request -> (state, Response)) -> Handler
statefulHandler method path initialState toResponse
  = Handler method path $ initTVar $ \stateVar -> do
      req <- createRequest
      Debug.traceShowM req

      res <- Scotty.liftAndCatchIO $
        STM.atomically $ do
          state <- TVar.readTVar stateVar
          let (newState, res) = toResponse state req
          TVar.writeTVar stateVar newState
          pure res

      handleResponse res
  where
    initTVar f = do
      stateVar <- TVar.newTVarIO initialState
      pure (f stateVar)

serverV2 :: Int -> Server -> IO ()
serverV2 port serverDef = do
  handlers <- traverse makeRoute serverDef
  Scotty.scotty port $ do
    Scotty.middleware corsMiddleware
    forM_ handlers $ \(method, route, routeHandler) ->
      Scotty.addroute method route routeHandler
  where
    makeRoute h = do
      let route
            = Scotty.capture (handlerPath h)
          method
            = case handlerMethod h of
                MethodGET  -> HTTP.Types.GET
                MethodPOST -> HTTP.Types.POST

      routeHandler <- handlerFn h
      Debug.traceShowM (method, handlerPath h)
      pure (method, route, routeHandler)

    corsMiddleware
      = Cors.cors
          ( const $ Just
            (Cors.simpleCorsResourcePolicy
              { Cors.corsRequestHeaders = ["Content-Type"] })
          )

testMain :: IO ()
testMain
  = serverV2 3000
  $ [ simpleHandler    MethodPOST "/math/add" testResponse
    , simpleHandler    MethodGET "/book" $ \_ -> okResponse @[Int] []
    , effectfulHandler MethodPOST "/math/random" testEffectResponse
    , statefulHandler  MethodPOST "/counter" 0 testStateResponse
    ]
