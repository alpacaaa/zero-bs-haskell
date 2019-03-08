{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Example where

import Zero.Server

import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified System.Random as Random

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


testMain :: IO ()
testMain
  = startServerOnPort 3000
  $ [ simpleHandler    POST "/math/add" testResponse
    , simpleHandler    GET "/book" $ \_ -> okResponse @[Int] []
    , effectfulHandler POST "/math/random" testEffectResponse
    , statefulHandler  0
        [ mkStatefulHandler POST "/counter" testStateResponse
        , mkStatefulHandler GET  "/counter" $ \state _ -> (state, okResponse state)
        ]
    , statefulHandler guessInitialState
        [ mkStatefulHandler POST "/guess" testStateGuess
        ]
    ]
