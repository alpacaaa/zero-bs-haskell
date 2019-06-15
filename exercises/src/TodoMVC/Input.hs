module TodoMVC.Input where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

data Input
  = Input
      { title     :: Maybe String
      , completed :: Maybe Bool
      , order     :: Maybe Int
      }
  deriving (Eq, Show, Generic, Aeson.FromJSON)
