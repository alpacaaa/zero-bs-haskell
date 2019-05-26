module TodoMVC.Input where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

-- | Data we read from the request body.
--   It's all Maybes as all fields are optional when patching.
data InputTodo
  = InputTodo
      { title     :: Maybe String
      , completed :: Maybe Bool
      , order     :: Maybe Int
      }
  deriving (Eq, Show, Generic, Aeson.FromJSON)
