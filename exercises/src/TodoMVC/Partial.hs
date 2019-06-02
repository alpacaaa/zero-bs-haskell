-- | The whole point of this module is to put `title`, `completed` and
--   `order` into a different scope. In Haskell, record fields must be
--   unique within the same module (unless you turn on extra extensions).
--   Recall that by defining a record, the compiler automatically generates
--   accessor functions with the same name as the record's fields.
module TodoMVC.Partial where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

-- | Data we read from the request body.
--   It's all Maybes as fields can be empty.
data PartialTodo
  = PartialTodo
      { title     :: Maybe String
      , completed :: Maybe Bool
      , order     :: Maybe Int
      }
  deriving (Eq, Show, Generic, Aeson.FromJSON)
