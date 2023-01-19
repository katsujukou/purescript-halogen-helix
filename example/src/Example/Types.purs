module Example.Types where

import Data.UUID (UUID)

type TodoItem =
  { id :: UUID
  , title :: String
  , done :: Boolean
  }