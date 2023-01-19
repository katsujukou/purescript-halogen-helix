module Example.Types where

import Prelude

import Data.UUID (UUID)

type TodoItem =
  { id :: UUID
  , title :: String
  , done :: Boolean
  }