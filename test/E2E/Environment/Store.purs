module Test.E2E.Environment.Store where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import Halogen.Helix (makeStore)
import Halogen.Helix.Store (StoreId)

type State = { count :: Int, switch :: Boolean }

data Action = Increment | Toggle | Reset

derive instance Eq Action
derive instance Generic Action _

instance Show Action where
  show = genericShow

reducer :: State -> Action -> State
reducer state = case _ of
  Increment -> state { count = state.count + 1 }
  Toggle -> state { switch = not state.switch }
  Reset -> initialState

initialState :: State
initialState = { count: 0, switch: false }

_counterSwitch :: forall m. MonadEffect m => StoreId State Action m
_counterSwitch = makeStore "counter-switch" reducer initialState
