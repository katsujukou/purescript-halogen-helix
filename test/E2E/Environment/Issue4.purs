module Test.E2E.Environment.Issue4 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Helix (useStore)
import Halogen.Helix.Store (StoreId, makeStore)
import Halogen.Hooks (useLifecycleEffect)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

type State = 
  { value :: Boolean
  }

data Action = SetValue Boolean

_app :: forall m. MonadEffect m => StoreId State Action m
_app = makeStore "app-store"  reducer  initialState
  where
  initialState = { value: false }

  reducer s0 = case _ of 
    SetValue b -> s0 { value = b }

parent :: forall q i o m. MonadEffect m => H.Component q i o m 
parent = Hooks.component \_ _ -> Hooks.do
  state /\ ctx <- useStore _app
  useLifecycleEffect do
    ctx.dispatch $ SetValue true
    pure Nothing

  Hooks.pure do
    if (not state.value) then HH.text "not initialized"
    else do
      HH.div_
       [ HH.text $ show state
       , HH.slot_ (Proxy :: _ "child") unit child {}
       ]

child :: forall q i o m. MonadEffect m => H.Component q i o m 
child = Hooks.component \ _ _ -> Hooks.do
  state /\ _ <- useStore _app
  Hooks.pure do
    HH.text $ if state.value then "ON" else "OFF"