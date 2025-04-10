module Test.E2E.Environment.Issue4.Previous where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import E2E.PreviousVersion.Issue4.Helix.Hooks (UseHelixHook, makeStore')
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (useLifecycleEffect)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

type State =
  { value :: Boolean
  , count :: Int
  }

data Action = SetValue Boolean | Incr

useMyStore :: forall p m. Eq p => MonadEffect m => UseHelixHook State Action p m
useMyStore = makeStore' "app-store" reducer initialState
  where
  initialState = { value: false, count: 0 }

  reducer s0 = case _ of
    SetValue b -> s0 { value = b }
    Incr -> s0 { count = s0.count + 1 }

dispatchOnInitialize :: forall q i o m. MonadEffect m => H.Component q i o m
dispatchOnInitialize = Hooks.component \_ _ -> Hooks.do
  state@{ value } /\ ctx <- useMyStore identity
  useLifecycleEffect do
    ctx.dispatch $ SetValue true
    pure Nothing

  Hooks.pure do
    if (not value) then HH.text "not initialized"
    else do
      HH.div_
        [ HH.text $ show state
        , HH.slot_ (Proxy :: _ "child") unit child {}
        ]

child :: forall q i o m. MonadEffect m => H.Component q i o m
child = Hooks.component \_ _ -> Hooks.do
  { value } /\ _ <- useMyStore (_.value >>> { value: _ })
  Hooks.pure do
    HH.text $ if value then "ON" else "OFF"