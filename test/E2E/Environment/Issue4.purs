module Test.E2E.Environment.Issue4 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Helix (makeStore, useSelector, useStore)
import Halogen.Helix.Store (StoreId)
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

type State =
  { value :: Boolean
  , count :: Int
  }

data Action = SetValue Boolean | Incr

appStore :: forall m. MonadEffect m => StoreId State Action m
appStore = makeStore "app-store" reducer initialState
  where
  initialState = { value: false, count: 0 }

  reducer s0 = case _ of
    SetValue b -> s0 { value = b }
    Incr -> s0 { count = s0.count + 1 }

dispatchOnInitialize :: forall q i o m. MonadEffect m => H.Component q i o m
dispatchOnInitialize = Hooks.component \_ _ -> Hooks.do
  state@{ value } /\ ctx <- useStore appStore
  useLifecycleEffect do
    ctx.dispatch $ SetValue true
    pure Nothing

  Hooks.pure do
    if (not value) then HH.text "not initialized"
    else do
      HH.div_
        [ HH.text $ show state
        , HH.slot_ (Proxy :: _ "child") unit child {}
        , HH.slot_ (Proxy :: _ "child2") unit child2 {}
        ]

child :: forall q i o m. MonadEffect m => H.Component q i o m
child = Hooks.component \_ _ -> Hooks.do
  { value } /\ _ <- useSelector appStore (_.value >>> { value: _ })
  Hooks.pure do
    HH.text $ if value then "ON" else "OFF"

child2 :: forall q i o m. MonadEffect m => H.Component q i o m
child2 = Hooks.component \_ _ -> Hooks.do
  { value } /\ _ <- useState { value: false }
  Hooks.pure do
    HH.text $ if value then "ON" else "OFF"