module Test.E2E.Environment.SingleComponent where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.Helix (useStore)
import Halogen.Hooks as Hooks
import Test.E2E.Environment.Store (Action(..), _counterSwitch)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  state /\ { dispatch } <- useCounterSwitch "component" identity

  Hooks.pure do
    HH.div [ HP.id "component" ]
      [ HH.div [ HP.id "state" ] [ HH.text $ show state ]
      , HH.button
          [ HP.type_ ButtonButton
          , HP.id "increment-button"
          , HE.onClick \_ -> dispatch Increment
          ]
          [ HH.text "+1" ]
      , HH.button
          [ HP.type_ ButtonButton
          , HP.id "toggle-button"
          , HE.onClick \_ -> dispatch Toggle
          ]
          [ HH.text "Toggle" ]
      ]

_component :: QuerySelector
_component = QuerySelector "#component"

_state :: QuerySelector
_state = QuerySelector "#state"

_incrementButton :: QuerySelector
_incrementButton = QuerySelector "#increment-button"

_toggleButton :: QuerySelector
_toggleButton = QuerySelector "#toggle-button"

app :: forall q i o m. MonadEffect m => H.Component q i o m
app = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.div [ HP.id "app" ]
      [ HH.slot_ (Proxy :: _ "component") unit component {} ]