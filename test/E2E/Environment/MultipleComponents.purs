module Test.E2E.Environment.MultipleComponents where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (captures, useQuery, useTickEffect)
import Halogen.Hooks as Hooks
import Test.E2E.Environment.Store (Action(..), useCounterSwitch)
import Test.E2E.Logger (class MonadLogger, writeLogLn)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

counter :: forall q i o m. MonadLogger m => H.Component q i o m
counter = Hooks.component \_ _ -> Hooks.do
  { count } /\ { dispatch } <- useCounterSwitch (_.count >>> { count: _ })

  captures {} useTickEffect do
    writeLogLn counterLogMessage
    pure Nothing

  Hooks.pure do
    HH.div
      [ HP.id "counter" ]
      [ HH.span [ HP.id "value" ] [ HH.text $ show count ]
      , HH.button
          [ HP.type_ ButtonButton
          , HP.id "button"
          , HE.onClick \_ -> dispatch Increment
          ]
          [ HH.text "+1" ]
      ]

switch :: forall q i o m. MonadLogger m => H.Component q i o m
switch = Hooks.component \_ _ -> Hooks.do
  { switch: state } /\ { dispatch } <- useCounterSwitch (_.switch >>> { switch: _ })

  captures {} useTickEffect do
    writeLogLn switchLogMessage
    pure Nothing

  Hooks.pure do
    HH.div
      [ HP.id "switch" ]
      [ HH.span [ HP.id "value" ] [ HH.text $ if state then "ON" else "OFF" ]
      , HH.button
          [ HP.type_ ButtonButton
          , HP.id "button"
          , HE.onClick \_ -> dispatch Toggle
          ]
          [ HH.text "Toggle" ]
      ]

whole :: forall q i o m. MonadAff m => MonadLogger m => H.Component q i o m
whole = Hooks.component \_ _ -> Hooks.do
  state /\ _ <- useCounterSwitch identity

  captures {} useTickEffect do
    writeLogLn wholeLogMessage
    pure Nothing

  Hooks.pure do
    HH.div
      [ HP.id "whole" ]
      [ HH.span [ HP.id "value" ] [ HH.text $ show state ] ]

counterLogMessage :: String
counterLogMessage = "Rerendered: counter"

wholeLogMessage :: String
wholeLogMessage = "Rerendered: whole"

switchLogMessage :: String
switchLogMessage = "Rerendered: switch"

_counter :: QuerySelector
_counter = QuerySelector "#counter"

_switch :: QuerySelector
_switch = QuerySelector "#switch"

_whole :: QuerySelector
_whole = QuerySelector "#whole"

_value :: QuerySelector
_value = QuerySelector "#value"

_button :: QuerySelector
_button = QuerySelector "#button"

data Query a = ResetStore a

app :: forall i o m. MonadAff m => MonadLogger m => H.Component Query i o m
app = Hooks.component \{ queryToken } _ -> Hooks.do
  _ /\ { dispatch } <- useCounterSwitch (const {})

  useQuery queryToken case _ of
    ResetStore next -> do
      dispatch Reset
      pure $ Just next

  Hooks.pure do
    HH.div [ HP.id "app" ]
      [ HH.slot (Proxy :: _ "counter") unit counter {} absurd
      , HH.slot (Proxy :: _ "switch") unit switch {} absurd
      , HH.slot (Proxy :: _ "whole") unit whole {} absurd
      ]