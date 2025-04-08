module Test.E2E.Environment.Middleware where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.Helix (makeStoreMiddleware, useStore, (|>))
import Halogen.Helix.Store (StoreId)
import Halogen.Hooks as Hooks
import Test.E2E.Environment.Store (Action(..), State, initialState, reducer)
import Test.E2E.Logger (class MonadLogger, writeLogLn)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

_counterSwitch :: forall m. MonadEffect m => MonadLogger m => StoreId State Action m
_counterSwitch = makeStoreMiddleware "counter-switch-mw" reducer initialState middleware
  where
  middleware = toggleDispatcher |> stateLogger |> actionLogger

  stateLogger ctx act next = do
    writeLogLn <<< ("Before state: " <> _) <<< show =<< ctx.getState
    next act
    writeLogLn <<< ("After state: " <> _) <<< show =<< ctx.getState

  actionLogger _ act next = do
    writeLogLn <<< ("Dispatched: " <> _) <<< show $ act
    next act

  -- WHen counter reaches to 3, dispatch Toggle action
  toggleDispatcher ctx act next = do
    next act
    whenM ((_ == 3) <<< _.count <$> ctx.getState) do
      when (act /= Toggle) do
        ctx.dispatch Toggle

component :: forall q i o m. MonadLogger m => H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  state /\ { dispatch } <- useStore _counterSwitch 

  Hooks.pure do
    HH.div [ HP.id "component" ]
      [ HH.div [ HP.id "state" ] [ HH.text $ show state ]
      , HH.button
          [ HP.type_ ButtonButton
          , HP.id "increment-button"
          , HE.onClick \_ -> dispatch Increment
          ]
          [ HH.text "+1" ]
      ]

_component :: QuerySelector
_component = QuerySelector "#component"

_state :: QuerySelector
_state = QuerySelector "#state"

_incrementButton :: QuerySelector
_incrementButton = QuerySelector "#increment-button"

app :: forall q i o m. MonadLogger m => H.Component q i o m
app = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.div [ HP.id "app" ]
      [ HH.slot_ (Proxy :: _ "component") unit component {} ]