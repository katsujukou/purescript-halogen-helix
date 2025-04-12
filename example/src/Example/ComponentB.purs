module Example.ComponentB where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Store (_todos)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Helix (useStore)
import Halogen.Hooks as Hooks

make :: forall q i o m. MonadEffect m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  { items } /\ _ <- useStore _todos

  Hooks.pure $ do
    HH.div []
      [ HH.text "Component B"
      , HH.p []
          [ HH.text $ show items ]
      ]
