module Halogen.Helix.Hooks
  ( UseHelix
  , UseHelixHook
  , makeStore
  , makeStore'
  , module Halogen.Helix.Types
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (HelixStore, mkHelixStore)
import Halogen.Helix.Store as Store
import Halogen.Helix.Types (HelixMiddleware', HelixContext')
import Halogen.Hooks (class HookNewtype, type (<>), UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks

foreign import data UseHelix :: Type -> Hooks.HookType

type UseHelix' state = UseState state <> UseEffect <> Hooks.Pure

instance HookNewtype (UseHelix s) (UseHelix' s)

type UseHelixHook state action part m = (state -> part) -> Hooks.Hook m (UseHelix state) (part /\ HelixContext' part action m)

makeStore
  :: forall state action part m
   . MonadEffect m
  => Eq part
  => String
  -> (state -> action -> state)
  -> state
  -> HelixMiddleware' state action m
  -> UseHelixHook state action part m
makeStore id reducer initialState middleware = unsafePerformEffect do
  store <- mkHelixStore id initialState reducer (Just middleware)
  pure $ Hooks.wrap <<< mkHook store (unsafePerformEffect $ Store.getState store)

makeStore'
  :: forall state action part m
   . MonadEffect m
  => Eq part
  => String
  -> (state -> action -> state)
  -> state
  -> UseHelixHook state action part m
makeStore' id reducer initialState = makeStore id reducer initialState (\_ act next -> next act)

mkHook
  :: forall m state action part
   . MonadEffect m
  => Eq part
  => (HelixStore state action (Hooks.HookM m))
  -> state
  -> (state -> part)
  -> Hooks.Hook m (UseHelix' state) (part /\ (HelixContext' part action m))
mkHook store initialState selector = Hooks.do
  _ /\ stateId <- useState initialState

  useLifecycleEffect do
    let emitter = Store.emitState store
    subscription <- Hooks.subscribe $ emitter <#> \nextState -> do
      prevState <- Hooks.get stateId
      when (selector nextState /= selector prevState) do
        Hooks.put stateId nextState

    pure $ Just $ Hooks.unsubscribe subscription

  let
    ctx =
      { getState: liftEffect $ selector <$> Store.getState store
      , dispatch: Store.dispatch store
      }

  Hooks.pure $ Tuple (selector $ unsafePerformEffect $ Store.getState store) ctx
