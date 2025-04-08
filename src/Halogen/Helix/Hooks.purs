module Halogen.Helix.Hooks
  ( UseHelix
  , UseHelixHook
  , useStore
  , module Halogen.Helix.Types
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (StoreId)
import Halogen.Helix.Store as Store
import Halogen.Helix.Types (HelixMiddleware', HelixContext')
import Halogen.Hooks (class HookNewtype, type (<>), UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks

foreign import data UseHelix :: Type -> Hooks.HookType

type UseHelix' state = UseState state <> UseEffect <> Hooks.Pure

instance HookNewtype (UseHelix s) (UseHelix' s)

type UseHelixHook state action part m = (state -> part) -> Hooks.Hook m (UseHelix state) (part /\ HelixContext' part action m)

useStore
  :: forall m state action
   . MonadEffect m
  => Eq state
  => StoreId state action (Hooks.HookM m)
  -> Hooks.Hook m (UseHelix state) (state /\ (HelixContext' state action m))
useStore storeId = Hooks.wrap hook
  where
  hook :: Hooks.Hook _ (UseHelix' _) _
  hook = Hooks.do
    state /\ stateId <- useState (unsafePerformEffect (Store.getState storeId))

    useLifecycleEffect do
      emitter <- liftEffect $ Store.emitState storeId
      subscription <- Hooks.subscribe $ emitter <#> \newState -> do
        current <- Hooks.get stateId
        when (newState /= current) do
          Hooks.put stateId newState

      liftEffect (Store.getState storeId) >>= Hooks.put stateId

      pure $ Just $ Hooks.unsubscribe subscription

    let
      ctx =
        { getState: liftEffect $ Store.getState storeId
        , dispatch: Store.dispatch storeId
        }

    Hooks.pure $ Tuple state ctx
