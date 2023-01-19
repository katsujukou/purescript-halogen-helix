module Halogen.Helix.Hooks
  ( HelixContext
  , UseHelix
  , UseHelixHook
  , makeStore
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (mkHelixStore)
import Halogen.Helix.Store as Store
import Halogen.Hooks (class HookNewtype, type (<>), UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks

foreign import data UseHelix :: Type -> Hooks.HookType

type UseHelix' state = UseState state <> UseEffect <> Hooks.Pure

instance HookNewtype (UseHelix s) (UseHelix' s)

type HelixContext state action m =
  { dispatch :: action -> Hooks.HookM m Unit
  , getState :: Hooks.HookM m state
  }

type UseHelixHook state action part m = (state -> part) -> Hooks.Hook m (UseHelix state) (part /\ HelixContext part action m)

makeStore
  :: forall state action part m
   . MonadEffect m
  => Eq part
  => String
  -> (state -> action -> state)
  -> state
  -> UseHelixHook state action part m
makeStore id reducer initialState = unsafePerformEffect do
  store <- mkHelixStore id initialState reducer
  pure $ Hooks.wrap <<< mkHook store
  where
  mkHook :: _ -> _ -> Hooks.Hook m (UseHelix' state) _
  mkHook store selector = Hooks.do
    state /\ stateId <- useState initialState

    useLifecycleEffect do
      let emitter = Store.emitState store
      subscription <- Hooks.subscribe $ emitter <#> \newState -> do
        current <- Hooks.get stateId
        when (selector newState /= selector current) do
          Hooks.put stateId newState

      Store.getState store >>= Hooks.put stateId

      pure $ Just $ Hooks.unsubscribe subscription

    let
      ctx =
        { dispatch: Store.dispatch store
        , getState: selector <$> Store.getState store
        }

    Hooks.pure $ (selector state) /\ ctx