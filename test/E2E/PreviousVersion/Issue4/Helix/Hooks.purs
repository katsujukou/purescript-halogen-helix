module E2E.PreviousVersion.Issue4.Helix.Hooks
  ( UseHelix
  , UseHelixHook
  , makeStore
  , makeStore'
  , module E2E.PreviousVersion.Issue4.Helix.Types
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Effect.Unsafe (unsafePerformEffect)
import E2E.PreviousVersion.Issue4.Helix.Store (HelixStore, mkHelixStore)
import E2E.PreviousVersion.Issue4.Helix.Store as Store
import E2E.PreviousVersion.Issue4.Helix.Types (HelixMiddleware', HelixContext')
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
  pure $ Hooks.wrap <<< mkHook store initialState

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
      { getState: selector <$> Store.getState store
      , dispatch: Store.dispatch store
      }

  Hooks.pure $ Tuple (selector state) ctx
