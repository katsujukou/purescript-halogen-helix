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
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (HelixStore, mkHelixStore)
import Halogen.Helix.Store as Store
import Halogen.Helix.Types (HelixMiddleware', HelixContext')
import Halogen.Hooks (class HookNewtype, type (<>), UseEffect, UseRef, UseState, useLifecycleEffect, useRef, useState)
import Halogen.Hooks as Hooks
import Halogen.UseTrigger (UseTrigger, useTrigger)

foreign import data UseHelix :: Type -> (Type -> Type) -> Hooks.HookType

type UseHelix' state m =
  UseState (Maybe state)
    <> UseRef (Hooks.HookM m Unit)
    <> UseTrigger m
    <> UseEffect
    <> Hooks.Pure

instance HookNewtype (UseHelix s m) (UseHelix' s m)

type UseHelixHook state action part m = (state -> part) -> Hooks.Hook m (UseHelix state m) (part /\ HelixContext' part action m)

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
  pure $ Hooks.wrap <<< mkHook store

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
  -> (state -> part)
  -> Hooks.Hook m (UseHelix' state m) (part /\ (HelixContext' part action m))
mkHook store selector = Hooks.do
  _ /\ stateId <- useState Nothing
  _ /\ finalizerRef <- useRef (pure unit)
  { onNextTick } <- useTrigger
  let
    state = unsafePerformEffect $ Store.getState store

    connect :: Hooks.HookM m (Hooks.HookM m Unit)
    connect = do
      let emitter = Store.emitState store
      subscription <- Hooks.subscribe $ emitter <#> \nextState -> do
        prevState <- Hooks.get stateId
        when ((selector <$> prevState) /= Just (selector nextState)) do
          Hooks.put stateId (Just nextState)

      pure $ Hooks.unsubscribe subscription

  useLifecycleEffect do
    onNextTick \_ -> do
      disconnect <- connect
      liftEffect $ Ref.write disconnect finalizerRef
      pure unit

    Hooks.put stateId (Just state)

    pure $ Just $ join $ liftEffect $ Ref.read finalizerRef
  let
    ctx =
      { getState: liftEffect $ selector <$> Store.getState store
      , dispatch: Store.dispatch store
      }

  Hooks.pure $ Tuple (selector state) ctx
