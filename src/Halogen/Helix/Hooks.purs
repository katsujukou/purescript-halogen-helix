module Halogen.Helix.Hooks
  ( UseDispatch
  , UseHelix
  , module Halogen.Helix.Types
  , useDispatch
  , useSelector
  , useStore
  )
  where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (StoreId, getInitialState)
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

useSelector
  :: forall m state action part
   . MonadEffect m
  => Eq part
  => StoreId state action m
  -> (state -> part)
  -> Hooks.Hook m (UseHelix part) (part /\ (HelixContext' part action m))
useSelector storeId selector = Hooks.wrap hook
  where
  hook :: Hooks.Hook _ (UseHelix' _) _
  hook = Hooks.do
    _ /\ stateId <- useState Nothing 
    _ /\ finalizerRef <- useRef (pure unit)
    { onNextTick } <- useTrigger 
    let 
      state = unsafePerformEffect $ Store.getState storeId

      connect :: Hooks.HookM m (Hooks.HookM m Unit)
      connect = do
        emitter <- liftEffect $ Store.emitState storeId
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
      { getState: liftEffect $ selector <$> Store.getState storeId
      , dispatch: lift <<< Store.dispatch storeId
      }

    Hooks.pure $ Tuple (selector state) ctx

useStore
  :: forall m state action
   . MonadEffect m
  => Eq state
  => StoreId state action m
  -> Hooks.Hook m (UseHelix state) (state /\ (HelixContext' state action m))
useStore storeId = useSelector storeId identity

foreign import data UseDispatch :: HookType

type UseDispatch' = Hooks.Pure

instance HookNewtype UseDispatch UseDispatch' 

useDispatch :: forall m state action 
  . MonadEffect m 
  => StoreId state action m
  -> Hooks.Hook m UseDispatch (action -> Hooks.HookM m Unit)
useDispatch storeId = Hooks.wrap $ Hooks.pure (lift <<< Store.dispatch storeId)