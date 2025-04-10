module Halogen.Helix.Hooks
  ( UseDispatch
  , UseHelix
  , UseHelixHook
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
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (StoreId)
import Halogen.Helix.Store as Store
import Halogen.Helix.Types (HelixContext, HelixMiddleware, HelixContext')
import Halogen.Hooks (class HookNewtype, type (<>), HookType, UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks

foreign import data UseHelix :: Type -> Hooks.HookType

type UseHelix' :: Type -> Hooks.HookType
type UseHelix' state = -- UseState UseStoreState
    UseState state
    <> UseEffect 
    -- <> UseEffect 
    <> Hooks.Pure

instance HookNewtype (UseHelix s) (UseHelix' s)

type UseHelixHook state action part m = (state -> part) -> Hooks.Hook m (UseHelix state) (part /\ HelixContext part action m)

data UseStoreState = Disconnected | Ready | Connected 

derive instance Eq UseStoreState 

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
    _ /\ stateId <- useState (unsafePerformEffect $ selector <$> Store.getState storeId)
 
    useLifecycleEffect do
        emitter <- liftEffect $ Store.emitState storeId
        subscription <- Hooks.subscribe $ emitter <#> selector >>> \newState -> do
          current <- Hooks.get stateId
          when (current /= newState) do
            Hooks.put stateId newState
        pure $ Just $ Hooks.unsubscribe subscription

    let
      ctx =
        { getState: liftEffect $ selector <$> Store.getState storeId
        , dispatch: lift <<< Store.dispatch storeId
        }

    Hooks.pure $ Tuple (unsafePerformEffect $ selector <$> Store.getState storeId) ctx

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