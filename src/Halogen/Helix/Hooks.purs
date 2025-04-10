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
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (StoreId)
import Halogen.Helix.Store as Store
import Halogen.Helix.Types (HelixContext, HelixMiddleware, HelixContext')
import Halogen.Hooks (class HookNewtype, type (<>), HookType, UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Halogen.UseTrigger (UseTrigger, useTrigger)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UseHelix :: Type -> Hooks.HookType

type UseHelix' :: Type -> Hooks.HookType
type UseHelix' state = UseState state
    <> UseEffect 
    <> Hooks.Pure

instance HookNewtype (UseHelix s) (UseHelix' s)

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
    _ /\ prevId <- useState (unsafeCoerce {})
    let
      connect :: Hooks.HookM m (Hooks.HookM m Unit)
      connect = do
        emitter <- liftEffect $ Store.emitState storeId
        subscription <- Hooks.subscribe $ emitter <#> selector >>> \newState -> do
          prev <- Hooks.get prevId
          when (prev /= newState) do
            Hooks.put prevId newState
            
        pure $ Hooks.unsubscribe subscription


    useLifecycleEffect do
      Just <$> connect

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