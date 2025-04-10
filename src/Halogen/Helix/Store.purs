module Halogen.Helix.Store
  ( HelixStore
  , StoreId
  , dispatch
  , emitState
  , getInitialState
  , getState
  , makeStore
  , makeStoreMiddleware
  )
  where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Halogen.Helix.Types (HelixMiddleware)
import Halogen.Subscription as HS
import Safe.Coerce (coerce)

foreign import data StoreIdRep :: Type -> Type

newtype StoreId s a m = StoreId (Effect (StoreIdRep (HelixStore s a m)))

foreign import _mkSingletonStore :: forall a. Fn2 String a (Effect (StoreIdRep a))

foreign import _getSingletonStore :: forall a. EffectFn1 (StoreIdRep a) a

getSingletonStore :: forall s a m. StoreId s a m -> Effect (HelixStore s a m)
getSingletonStore (StoreId id) = id >>= runEffectFn1 _getSingletonStore

newtype HelixStore s a m = HelixStore
  { initialState :: s 
  , state :: Ref s
  , dispatch :: a -> m Unit
  , emitter :: HS.Emitter s
  , listener :: HS.Listener s
  }

getInitialState :: forall s a m. StoreId s a m -> Effect s 
getInitialState storeId = getSingletonStore storeId <#> \(HelixStore s) -> s.initialState 

getState :: forall m s a. StoreId s a m -> Effect s
getState storeId = getSingletonStore storeId >>= \(HelixStore { state }) -> Ref.read state

dispatch :: forall m s a. MonadEffect m => StoreId s a m -> a -> m Unit
dispatch storeId action = liftEffect (getSingletonStore storeId) >>= \(HelixStore st) -> st.dispatch action

emitState :: forall s a m. StoreId s a m -> Effect (HS.Emitter s)
emitState storeId = getSingletonStore storeId <#> \(HelixStore st) -> st.emitter

makeStore
  :: forall s a m
   . MonadEffect m
  => String
  -> (s -> a -> s)
  -> s
  -> StoreId s a m
makeStore id reducer initial = makeStoreMiddleware id reducer initial \_ act next -> next act

makeStoreMiddleware
  :: forall s a m
   . MonadEffect m
  => String
  -> (s -> a -> s)
  -> s
  -> HelixMiddleware s a m
  -> StoreId s a m
makeStoreMiddleware id reducer initial mw = StoreId $ runFn2 _mkSingletonStore id =<< storeEff
  where
  storeEff = do
    state <- Ref.new initial
    { emitter, listener } <- HS.create

    let
      bareDispatch :: a -> Effect Unit
      bareDispatch action = do
        currentState <-  Ref.read state
        let newState = reducer currentState action
        Ref.write newState state
        HS.notify listener newState

      dispatchImpl :: a -> m Unit
      dispatchImpl action = 
        mw
          { getState: liftEffect $ Ref.read state, dispatch: dispatchImpl }
          action
          (liftEffect <<< bareDispatch)

    pure $ HelixStore
      { initialState: initial
      , state
      , dispatch: dispatchImpl
      , listener
      , emitter
      }
