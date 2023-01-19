module Halogen.Helix.Store
  ( HelixStore
  , dispatch
  , emitState
  , getState
  , mkHelixStore
  ) where

import Prelude

import Data.Lazy (Lazy, defer)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Subscription as HS

newtype HelixStore s a = HelixStore
  { state :: HelixState s
  , stepper :: HelixStepper s a
  }

type HelixState s =
  { value :: Ref s
  , emitter :: HS.Emitter s
  , listener :: HS.Listener s
  }

type HelixStepper s a =
  { action :: HS.SubscribeIO a
  , state :: HS.SubscribeIO s
  }

getState :: forall m s a. MonadEffect m => HelixStore s a -> m s
getState (HelixStore { state }) = liftEffect $ Ref.read state.value

dispatch :: forall m s a. MonadEffect m => HelixStore s a -> a -> m Unit
dispatch (HelixStore { stepper: { action: { listener } } }) action = liftEffect do
  HS.notify listener action

emitState :: forall s a. HelixStore s a -> HS.Emitter s
emitState (HelixStore { state }) = state.emitter

mkHelixStore :: forall s a. String -> s -> (s -> a -> s) -> Effect (HelixStore s a)
mkHelixStore id initial reducer = runEffectFn2 unsafeGetOrCache id $
  defer \_ -> unsafePerformEffect $ do
    stepper <- do
      action <- HS.create
      state <- HS.create
      pure { action, state }
    state <- do
      value <- Ref.new initial
      { emitter, listener } <- HS.create
      pure { value, emitter, listener }
    setupFlow reducer state stepper
    pure $ HelixStore { state, stepper }

setupFlow :: forall s a. (s -> a -> s) -> HelixState s -> HelixStepper s a -> Effect Unit
setupFlow reducer st stepper = do
  void $ HS.subscribe stepper.action.emitter \action -> do
    current <- Ref.read st.value
    let newState = reducer current action
    HS.notify stepper.state.listener newState

  void $ HS.subscribe stepper.state.emitter \state -> do
    Ref.write state st.value
    HS.notify st.listener state

foreign import unsafeGetOrCache :: forall a. EffectFn2 String (Lazy a) a