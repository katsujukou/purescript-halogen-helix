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
  { state :: Ref s
  , dispatch :: a -> Effect Unit
  , emitter :: HS.Emitter s
  , listener :: HS.Listener s
  }

getState :: forall m s a. MonadEffect m => HelixStore s a -> m s
getState (HelixStore { state }) = liftEffect $ Ref.read state

dispatch :: forall m s a. MonadEffect m => HelixStore s a -> a -> m Unit
dispatch (HelixStore s) action = liftEffect $ s.dispatch action

emitState :: forall s a. HelixStore s a -> HS.Emitter s
emitState (HelixStore { emitter }) = emitter

mkHelixStore :: forall s a. String -> s -> (s -> a -> s) -> Effect (HelixStore s a)
mkHelixStore id initial reducer = runEffectFn2 unsafeGetOrCache id $
  defer \_ -> unsafePerformEffect $ do
    state <- Ref.new initial
    { emitter, listener } <- HS.create

    let
      dispatchImpl action = do
        currentState <- Ref.read state
        let newState = reducer currentState action
        Ref.write newState state
        HS.notify listener newState

    pure $ HelixStore
      { state
      , dispatch: dispatchImpl
      , listener
      , emitter
      }

foreign import unsafeGetOrCache :: forall a. EffectFn2 String (Lazy a) a