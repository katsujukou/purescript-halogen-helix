module E2E.PreviousVersion.Issue4.Helix.Store
  ( HelixStore
  , dispatch
  , emitState
  , getState
  , mkHelixStore
  ) where

import Prelude

import Data.Lazy (Lazy, defer)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Types (HelixMiddleware)
import Halogen.Subscription as HS

newtype HelixStore s a m = HelixStore
  { state :: Ref s
  , dispatch :: a -> m Unit
  , emitter :: HS.Emitter s
  , listener :: HS.Listener s
  }

getState :: forall m s a. MonadEffect m => HelixStore s a m -> m s
getState (HelixStore { state }) = liftEffect $ Ref.read state

dispatch :: forall m s a. MonadEffect m => HelixStore s a m -> a -> m Unit
dispatch (HelixStore s) action = s.dispatch action

emitState :: forall s a m. HelixStore s a m -> HS.Emitter s
emitState (HelixStore { emitter }) = emitter

mkHelixStore
  :: forall s a m
   . MonadEffect m
  => String
  -> s
  -> (s -> a -> s)
  -> Maybe (HelixMiddleware s a m)
  -> Effect (HelixStore s a m)
mkHelixStore id initial reducer mbMiddleware =
  runEffectFn2 unsafeGetOrCache id $
    defer \_ -> unsafePerformEffect $ do
      state <- Ref.new initial
      { emitter, listener } <- HS.create

      let
        bareDispatch action = do
          currentState <- Ref.read state
          let newState = reducer currentState action
          Ref.write newState state
          HS.notify listener newState

        dispatchImpl action = case mbMiddleware of
          Just mw -> mw
            { getState: liftEffect $ Ref.read state, dispatch: dispatchImpl }
            action
            (liftEffect <<< bareDispatch)
          Nothing -> liftEffect <<< bareDispatch $ action

      pure $ HelixStore
        { state
        , dispatch: dispatchImpl
        , listener
        , emitter
        }

foreign import unsafeGetOrCache :: forall a. EffectFn2 String (Lazy a) a