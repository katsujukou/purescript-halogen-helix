module Test.Utils.HappyDOM
  ( awaitAsyncComplete
  , window
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Web.HTML (Window)

foreign import window :: Effect Window

foreign import awaitAsyncCompletePromise :: EffectFn1 Window (Promise Unit)

awaitAsyncComplete :: forall m. MonadAff m => Window -> m Unit
awaitAsyncComplete = runEffectFn1 awaitAsyncCompletePromise >>> toAffE >>> liftAff