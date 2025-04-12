module Test.Utils.HappyDOM
  ( waitUntilComplete
  , window
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Promise (Promise)
import Promise.Aff (toAffE)
import Web.HTML (Window)

foreign import window :: Effect Window

foreign import _waitUntilComplete :: EffectFn1 Window (Promise Unit)

waitUntilComplete :: forall m. MonadAff m => Window -> m Unit
waitUntilComplete = runEffectFn1 _waitUntilComplete >>> toAffE >>> liftAff
