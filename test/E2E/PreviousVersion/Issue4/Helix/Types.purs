module E2E.PreviousVersion.Issue4.Helix.Types
  ( HelixContext
  , HelixContext'
  , HelixMiddleware
  , HelixMiddleware'
  ) where

import Prelude

import Halogen.Hooks as Hooks

type HelixContext state action m =
  { dispatch :: action -> m Unit
  , getState :: m state
  }

type HelixMiddleware state action m = (HelixContext state action m) -> action -> (action -> m Unit) -> m Unit

type HelixMiddleware' s a m = HelixMiddleware s a (Hooks.HookM m)

type HelixContext' s a m = HelixContext s a (Hooks.HookM m)