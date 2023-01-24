module Halogen.Helix.Middleware
  ( (<|)
  , (|>)
  , HelixMiddleware
  , HelixMiddlewareContext
  , composeMiddlewareLtoR
  , composeMiddlewareRtoL
  ) where

import Prelude

type HelixMiddlewareContext state action m =
  { getState :: m state
  , dispatch :: action -> m Unit
  }

type HelixMiddleware state action m = (HelixMiddlewareContext state action m) -> action -> (action -> m Unit) -> m Unit

composeMiddlewareLtoR :: forall s a m. HelixMiddleware s a m -> HelixMiddleware s a m -> HelixMiddleware s a m
composeMiddlewareLtoR g f = \ctx act next -> g ctx act (\act' -> f ctx act' next)

infixr 4 composeMiddlewareLtoR as |>

composeMiddlewareRtoL :: forall s a m. HelixMiddleware s a m -> HelixMiddleware s a m -> HelixMiddleware s a m
composeMiddlewareRtoL = flip composeMiddlewareLtoR

infixl 4 composeMiddlewareRtoL as <|
