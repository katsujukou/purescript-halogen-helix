module Halogen.Helix.Middleware
  ( (<|)
  , (|>)
  , composeMiddlewareLtoR
  , composeMiddlewareRtoL
  , module Halogen.Helix.Types
  ) where

import Prelude

import Halogen.Helix.Types (HelixMiddleware)

composeMiddlewareLtoR :: forall s a m. HelixMiddleware s a m -> HelixMiddleware s a m -> HelixMiddleware s a m
composeMiddlewareLtoR g f = \ctx act next -> g ctx act (\act' -> f ctx act' next)

infixr 4 composeMiddlewareLtoR as |>

composeMiddlewareRtoL :: forall s a m. HelixMiddleware s a m -> HelixMiddleware s a m -> HelixMiddleware s a m
composeMiddlewareRtoL = flip composeMiddlewareLtoR

infixl 4 composeMiddlewareRtoL as <|
