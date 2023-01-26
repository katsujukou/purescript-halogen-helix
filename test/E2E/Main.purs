module Test.E2E.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.E2E.Spec (spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec