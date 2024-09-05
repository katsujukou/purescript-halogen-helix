module Test.E2E.Main where

import Prelude

import Effect (Effect)
import Test.E2E.Spec (spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec