module Test.Unit.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Unit.Spec (spec)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec