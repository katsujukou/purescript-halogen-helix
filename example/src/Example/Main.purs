module Example.Main where

import Prelude

import Effect (Effect)
import Example.App (app)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI app {} body
