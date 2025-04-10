module Test.E2E.Spec where

import Prelude

import Test.E2E.Spec.Issue4 as Issue4
import Test.E2E.Spec.Middleware as MW
import Test.E2E.Spec.MultipleComponents as MC
import Test.E2E.Spec.SingleComponent as SC
import Test.Spec (Spec)

spec :: Spec Unit
spec = do
  SC.spec
  MC.spec
  MW.spec
  Issue4.spec