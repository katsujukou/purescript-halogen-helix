module Test.Unit.Spec where

import Prelude

import Test.Spec (Spec)
import Test.Unit.Store as Store

spec :: Spec Unit
spec = do
  Store.spec