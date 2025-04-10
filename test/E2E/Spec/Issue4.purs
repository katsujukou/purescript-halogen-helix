module Test.E2E.Spec.Issue4 where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen (HalogenIO)
import Test.E2E.Environment as Environment
import Test.E2E.Environment.Issue4 (parent)
import Test.E2E.Logger (VirtualConsole)
import Test.Spec (Spec, before, describe, it)
import Web.HTML (Window)


type TestEnv =
  { window :: Window
  , console :: VirtualConsole
  , io :: HalogenIO (Const Unit) Void Aff
  , elements :: {}
  }

setup :: Aff TestEnv
setup = do
  { window, console, io } <- Environment.setup parent
  pure
    { window
    , console
    , io
    , elements: {}
    }

spec :: Spec Unit
spec = before setup do
  describe "Test for Issue #4" do
    it "should not cause error" \{io} -> do
      io.dispose :: Aff Unit