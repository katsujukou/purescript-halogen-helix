module Test.E2E.Spec.Issue4 where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..), isLeft, isRight)
import Effect.Aff (Aff, Error, launchAff_, makeAff, nonCanceler)
import Halogen (HalogenIO, liftEffect)
import Halogen as H
import Node.EventEmitter (on_)
import Node.Process (process, uncaughtExceptionH)
import Test.E2E.Environment as Environment
import Test.E2E.Environment.Issue4 as Issue4
import Test.E2E.Environment.Issue4.Previous as Previous
import Test.E2E.Logger (LoggerT, VirtualConsole)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Utils.HappyDOM (waitUntilComplete)
import Web.HTML (Window)

type TestEnv =
  { window :: Window
  , console :: VirtualConsole
  , io :: HalogenIO (Const Unit) Void Aff
  , elements :: {}
  }

spec :: Spec Unit
spec = describe "Test for Issue #4" do
  describe "Previous behavior" do 
    it "should cause exception" do
      res <- setupAndAwaitUncaughtException Previous.dispatchOnInitialize
      res `shouldSatisfy` isLeft

  describe "Fixed behavior" do
    it "should successfully be initialized" do
      res <- setupAndAwaitUncaughtException Issue4.dispatchOnInitialize
      res `shouldSatisfy` isRight

setupAndAwaitUncaughtException :: forall q o. H.Component q {} o (LoggerT Aff) -> Aff (Either Error Unit)
setupAndAwaitUncaughtException component = do
  makeAff \done -> do
    process # on_ uncaughtExceptionH \err _ -> do 
      done (Right (Left err))
    launchAff_ do 
      { window } <- Environment.setup component
      waitUntilComplete window
      liftEffect $ done (Right (Right unit))
      
    pure nonCanceler
