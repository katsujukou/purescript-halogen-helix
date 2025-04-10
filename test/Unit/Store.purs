module Test.Unit.Store where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (dispatch, emitState, mkHelixStore)
import Halogen.Helix.Store as Store
import Halogen.Subscription as HS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type State = { count :: Int, switch :: Boolean }

data Action = Increment | Decrement | Toggle

reducer :: State -> Action -> State
reducer state = case _ of
  Increment -> state { count = state.count + 1 }
  Decrement -> state { count = state.count - 1 }
  Toggle -> state { switch = not state.switch }

initialState :: State
initialState = { count: 0, switch: false }

spec :: Spec Unit
spec = describe "Halogen.Helix.Store" do
  let 
    getState :: _ -> Aff _
    getState = liftEffect <<< Store.getState
  describe "getState" do
    it "should return current value" do
      let store = unsafePerformEffect $ mkHelixStore "test1" initialState reducer Nothing
      value <- getState store
      value `shouldEqual` initialState

  describe "dispatch" do
    it "should update state correctly: case Increment" do
      let store = unsafePerformEffect $ mkHelixStore "test2" initialState reducer Nothing
      before <- getState store
      dispatch store Increment
      after <- getState store
      after `shouldEqual` (before { count = before.count + 1 })

    it "should update state correctly: case Decrement" do
      let store = unsafePerformEffect $ mkHelixStore "test3" initialState reducer Nothing
      before <- getState store
      dispatch store Decrement
      after <- getState store
      after `shouldEqual` (before { count = before.count - 1 })

    it "should update state correctly: case Toggle" do
      let store = unsafePerformEffect $ mkHelixStore "test4" initialState reducer Nothing
      before <- getState store
      dispatch store Toggle
      after <- getState store
      after `shouldEqual` (before { switch = not before.switch })

    it "should emit when dispatched" do
      let
        store = unsafePerformEffect $ mkHelixStore "test5" initialState reducer Nothing
        emitter = emitState store
      _ <- liftEffect $ HS.subscribe emitter \st -> do
        st `shouldEqual` (reducer initialState Increment)
      dispatch store Increment