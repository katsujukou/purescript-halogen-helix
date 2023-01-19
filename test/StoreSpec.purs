module Test.StoreSpec where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Helix.Store (dispatch, emitState, getState, mkHelixStore)
import Halogen.Subscription (Subscription)
import Halogen.Subscription as HS
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Store (Action(..), reducer)

storeSpec :: forall m. Monad m => SpecT Aff Unit m Unit
storeSpec = describe "Halogen.Helix.Store" do
  let initialState = { count: 0, switch: false }

  describe "getState" do
    it "should return current value" do
      let store = unsafePerformEffect $ mkHelixStore "test1" initialState reducer
      value <- getState store
      value `shouldEqual` initialState

  describe "dispatch" do
    it "should update state correctly: case Increment" do
      let store = unsafePerformEffect $ mkHelixStore "test2" initialState reducer
      before <- getState store
      dispatch store Increment
      after <- getState store
      after `shouldEqual` (before { count = before.count + 1 })

    it "should update state correctly: case Decrement" do
      let store = unsafePerformEffect $ mkHelixStore "test3" initialState reducer
      before <- getState store
      dispatch store Decrement
      after <- getState store
      after `shouldEqual` (before { count = before.count - 1 })

    it "should update state correctly: case Toggle" do
      let store = unsafePerformEffect $ mkHelixStore "test4" initialState reducer
      before <- getState store
      dispatch store Toggle
      after <- getState store
      after `shouldEqual` (before { switch = not before.switch })

    it "should emit when dispatched" do
      let
        store = unsafePerformEffect $ mkHelixStore "test5" initialState reducer
        emitter = emitState store
      _ <- liftEffect $ HS.subscribe emitter \st -> do
        st `shouldEqual` (reducer initialState Increment)
      dispatch store Increment