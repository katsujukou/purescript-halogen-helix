module Test.Unit.Store where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.Helix.Store (StoreId, dispatch, emitState, makeStore)
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

_test1 :: StoreId State Action Aff
_test1 = makeStore "test1" reducer initialState

_test2 :: StoreId State Action Aff
_test2 = makeStore "test2" reducer initialState

_test3 :: StoreId State Action Aff
_test3 = makeStore "test3" reducer initialState

_test4 :: StoreId State Action Aff
_test4 = makeStore "test4" reducer initialState

_test5 :: StoreId State Action Aff
_test5 = makeStore "test5" reducer initialState

spec :: Spec Unit
spec = describe "Halogen.Helix.Store" do
  let
    getState :: _ -> Aff _
    getState = liftEffect <<< Store.getState
  describe "getState" do
    it "should return current value" do
      value <- getState _test1
      value `shouldEqual` initialState

  describe "dispatch" do
    it "should update state correctly: case Increment" do
      before <- getState _test2
      dispatch _test2 Increment
      after <- getState _test2
      after `shouldEqual` (before { count = before.count + 1 })

    it "should update state correctly: case Decrement" do
      before <- getState _test3
      dispatch _test3 Decrement
      after <- getState _test3
      after `shouldEqual` (before { count = before.count - 1 })

    it "should update state correctly: case Toggle" do
      before <- getState _test4
      dispatch _test4 Toggle
      after <- getState _test4
      after `shouldEqual` (before { switch = not before.switch })

    it "should emit when dispatched" do
      emitter <- liftEffect $ emitState _test5
      _ <- liftEffect $ HS.subscribe emitter \st -> do
        st `shouldEqual` (reducer initialState Increment)
      dispatch _test5 Increment