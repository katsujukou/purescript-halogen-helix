module Example.Store where

import Prelude

import Data.Array (findIndex, modifyAt, snoc)
import Data.Maybe (fromMaybe)
import Data.UUID as UUID
import Effect.Class (class MonadEffect)
import Effect.Unsafe (unsafePerformEffect)
import Example.Types (TodoItem)
import Halogen.Helix (UseHelixHook, makeStore)

type State = Array TodoItem

data Action
  = AddTodoItem UUID.UUID String
  | MarkDone UUID.UUID

initialState :: State
initialState = unsafePerformEffect do
  item1 <- { id: _, title: "Develop Halogen App", done: false } <$> UUID.genUUID
  item2 <- { id: _, title: "Study Category Theory", done: false } <$> UUID.genUUID
  item3 <- { id: _, title: "Eat Icecream", done: false } <$> UUID.genUUID
  pure [ item1, item2, item3 ]

useTodos :: forall ctx m. MonadEffect m => Eq ctx => UseHelixHook State Action ctx m
useTodos = makeStore "todos" reducer initialState
  where
  reducer st act = case act of
    AddTodoItem id title -> snoc st { id, title, done: false }
    MarkDone id -> fromMaybe st
      $ (\idx -> modifyAt idx (_ { done = true }) st)
          <=< findIndex ((_ == id) <<< _.id)
      $ st