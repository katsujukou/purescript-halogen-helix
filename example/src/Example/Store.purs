module Example.Store where

import Prelude

import Data.Array (findIndex, modifyAt, snoc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.UUID as UUID
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Example.Types (TodoItem)
import Halogen.Helix (HelixMiddleware, UseHelixHook, makeStoreMiddleware, (<|))

type State = Array TodoItem

data Action
  = CreateTodoItem UUID.UUID String
  | AddTodo String
  | MarkDone UUID.UUID

derive instance Generic Action _
instance Show Action where
  show = genericShow

middlewares :: forall m. MonadEffect m => HelixMiddleware State Action m
middlewares = actionLogger <| stateLogger <| idProvider
  where
  stateLogger ctx action next = do
    before <- ctx.getState
    Console.log $ "Before: " <> show before
    next action
    after <- ctx.getState
    Console.log $ "After: " <> show after

  actionLogger _ action next = do
    Console.log $ "Dispatched: " <> show action
    next action

  idProvider ctx action next = case action of
    AddTodo item -> do
      id <- liftEffect UUID.genUUID
      ctx.dispatch $ CreateTodoItem id item
    _ -> next action

initialState :: State
initialState = unsafePerformEffect do
  item1 <- { id: _, title: "Develop Halogen App", done: false } <$> UUID.genUUID
  item2 <- { id: _, title: "Study Category Theory", done: false } <$> UUID.genUUID
  item3 <- { id: _, title: "Eat Icecream", done: false } <$> UUID.genUUID
  pure [ item1, item2, item3 ]

useTodos :: forall ctx m. MonadEffect m => Eq ctx => UseHelixHook State Action ctx m
useTodos = makeStoreMiddleware "todos" reducer initialState middlewares
  where
  reducer st act = case act of
    CreateTodoItem id title -> snoc st { id, title, done: false }
    MarkDone id -> fromMaybe st
      $ (\idx -> modifyAt idx (_ { done = true }) st)
          <=< findIndex ((_ == id) <<< _.id)
      $ st
    _ -> st