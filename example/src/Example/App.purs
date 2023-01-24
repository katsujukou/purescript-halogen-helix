module Example.App where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Example.Store (Action(..), useTodos)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

app :: forall q i o m. MonadEffect m => H.Component q i o m
app = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.div []
      [ HH.div []
          [ HH.h1 []
              [ HH.text "Helix Todo App" ]
          ]
      , HH.div []
          [ HH.slot_ (Proxy :: _ "addTodoItemForm") unit addTodoItemForm {}
          ]
      , HH.div []
          [ HH.slot_ (Proxy :: _ "todoItemList") unit todoItemList {}
          ]
      ]

todoItemList :: forall q i o m. MonadEffect m => H.Component q i o m
todoItemList = Hooks.component \_ _ -> Hooks.do
  todos /\ ctx <- useTodos identity

  let
    classString done = "todo-item " <> case done of
      true -> "done "
      false -> ""

  Hooks.pure do
    HH.div [] $
      todos <#> \todo -> do
        HH.div
          [ HP.class_ $ ClassName $ classString todo.done
          , HE.onClick \_ -> ctx.dispatch (MarkDone todo.id)
          ]
          [ HH.text todo.title ]

addTodoItemForm :: forall q i o m. MonadEffect m => H.Component q i o m
addTodoItemForm = Hooks.component \_ _ -> Hooks.do
  text /\ textId <- useState ""
  _ /\ ctx <- useTodos identity

  let
    handleClick = do
      title <- Hooks.get textId
      ctx.dispatch $ AddTodo title
      Hooks.put textId ""

  Hooks.pure do
    HH.div [ HP.class_ $ ClassName "add-todo-form" ]
      [ HH.input
          [ HP.type_ InputText
          , HP.value text
          , HP.class_ $ ClassName "add-todo-form-input"
          , HP.placeholder "Enter new todo item..."
          , HE.onValueInput $ Hooks.put textId
          ]
      , HH.button
          [ HP.type_ ButtonButton
          , HP.class_ $ ClassName "add-todo-form-button"
          , HP.disabled (text == "")
          , HE.onClick \_ -> handleClick
          ]
          [ HH.text "Add"
          ]
      ]
