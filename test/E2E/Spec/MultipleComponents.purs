module Test.E2E.Spec.MultipleComponents where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (fold)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Halogen (HalogenIO, mkTell)
import Test.E2E.Environment (logShouldBe)
import Test.E2E.Environment as Environment
import Test.E2E.Environment.MultipleComponents (Query(..), _button, _counter, _switch, _value, _whole, app, counterLogMessage, switchLogMessage, wholeLogMessage)
import Test.E2E.Logger (VirtualConsole, resetLog)
import Test.Spec (Spec, before, describe, it)
import Test.Utils.HTML (unsafeQuerySelector, unsafeToHTMLElement, (.>))
import Test.Utils.HTML as HTML
import Test.Utils.HappyDOM (waitUntilComplete)
import Web.DOM (Element)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (Window, document)

type TestEnv =
  { window :: Window
  , console :: VirtualConsole
  , resetStore :: Aff Unit
  , io :: HalogenIO Query {} Aff
  , elements ::
      { counterValue :: Element
      , incrementBtn :: Element
      , switchValue :: Element
      , toggleBtn :: Element
      , wholeValue :: Element
      }
  }

setup :: Aff TestEnv
setup = do
  { window, console, io } <- Environment.setup app
  pn <- liftEffect $ HTMLDocument.toParentNode <$> document window
  elements <- { counterValue: _, incrementBtn: _, switchValue: _, toggleBtn: _, wholeValue: _ }
    <$> unsafeQuerySelector (_counter .> _value) pn
    <*> unsafeQuerySelector (_counter .> _button) pn
    <*> unsafeQuerySelector (_switch .> _value) pn
    <*> unsafeQuerySelector (_switch .> _button) pn
    <*> unsafeQuerySelector (_whole .> _value) pn

  let
    resetStore = void $ io.query (mkTell ResetStore)

  resetLog console

  pure { window, console, elements, resetStore, io }

-- fire click and await all asynchronous effects complete.
clickAwait :: forall m. MonadThrow Error m => MonadAff m => Window -> Element -> m Unit
clickAwait window el = do
  liftEffect <<< HTMLElement.click =<< unsafeToHTMLElement el
  waitUntilComplete window

textContentShouldBe :: forall m a. MonadEffect m => MonadThrow Error m => Show a => Element -> a -> m Unit
textContentShouldBe el = show >>> (el `HTML.textContentShouldBe` _)

spec :: Spec Unit
spec = before setup do
  describe "Multiple components app" do
    it "should share same state accross multiple components" \{ window, elements, resetStore, io } -> do
      let click = clickAwait window

      resetStore

      click elements.incrementBtn
      elements.counterValue `textContentShouldBe` 1
      elements.wholeValue `textContentShouldBe` { count: 1, switch: false }

      click elements.toggleBtn
      elements.switchValue `HTML.textContentShouldBe` "ON"
      elements.wholeValue `textContentShouldBe` { count: 1, switch: true }

      io.dispose :: Aff Unit

    it "should rerender only when selected part of the store is updated" 
      \{ console, window, elements, io, resetStore } -> do
        let click = clickAwait window
        resetStore
        click elements.incrementBtn
        -- clicking Increment button should not cause rerendering of toggle switch component
        console `logShouldBe` fold
          [ logStreamAfterClickingIncrement
          ]

        click elements.toggleBtn
        -- clicking Toggle button should not cause rerendering of counter component
        console `logShouldBe` fold
          [ logStreamAfterClickingIncrement
          , logStreamAfterClickingToggle
          ]

        io.dispose :: Aff Unit

  where
  logStreamAfterClickingIncrement = [ wholeLogMessage, counterLogMessage ]
  logStreamAfterClickingToggle = [ wholeLogMessage, switchLogMessage ]