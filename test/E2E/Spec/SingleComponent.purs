module Test.E2E.Spec.SingleComponent where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Const (Const)
import Effect.Aff (Aff, Error)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenIO)
import Test.E2E.Environment as Environment
import Test.E2E.Environment.SingleComponent (_component, _incrementButton, _toggleButton, _state, app)
import Test.E2E.Logger (VirtualConsole)
import Test.Spec (Spec, before, describe, it)
import Test.Utils.HTML (unsafeQuerySelector, unsafeToHTMLElement, (.>))
import Test.Utils.HTML as HTML
import Web.DOM.Element (Element)
import Web.HTML (Window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

type TestEnv =
  { window :: Window
  , console :: VirtualConsole
  , io :: HalogenIO (Const Unit) Void Aff
  , elements ::
      { stateEl :: Element
      , incrementBtnEl :: Element
      , toggleBtnEl :: Element
      }
  }

setup :: Aff TestEnv
setup = do
  { window, console, io } <- Environment.setup app
  pn <- liftEffect $ HTMLDocument.toParentNode <$> document window
  stateEl <- unsafeQuerySelector (_component .> _state) pn
  incrementBtnEl <- unsafeQuerySelector (_component .> _incrementButton) pn
  toggleBtnEl <- unsafeQuerySelector (_component .> _toggleButton) pn
  pure
    { window
    , console
    , io
    , elements: { stateEl, incrementBtnEl, toggleBtnEl }
    }

textContentShouldBe :: forall m a. MonadEffect m => MonadThrow Error m => Show a => Element -> a -> m Unit
textContentShouldBe el = show >>> (el `HTML.textContentShouldBe` _)

spec :: Spec Unit
spec = before setup do
  describe "Single component app" do
    it "should render initial state correctly" \{ elements: { stateEl } } -> do
      stateEl `textContentShouldBe` { count: 0, switch: false }

    it "should rerender in response to store update" \{ io, elements: { stateEl, incrementBtnEl, toggleBtnEl } } -> do
      let
        click = unsafeToHTMLElement >=> HTMLElement.click >>> liftEffect

      click incrementBtnEl
      stateEl `textContentShouldBe` { count: 1, switch: false }

      click toggleBtnEl
      stateEl `textContentShouldBe` { count: 1, switch: true }

      click incrementBtnEl
      stateEl `textContentShouldBe` { count: 2, switch: true }

      io.dispose :: Aff Unit